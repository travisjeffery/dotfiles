-------------------------------------------------------------------------------
--
-- Net widget for Awesome 3.4
-- Copyright (C) 2011 Tuomas Jormola <tj@solitudo.net>
--
-- Licensed under the terms of GNU General Public License Version 2.0.
--
-- Description:
--
-- Displays current download and upload speed of selected network device
-- of the system. Some interfaces can be excluded from the monitoring
-- if desired. Also displays an icon next to the download and upload
-- speed display that indicates the type of the network device
-- (wired, wireless, or dialup). Clicking the icon launches
-- an external application (if configured).
--
-- Widget extends vicious.widgets.net from Vicious widget framework.
--
-- Widget tries to use icons from the package gnome-icon-theme
-- if available.
--
--
-- Configuration:
--
-- The load() function can be supplied with configuration.
-- Format of the configuration is as follows.
-- {
-- -- A list of Lua patterns to match each network device on the system.
-- -- If a match is found, no download and upload display for the device
-- -- is shown. Use this setting to exclude some local devices that
-- -- you don't want to monitor and is not ingored by default by
-- -- the next configure option. Empty list by default.
--	      excluded_devices         = { '^somedev$' },
-- -- Most you likely don't want to change this option. It defines which
-- -- network devices to exclude from monitoring by default. It uses
-- -- the same format as previous option (a list of Lua patterns).
-- -- Default is
-- -- { '^lo$', '^tun%d*$', '^tap%d*$', '^pan%d+$', '^br%d+$', '^vm%d+$' }
--        excluded_devices_default = { '^moredev%d+$' },
-- -- A list of Lua patterns that define which network devices are considered
-- -- wireless devices and thus the widget displays the wireless device icon.
-- -- Most you likely don't want to change this option. Default is
-- -- { '^wlan%d*', '^wifi%d*', '^ath%d+$', '^ra%d+$' }
--        wireless_devices         = { '^mywlandev%d+$' },
-- -- A list of Lua patterns that define which network devices are considered
-- -- dialup devices and thus the widget displays the wireless device icon.
-- -- Most you likely don't want to change this option. Default is
-- -- { '^ppp%d+' }
--        dialup_devices           = { '^mydialupdev%d+$' },
-- -- Command to execute when left-clicking the widget icon.
-- -- Empty by default.
--	      command                  = 'gnome-nettool',
-- -- Don't try to display any icons. Default is false (i.e. display icons).
--        no_icon                  = true,
-- -- How often update the widget data. Default is 3 seconds.
--	      update_interval          = 10
-- }
--
--
-- Theme:
--
-- The widget uses following colors and icons if available in
-- the Awesome theme.
--
-- theme.fg_widget                   - color of upload speed text
-- theme.fg_end_widget               - color of download speed text
-- theme.delightful_network_wired    - icon shown next to wired network interfaces
--                                     (eth0, etc.)
-- theme.delightful_network_wireless - icon shown next to wireless network interfaces
--                                     (wlan0, ra0, etc.)
-- theme.delightful_network_dialup   - icon shown next to dialup network interfaces
--                                     (ppp0, etc.)
--
-------------------------------------------------------------------------------

local awful_button      = require('awful.button')
local awful_tooltip     = require('awful.tooltip')
local awful_util        = require('awful.util')
local image             = require('image')
local widget            = require('widget')

local delightful_utils  = require('delightful.utils')
local vicious           = require('vicious')

local io                = io
local pairs             = pairs
local string            = { format = string.format }
local table             = { insert = table.insert }

module('delightful.widgets.network')

local network_config
local fatal_error
local icon_files = {}

local config_description = {
	{
		name     = 'excluded_devices',
		require  = true,
		default  = {},
		coerce   = function(value) return delightful_utils.coerce_table(value) end,
		validate = function(value) return delightful_utils.config_table(value) end
	},
	{
		name     = 'excluded_devices_default',
		required = true,
		default  = { '^lo$', 'dummy%d*$', '^tun%d*$', '^tap%d*$', '^pan%d+$', '^br%d+$', '^vm%d+$' },
		coerce   = function(value) return delightful_utils.coerce_table(value) end,
		validate = function(value) return delightful_utils.config_table(value) end
	},
	{
		name     = 'wireless_devices',
		required = true,
		default  = { '^wlan%d*', '^wifi%d*', '^ath%d+$', '^ra%d+$' },
		coerce   = function(value) return delightful_utils.coerce_table(value) end,
		validate = function(value) return delightful_utils.config_table(value) end
	},
	{
		name     = 'dialup_devices',
		required = true,
		default  = { '^ppp%d+' },
		coerce   = function(value) return delightful_utils.coerce_table(value) end,
		validate = function(value) return delightful_utils.config_table(value) end
	},
	{
		name     = 'command',
		validate = function(value) return delightful_utils.config_string(value) end
	},
	{
		name     = 'no_icon',
		validate = function(value) return delightful_utils.config_boolean(value) end
	},
	{
		name     = 'update_interval',
		required = true,
		default  = 3,
		validate = function(value) return delightful_utils.config_int(value) end
	},
}

local icon_description = {
	wired    = { beautiful_name = 'delightful_network_wired',    default_icon = function() return 'network-wired' end    },
	wireless = { beautiful_name = 'delightful_network_wireless', default_icon = function() return 'network-wireless' end },
	dialup   = { beautiful_name = 'delightful_network_dialup',   default_icon = function() return 'modem' end            },
}

-- Configuration handler
function handle_config(user_config)
	local empty_config = delightful_utils.get_empty_config(config_description)
	if not user_config then
		user_config = empty_config
	end
	local config_data = delightful_utils.normalize_config(user_config, config_description)
	local validation_errors = delightful_utils.validate_config(config_data, config_description)
	if validation_errors then
		fatal_error = 'Configuration errors: \n'
		for error_index, error_entry in pairs(validation_errors) do
			fatal_error = string.format('%s %s', fatal_error, error_entry)
			if error_index < #validation_errors then
				fatal_error = string.format('%s \n', fatal_error)
			end
		end
		network_config = empty_config
		return
	end
	network_config = config_data
end

-- Initalization
function load(self, config)
	handle_config(config)
	if fatal_error then
		delightful_utils.print_error('net', fatal_error)
		return nil, nil
	end
	if not network_config.no_icon then
		icon_files = delightful_utils.find_icon_files(icon_description)
	end
	local devices = {}
	for line in io.lines('/proc/net/dev') do
		local device = line:match('^[%s]?[%s]?[%s]?[%s]?([%w]+):')
		if device then
			local exclude = false
			for _, excluded_device in pairs(awful_util.table.join(network_config.excluded_devices, network_config.excluded_devices_default)) do
				if device:find(excluded_device) then
					exclude = true
					break
				end
			end
			if not exclude then
				table.insert(devices, device)
			end
		end
	end

	local color_download = delightful_utils.find_theme_color({ 'fg_end_widget' })
	local color_upload   = delightful_utils.find_theme_color({ 'fg_widget'     })

	local widgets
	local icons
	for _, device in pairs(devices) do
		local icon
		local icon_file
		for _, wireless_device in pairs(network_config.wireless_devices) do
			if device:find(wireless_device) then
				icon_file = icon_files.wireless
			end
		end
		if not icon_file then
			for _, dialup_device in pairs(network_config.dialup_devices) do
				if device:find(dialup_device) then
					icon_file = icon_files.dialup
				end
			end
		end
		if not icon_file then
			icon_file = icon_files.wired
		end

    	if icon_file then
			local icon_data = image(icon_file)
			if icon_data then
				local buttons = awful_button({}, 1, function()
						if not fatal_error and network_config.command then
							awful_util.spawn(network_config.command, true)
						end
				end)
				icon = widget({ type = 'imagebox', name = 'net' .. device .. 'icon' })
				icon:buttons(buttons)
		    	icon.image = icon_data
				local tooltip = awful_tooltip({ objects = { icon } })
    			tooltip:set_text(' Download and upload speed \n of the network device ' .. device .. ' \n in kilobytes per second ')
				if not icons then
					icons = {}
				end
				table.insert(icons, icon)
			end
		end

	    local net_widget = widget({ type = 'textbox', name = 'net' .. device .. 'widget' })
    	local widget_text = '↓'
		local close_span = false
		if color_download then
			widget_text = string.format('%s<span color="%s">', widget_text, color_download)
			close_span = true
		end
		widget_text = string.format('%s${%s down_kb}', widget_text, device)
		if close_span then
			widget_text = string.format('%s%s', widget_text, '</span>')
			close_span = false
		end
		widget_text = string.format('%s%s', widget_text, ' ↑')
		if color_upload then
			widget_text = string.format('%s<span color="%s">', widget_text, color_upload)
			close_span = true
		end
		widget_text = string.format('%s${%s up_kb}', widget_text, device)
		if close_span then
			widget_text = string.format('%s%s', widget_text, '</span>')
			close_span = false
		end
    	vicious.register(net_widget, vicious.widgets.net, widget_text, network_config.update_interval)
		if not widgets then
			widgets = {}
		end
		table.insert(widgets, net_widget)
	end

	vicious.cache(vicious.widgets.net)

	return widgets, icons
end
