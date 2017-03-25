-------------------------------------------------------------------------------
--
-- CPU widget for Awesome 3.4
-- Copyright (C) 2011 Tuomas Jormola <tj@solitudo.net>
--
-- Licensed under the terms of GNU General Public License Version 2.0.
--
-- Description:
--
-- Displays horizontal usage trend graph of all the CPUs combined.
-- Also displays a CPU icon next to the graph. Clicking the icon
-- launches an external application (if configured).
--
-- Widget extends vicious.widgets.cpu from Vicious widget framework.
--
-- Widget tries to use an icon from the package sensors-applet
-- if available.
--
--
-- Configuration:
--
-- The load() function can be supplied with configuration.
-- Format of the configuration is as follows.
-- {
-- -- Width of the graph in pixels. Default is 20.
--	      graph_width     = 50,
-- -- Command to execute when left-clicking the widget icon.
-- -- Empty by default.
--	      command         = 'gnome-system-monitor',
-- -- Don't try to display any icons. Default is false (i.e. display icons).
--        no_icon         = true,
-- -- How often update the widget data. Default is 1 second.
--	      update_interval = 2
-- }
--
--
-- Theme:
--
-- The widget uses following colors and icons if available in
-- the Awesome theme.
--
-- theme.bg_widget        - widget background color
-- theme.fg_widget        - widget foreground color
-- theme.fg_center_widget - widget gradient color, middle
-- theme.fg_end_widget    - widget gradient color, end
-- theme.delightful_cpu   - icon shown next to the CPU graph
---
-------------------------------------------------------------------------------

local awful_button      = require('awful.button')
local awful_tooltip     = require('awful.tooltip')
local awful_util        = require('awful.util')
local awful_widget      = require('awful.widget')
local image             = require('image')
local widget            = require('widget')

local delightful_utils  = require('delightful.utils')
local vicious           = require('vicious')

local pairs             = pairs
local string            = { format = string.format }

module('delightful.widgets.cpu')

local cpu_config
local fatal_error
local icon_tooltip

local config_description = {
	{
		name     = 'graph_width',
		required = true,
		default  = 20,
		validate = function(value) return delightful_utils.config_int(value) end
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
		default  = 1,
		validate = function(value) return delightful_utils.config_int(value) end
	},
}

local icon_description = {
	cpu = { beautiful_name = 'delightful_cpu', default_icon = function() return 'sensors-applet-cpu' end },
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
		cpu_config = empty_config
		return
	end
	cpu_config = config_data
end

-- Initalization
function load(self, config)
	handle_config(config)
	if fatal_error then
		delightful_utils.print_error('cpu', fatal_error)
		return nil, nil
	end
	local icon
	local icon_files
	if not cpu_config.no_icon then
		icon_files = delightful_utils.find_icon_files(icon_description)
	end
	local icon_file = icon_files and icon_files.cpu
	if icon_file then
		local icon_data = image(icon_file);
		if icon_data then
			local buttons = awful_button({}, 1, function()
					if not fatal_error and cpu_config.command then
						awful_util.spawn(cpu_config.command, true)
					end
			end)
			icon = widget({ type = 'imagebox', name = 'cpu' })
			icon:buttons(buttons)
			icon.image = icon_data
			icon_tooltip = awful_tooltip({ objects = { icon } })
		end
	end

	local bg_color        = delightful_utils.find_theme_color({ 'bg_widget', 'bg_normal'                     })
	local fg_color        = delightful_utils.find_theme_color({ 'fg_widget', 'fg_normal'                     })
	local fg_center_color = delightful_utils.find_theme_color({ 'fg_center_widget', 'fg_widget', 'fg_normal' })
	local fg_end_color    = delightful_utils.find_theme_color({ 'fg_end_widget', 'fg_widget', 'fg_normal'    })

	local cpu_widget = awful_widget.graph({ layout = awful_widget.layout.horizontal.rightleft })
	if bg_color then
		cpu_widget:set_background_color(bg_color)
		cpu_widget:set_border_color(bg_color)
	end
	if fg_color then
		cpu_widget:set_color(fg_color)
	end
	if fg_color and fg_center_color and fg_end_color then
		cpu_widget:set_gradient_colors({ fg_color, fg_center_color, fg_end_color })
	end
	cpu_widget:set_width(cpu_config.graph_width)
	cpu_widget:set_height(19)
	cpu_widget:set_gradient_angle(180)
	vicious.register(cpu_widget, vicious.widgets.cpu, vicious_formatter, cpu_config.update_interval)

	return { cpu_widget }, { icon }
end

-- Vicious display formatter, also update widget tooltip
function vicious_formatter(widget, data)
	if icon_tooltip then
		local tooltip_text = string.format(' CPU usage trend graph of all the CPUs in the system \n Current CPU usage: %d%% ', data[1])
		icon_tooltip:set_text(tooltip_text)
	end
	return data[1]
end
