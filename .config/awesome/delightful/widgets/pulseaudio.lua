-------------------------------------------------------------------------------
--
-- PulseAudio mixer widget for Awesome 3.4
-- Copyright (C) 2011 Tuomas Jormola <tj@solitudo.net>
--
-- Licensed under the terms of GNU General Public License Version 2.0.
--
-- Description:
--
-- Shows a mixer display and control for PulseAudio sinks.
-- Volume level is indicated as a vertical progress bar and
-- an icon is shown next to it. Clicking the icon performs
-- some mixer-related actions. Left-click will mute the sink,
-- right-click launcheѕ external mixer application (if configured),
-- and using the scroll wheel will pump up and down the volume.
--
-- Widget uses Vicious widget framework to gather widget data.
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
-- -- PulseAudio sink IDs as listed in the "index:" line of output of
-- -- "pacmd list-sinks" command. This can be used to limit mixer controls
-- -- to certain sinks only. By default, widget creates controls for
-- -- all the sinks in the system. The example shows 1st and 3rd sink.
--    	  sink_nums          = { 0, 2 },
-- -- Whether to try to start PulseAudio if reading of the sink data
-- -- fails. Default is true.
--	      pulseaudio_start   = true,
-- -- Path to pulseaudio command. 'pulseaudio' by default.
--	      pulseaudio_command = '/usr/local/bin/pulseaudio',
-- -- Path to pacmd command. 'pacmd' by default.
--	      pacmd_command      = '/usr/local/bin/pacmd',
-- -- Command to execute when right-clicking the widget icon.
-- -- Empty by default.
--	      mixer_command      = 'gnome-volume-control',
-- -- Don't try to display any icons. Default is false (i.e. display icons).
--        no_icon            = true,
-- -- How often update the widget data. Default is 10 seconds.
--	      update_interval    = 30
-- }
--
--
-- Theme:
--
-- The widget uses following colors and icons if available in
-- the Awesome theme.
--
-- theme.bg_widget           - widget background color
-- theme.fg_widget           - widget foreground color
-- theme.fg_center_widget    - widget gradient color, middle
-- theme.fg_end_widget       - widget gradient color, end
-- theme.delightful_vol      - default icon, this is also used as
--                             the max, med, min, and zero icons if these
--                             are not specified
-- theme.delightful_vol_max  - icon shown when volume level is high
-- theme.delightful_vol_med  - icon shown when volume level is medium
-- theme.delightful_vol_min  - icon shown when volume level is low
-- theme.delightful_vol_zero - icon shown when volume level is at the bottom
-- theme.delightful_vol_mute - icon shown when sink is muted
-- theme.delightful_error    - icon shown when critical error has occurred
--
-------------------------------------------------------------------------------

local awful_button       = require('awful.button')
local awful_tooltip      = require('awful.tooltip')
local awful_util         = require('awful.util')
local awful_widget       = require('awful.widget')
local image              = require('image')
local widget             = require('widget')

local delightful_utils   = require('delightful.utils')
local vicious            = require('vicious')

local io                 = { popen = io.popen }
local math               = { floor = math.floor }
local os                 = { execute = os.execute, time = os.time }
local pairs              = pairs
local setmetatable       = setmetatable
local string             = { format = string.format }
local table              = { insert = table.insert, remove = table.remove }
local tonumber           = tonumber
local type               = type
local print=print
module('delightful.widgets.pulseaudio')

local maxvol             = 65536
local volstep            = 5

local widgets            = {}
local icons              = {}
local icon_files         = {}
local prev_icons         = {}
local tooltips           = {}
local sink_data          = {}
local new_data           = {}
local number_of_sinks
local fatal_error
local retry_fatal_error  = true
local pulseaudio_config
local pacmd_string
local pacmd_timestamp
local pacmd_force_update = false

local config_description = {
	{
		name     = 'sink_nums',
		coerce   = function(value) return delightful_utils.coerce_table(value) end,
		validate = function(value) return delightful_utils.config_table(value) end
	},
	{
		name     = 'pulseaudio_start',
		required = true,
		default  = true,
		validate = function(value) return delightful_utils.config_boolean(value) end
	},
	{
		name     = 'pulseaudio_command',
		required = true,
		default  = 'pulseaudio',
		validate = function(value) return delightful_utils.config_string(value) end
	},
	{
		name     = 'pacmd_command',
		required = true,
		default  = 'pacmd',
		validate = function(value) return delightful_utils.config_string(value) end
	},
	{
		name     = 'mixer_command',
		default  = function(config_data) if mixer_cmd then return mixer_cmd end end,
		validate = function(value) return delightful_utils.config_string(value) end
	},
	{
		name     = 'no_icon',
		validate = function(value) return delightful_utils.config_boolean(value) end
	},
	{
		name     = 'update_interval',
		required = true,
		default  = 10,
		validate = function(value) return delightful_utils.config_int(value) end
	},
}

local icon_description = {
	vol   = { beautiful_name = 'delightful_vol',      default_icon = function() return 'stock_volume' end      },
	max   = { beautiful_name = 'delightful_vol_max',  default_icon = function() return 'stock_volume-max' end  },
	med   = { beautiful_name = 'delightful_vol_med',  default_icon = function() return 'stock_volume-med' end  },
	min   = { beautiful_name = 'delightful_vol_min',  default_icon = function() return 'stock_volume-min' end  },
	zero  = { beautiful_name = 'delightful_vol_zero', default_icon = function() return 'stock_volume-0' end    },
	mute  = { beautiful_name = 'delightful_vol_mute', default_icon = function() return 'stock_volume-mute' end },
	error = { beautiful_name = 'delightful_error',    default_icon = function() return 'error' end             },
}

-- Read sink info
function update_data(force_update)
	update_sink_string(force_update)
	sink_data = {}
	for i = 1, number_of_sinks do
		sink_data[i] = {}
	end
	if not pacmd_string or fatal_error then
		return
	end
	local sink_id = 0
	-- iterate all lines in "pacmd list-sinks" output
	pacmd_string:gsub('(.-)\n', function(line)
			-- parse sink id
			line:gsub('^[%s\*]+index:%s(%d)$', function(match)
					local sink_num_ok = false
					local sink_num = tonumber(match)
					if pulseaudio_config.sink_nums then
						for _, accepted_sink_num in pairs(pulseaudio_config.sink_nums) do
							sink_num_ok = sink_num == accepted_sink_num
							if sink_num_ok then
								break
							end
						end
					else
						sink_num_ok = true
					end
					if sink_num_ok then
						sink_id = sink_id + 1
						sink_data[sink_id].num = sink_num
					end
			end)
			-- parse mute status
			line:gsub('^%s+muted:%s(.+)$', function(match)
					if sink_id == 0 or not sink_data[sink_id].num then
						return
					end
					if match == 'yes' then
						sink_data[sink_id].muted = true
					else
						sink_data[sink_id].muted = false
					end
			end)
			-- parse volume
			line:gsub('^%s+volume:%s+0:%s+(%d+)%%%s+1:%s+%d+%%$', function(match)
					if sink_id == 0 or not sink_data[sink_id].num then
						return
					end
					sink_data[sink_id].volperc = tonumber(match)
					sink_data[sink_id].volnum  = math.floor(((maxvol / 100) * sink_data[sink_id].volperc) + 0.5)
			end)
			-- parse device name
			line:gsub('^%s+alsa\.name%s+=%s+[\'"]([^\'"]+)[\'"]$', function(match)
					if sink_id == 0 or not sink_data[sink_id].num then
						return
					end
					sink_data[sink_id].name = match
			end)
	end)
	-- ensure all required info was found
	for found_sink_id, found_sink_data in pairs(sink_data) do
		if not (found_sink_data.name and found_sink_data.muted ~= nil and found_sink_data.volperc) then
			sink_data[found_sink_id] = {
					error_string = 'Failed to get required info about PulseAudio sink'
			}
		end
	end
end

-- Update widget icon based on the volume
function update_icon(sink_id)
	if not icons[sink_id] or not icon_files.error then
		return
	end
	local icon_file
	if (fatal_error or (sink_data[sink_id] and sink_data[sink_id].error_string)) and icon_files.error then
		icon_file = icon_files.error
	elseif sink_data[sink_id] and icon_files.vol then
		icon_file = icon_files.vol
		if sink_data[sink_id].muted then
			icon_file = icon_files.mute
		elseif sink_data[sink_id].volperc then
			if     sink_data[sink_id].volperc > 100 * 0.7 then
				icon_file = icon_files.max
			elseif sink_data[sink_id].volperc > 100 * 0.3 then
				icon_file = icon_files.med
			elseif sink_data[sink_id].volperc > 0 then
				icon_file = icon_files.min
			elseif sink_data[sink_id].volperc == 0 then
				icon_file = icon_files.zero
			end
		end
	end
	if icon_file and (not prev_icons[sink_id] or prev_icons[sink_id] ~= icon_file) then
		prev_icons[sink_id]  = icon_file
		icons[sink_id].image = image(icon_file)
	end
end

-- Update the mixer tooltip text
function update_tooltip(sink_id)
	if not tooltips[sink_id] or not sink_data[sink_id]then
		return
	end
	local text
	if fatal_error then
		text = string.format(' %s ', fatal_error)
	elseif sink_data[sink_id].error_string then
		text = string.format(' %s ', sink_data[sink_id].error_string)
	else
		local volume_text = 'Unknown'
		if sink_data[sink_id].muted then
			volume_text = 'muted'
		elseif sink_data[sink_id].volperc then
			volume_text = string.format('%d%%', sink_data[sink_id].volperc)
		end
		text = string.format(' Audio device %s \n Volume level: %s \n Volume controls: \n Left mouse button: toggle mute on and off \n Right mouse button: launch mixer \n Scrollwheel up and down: rise and lower volume ', sink_data[sink_id].name, volume_text)

	end
	tooltips[sink_id]:set_text(text)
end

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
		retry_fatal_error = false
		pulseaudio_config = empty_config
		return
	end
	pulseaudio_config = config_data
end

-- Initalization
function load(self, config)
	handle_config(config)
	if not pulseaudio_config.no_icon then
		icon_files = delightful_utils.find_icon_files(icon_description)
	end
	update_sink_string()
	update_number_of_sinks()
	if not pulseaudio_config.sink_nums then
		pulseaudio_config.sink_nums = {}
		for sink_id = 1, number_of_sinks do
			table.insert(pulseaudio_config.sink_nums, sink_id - 1)
		end
	end

	local bg_color        = delightful_utils.find_theme_color({ 'bg_widget', 'bg_normal'                     })
	local fg_color        = delightful_utils.find_theme_color({ 'fg_widget', 'fg_normal'                     })
	local fg_center_color = delightful_utils.find_theme_color({ 'fg_center_widget', 'fg_widget', 'fg_normal' })
	local fg_end_color    = delightful_utils.find_theme_color({ 'fg_end_widget', 'fg_widget', 'fg_normal'    })

	for sink_id = 1, number_of_sinks do
		if icon_files.vol and icon_files.error then
			local buttons = awful_util.table.join(
					awful_button({}, 1, function()
							if sink_data[sink_id] and not fatal_error and not sink_data[sink_id].error_string then
								pulseaudio_control('toggle', sink_id)
							end
					end),
					awful_button({}, 3, function()
							if sink_data[sink_id] and not fatal_error and not sink_data[sink_id].error_string then
								if pulseaudio_config.mixer_command then
									awful_util.spawn(pulseaudio_config.mixer_command, true)
								end
							end
					end),
					awful_button({}, 4, function()
							if sink_data[sink_id] and not fatal_error and not sink_data[sink_id].error_string then
								pulseaudio_control('up', sink_id)
							end
					end),
					awful_button({}, 5, function()
							if sink_data[sink_id] and not fatal_error and not sink_data[sink_id].error_string then
								pulseaudio_control('down', sink_id)
							end
					end)
			)
			icons[sink_id] = widget({ type = 'imagebox', name = 'pulseaudio_' .. sink_id})
			icons[sink_id]:buttons(buttons)
			tooltips[sink_id] = awful_tooltip( { objects = { icons[sink_id] } })
			update_icon(sink_id)
			update_tooltip(sink_id)
		end

		local widget = awful_widget.progressbar({ layout = awful_widget.layout.horizontal.rightleft })
		if bg_color then
			widget:set_border_color(bg_color)
			widget:set_background_color(bg_color)
		end
		if fg_color then
			widget:set_color(fg_color)
		end
		if fg_color and fg_center_color and fg_end_color then
			widget:set_gradient_colors({ fg_color, fg_center_color, fg_end_color })
		end
		widget:set_width(8)
		widget:set_height(19)
		widget:set_vertical(true)
		widgets[sink_id] = widget
		vicious.register(widget, self, '$1', pulseaudio_config.update_interval, sink_id)
	end
	return widgets, icons
end

-- Vicious worker function
function vicious_worker(format, sink_id)
	update_data(pacmd_force_update)
	update_icon(sink_id)
	update_tooltip(sink_id)
	pacmd_force_update = false
	if fatal_error then
		delightful_utils.print_error('pulseaudio', fatal_error)
		return 0
	end
	if not sink_data[sink_id] then
		return 0
	end
	if sink_data[sink_id].error_string then
		delightful_utils.print_error('pulseaudio', sink_data[sink_id].error_string)
		return 0
	end
	return sink_data[sink_id].volperc
end

-- Sink helpers

function update_sink_string(force_update)
	if not retry_fatal_error then
		return
	end
	local now = os.time()
	local pacmd_command = pulseaudio_config.pacmd_command .. ' list-sinks'
	if force_update or not pacmd_string or (pacmd_timestamp and now - pacmd_timestamp >= pulseaudio_config.update_interval) then
		pacmd_string = awful_util.pread(pacmd_command)
		pacmd_timestamp = now
	end
	if not pacmd_string or #pacmd_string == 0 then
		pacmd_string = nil
		-- try starting PulseAudio
		if pulseaudio_config.pulseaudio_start then
			awful_util.spawn(pulseaudio_config.pulseaudio_command, false)
			os.execute('sleep 1')
			pacmd_string = awful_util.pread(pacmd_command)
			if not pacmd_string or #pacmd_string == 0 then
				pacmd_string = nil
				fatal_error = 'Tried to start PulseAudio, but failed list PulseAudio sinks. Is PulseAudio installed and properly configured?'
				return
			end
		else
			fatal_error = 'Failed to list PulseAudio sinks. Is PulseAudio installed and running?'
			return
		end
	end
end

function update_number_of_sinks()
	if number_of_sinks then
		return
	end
	number_of_sinks = 0
	if pulseaudio_config.sink_nums then
		number_of_sinks = #pulseaudio_config.sink_nums
	elseif pacmd_string then
		pacmd_string:gsub('(.-)\n', function(line)
				line:gsub('^[%s\*]+index:%s%d$', function(match)
						number_of_sinks = number_of_sinks + 1
				end)
		end)
	end
	if number_of_sinks == 0 then
		number_of_sinks = 1
		local error_string = 'Failed to detect PulseAudio sinks'
		if fatal_error then
			error_string = string.format('%s: %s', error_string, fatal_error)
		end
		fatal_error = error_string
	end
end

-- PulseAudio volume control functions

function pulseaudio_control(command, sink_id)
	if sink_data[sink_id] and not fatal_error and not sink_data[sink_id].error_string then
		if command == 'toggle' then
			pulseaudio_toggle(sink_id)
		elseif command == 'up' then
			pulseaudio_set_volume(sink_id, volstep)
		elseif command == 'down' then
			pulseaudio_set_volume(sink_id, -volstep)
		end
	end
	pacmd_force_update = true
	vicious.force({ widgets[sink_id] })
	update_icon(sink_id)
end

function pulseaudio_set_volume(sink_id, step)
	if not sink_data[sink_id] or fatal_error or sink_data[sink_id].error_string or not sink_data[sink_id].volperc then
		return
	end
	local volperc_new = sink_data[sink_id].volperc + step
	if volperc_new > 100 then
		volperc_new = 100
	elseif volperc_new < 0 then
		volperc_new = 0
	end
	local volnum_new = math.floor(((maxvol / 100) * volperc_new) + 0.5)
	if volnum_new ~= sink_data[sink_id].volnum then
		awful_util.spawn('pacmd set-sink-volume ' .. sink_id - 1 .. ' ' .. volnum_new, false)
		sink_data[sink_id].volperc = volperc_new
		sink_data[sink_id].volnum = volnum_new
	end
end

function pulseaudio_toggle(sink_id)
	if not sink_data[sink_id] or fatal_error or sink_data[sink_id].error_string then
		return
	elseif sink_data[sink_id].muted ~= nil then
		if sink_data[sink_id].muted then
			pulseaudio_unmute(sink_id)
		else
			pulseaudio_mute(sink_id)
		end
	end
end

function pulseaudio_mute(sink_id)
	if not sink_data[sink_id] or fatal_error or sink_data[sink_id].error_string then
		return
	end
	awful_util.spawn('pacmd set-sink-mute ' .. sink_id - 1 .. ' 1', false) 
	sink_data[sink_id].muted = true
end

function pulseaudio_unmute(sink_id)
	if not sink_data[sink_id] or fatal_error or sink_data[sink_id].error_string then
		return
	end
	awful_util.spawn('pacmd set-sink-mute ' .. sink_id - 1 .. ' 0', false) 
	sink_data[sink_id].muted = false
end

setmetatable(_M, { __call = function(_, ...) return vicious_worker(...) end })
