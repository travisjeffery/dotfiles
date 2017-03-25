-------------------------------------------------------------------------------
--
-- Common utility routines for Delightful widgets for Awesome 3.4
-- Copyright (C) 2011 Tuomas Jormola <tj@solitudo.net>
--
-- Licensed under the terms of GNU General Public License Version 2.0.
--
-------------------------------------------------------------------------------

local awful_util        = require('awful.util')
local beautiful         = require('beautiful')

local freedesktop_utils = require('freedesktop.utils')

local pairs             = pairs
local print             = print
local string            = { format = string.format }
local table             = { insert = table.insert }
local tostring          = tostring
local type              = type

module('delightful.utils')

-- get configuration with defaults applied
function get_empty_config(config_description)
	local empty_config = {}
	for config_id, config_description_entry in pairs(config_description) do
		local config_key = config_description_entry.name
		empty_config[config_key] =
				evaluate_config_value(config_description_entry.default,
						empty_config, config_description_entry.coerce)
	end
	return empty_config
end

-- fully evaluate given configuration against description
function normalize_config(config_data, config_description)
	for config_id, config_description_entry in pairs(config_description) do
		local config_key = config_description_entry.name
		local config_value
		if config_data[config_key] == nil then
			config_value = config_description_entry.default
		else
			config_value = config_data[config_key]
		end
		config_data[config_key] =
				evaluate_config_value(config_value,
						config_data,
						config_description_entry.coerce)
	end
	return config_data
end

-- evaluate a configuration value
function evaluate_config_value(value, data, coerce)
	if type(value) == 'function' then
		value = value(data)
	end
	if coerce then
		value = coerce(value)
	end
	if value ~= nil and type(value) == 'string' and #value < 1 then
		value = nil
	elseif value ~= nil and type(value) == 'table' then
		local elemnum = 0
		for _ in pairs(value) do
			elemnum = elemnum + 1
		end
		if elemnum == 0 then
			value = nil
		end
	end
	return value
end

-- validate configuration against description
function validate_config(config_data, config_description)
	local validation_errors = {}
	-- check required options
	for config_id, config_description_entry in pairs(config_description) do
		local config_key = config_description_entry.name
		if config_description_entry.required and config_data[config_key] == nil then
			table.insert(validation_errors,
					string.format('Required configuration option missing or empty: %s', config_key))
		end
	end

	-- validate options
	for config_id, config_description_entry in pairs(config_description) do
		local config_key = config_description_entry.name
		if config_description_entry.validate and config_data[config_key] ~= nil then
			local is_valid, error_string = config_description_entry.validate(config_data[config_key])
			if not is_valid then
				table.insert(validation_errors,
						string.format('Invalid configuration option "%s": %s', config_key, error_string))
			end
		end
	end

	-- check unknown options
	for config_key, config_value in pairs(config_data) do
		local description_found = false
		for config_id, config_description_entry in pairs(config_description) do
			if config_key == config_description_entry.name then
				description_found = true
				break
			end
		end
		if not description_found then
			table.insert(validation_errors,
				string.format('Unknown configuration option: %s', config_key))
		end
	end

	return #validation_errors > 0 and validation_errors
end

-- return a string from validation errors
function format_validation_errors(validation_errors)
	local error_string = ''
	for error_index, error_entry in pairs(validation_errors) do
		error_string = string.format('%s%s', error_string, error_entry)
		if error_index < #validation_errors then
			error_string = string.format('%s\n', error_string)
		end
	end
	return error_string
end

-- configuration validators
function config_string(value)
	if type(value) ~= 'string' then
		return false, 'must be a string'
	end
	return true
end
function config_table(value)
	if type(value) ~= 'table' then
		return false, 'must be a table'
	end
	return true
end
function config_int(value)
	if type(value) ~= 'number' then
		return false, 'must be an integer'
	end
	if not tostring(value):find('^-?%d+$') then
		return false, 'must be an integer'
	end
	return true
end
function config_number(value)
	if type(value) ~= 'number' then
		return false, 'must be a number'
	end
	if not tostring(value):find('^-?%d+\.?%d*$') then
		return false, 'must be a number'
	end
	return true
end
function config_boolean(value)
	if type(value) ~= 'boolean' then
		return false, 'must be true or false'
	end
	return true
end
function config_file(value)
	if type(value) ~= 'string' then
		return false, 'invalid file name'
	end
	if not awful_util.file_readable(value) then
		return false, 'file not readable'
	end
	return true
end

-- type coersions
function coerce_table(value)
	if value == nil or type(value) == 'table' then
		return value
	else
		return { value }
	end
end

-- return the full paths to icon files using the description
function find_icon_files(icon_description)
	if not icon_description then
		return
	end
	-- Ensure GNOME icon theme is available
	local previous_icon_theme = freedesktop_utils.icon_theme
	local have_gnome_icon_theme = false
	if freedesktop_utils.icon_theme then
		have_gnome_icon_theme = freedesktop_utils.icon_theme == 'gnome'
	end
	if not have_gnome_icon_theme then
		freedesktop_utils.icon_theme = 'gnome'
	end
	-- Load icons
	local icon_files = {}
	for name, info in pairs(icon_description) do
		-- try icon defined in the Awesome theme
		if beautiful[info.beautiful_name] then
			icon_files[name] = beautiful[info.beautiful_name]
		end
		-- if no icon in the theme, try default
		if not icon_files[name] and info.default_icon then
			local default_icon
			if type(info.default_icon) == 'function' then
				default_icon = info.default_icon()
			else
				default_icon = info.default_icon
			end
			icon_files[name] = freedesktop_utils.lookup_icon({ icon = default_icon })
		end
	end
	if not have_gnome_icon_theme then
		freedesktop_utils.icon_theme = previous_icon_theme
	end
	return icon_files
end

-- check if the given colors are defined in the Awesome theme
function find_theme_color(colors)
	if type(colors) ~= 'table' then
		colors = { colors }
	end
	for _, theme_color_key in pairs(colors) do
		if beautiful[theme_color_key] then
			return beautiful[theme_color_key]
		end
	end
	return
end

-- pad a string with spaces
function pad_string_with_spaces(s, width)
	if not width then
		return s
	end
	if not s then
		s = ''
	end
	local s_head = s:sub(1, width)
	local space = ' '
	return string.format('%s%s', awful_util.escape(s_head), space:rep(width - #s_head))
end

-- print an error message
function print_error(ident, error_string)
	if not ident then
		return
	end
	if not error_string then
		error_string = ident
		ident = nil
	end
	local text = ''
	if ident then
		text = string.format('[%s] ', ident)
	end
	print(string.format('%s%s', text, error_string))
end
