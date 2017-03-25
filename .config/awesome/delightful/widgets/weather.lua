local p=print
-------------------------------------------------------------------------------
--
-- Weather widget for Awesome 3.4
-- Copyright (C) 2011 Tuomas Jormola <tj@solitudo.net>
--
-- Licensed under the terms of GNU General Public License Version 2.0.
--
-- Description:
--
-- Displays current weather conditions. An icon indicates the generic
-- weather condition (clear with different icons for day and night with
-- moonphase display, rain, snow, storm and so on). Next to the icon
-- current temperature is displayed. When the mouse cursor is hovering
-- over the widget, detailed weather summary is displayed. Clicking
-- the icon launches an external application (if configured).
--
-- Widget uses Vicious widget framework to gather widget data.
--
-- This widget uses metar.lua by Tuomas Jormola <tj@solitudo.net>
-- http://solitudo.net/software/lua/metar/
--
-- This widget uses weatherlib.lua by Tuomas Jormola <tj@solitudo.net>
-- http://solitudo.net/software/lua/metar/
--
-- This widget optionally uses LuaExpat
-- http://www.keplerproject.org/luaexpat/
--
-- This widget optionally uses GWeather XML location datafile.
--
-- Widget uses icons from the package gnome-icon-theme.
--
--
-- Configuration:
--
-- The load() function expects to get the location configuration as
-- the 1st argument. You can specify as many location as you wish.
--
-- Format of the configuration is as follows.
-- {
-- -- Temperature unit to use when displaying text.
-- -- Possible values are 'c' for Celcius (default), and 'f' for Fahrenheit.
--        temperature_unit      = 'f',
-- -- Wind speed unit to use when displaying text.
-- -- Possible values are 'kn' for Knot, 'ms' for meters per second (default),
-- -- 'kmh' for kilometers per hour, and 'mph' for miles per hour.
--        wind_speed_unit       = 'mph',
-- -- Pressure unit to use when displaying text.
-- -- Possible values are 'hpa' for hectopascal (default), 'atm' for
-- -- standard atmosphere, and 'inhg' for inches of Mercury.
--        pressure_unit         = 'atm',
-- -- Visibility unit to use when displaying text.
-- -- Possible values are 'm' for meters (default), 'km' for kilometers,
-- -- 'ft' for feet, 'yd' for yards and 'mi' for miles.
--        visibility_unit       = 'y',
-- -- You have two options to specify the location. Either use the following
-- -- setting and directly specify a four-letter ICAO airport identifier code.
-- -- Weather report is fetched from this location in METAR form. See
-- -- http://www.airport-technology.com/icao-codes/ for the list of airport
-- -- codes. You can use this option to display weather reports without
-- -- requiring GWather and it's XML location datafile and LuaExpat
-- -- required to parse the file to be installed. However, in this case you're
-- -- not able to enjoy full weather report and icon displaying features since
-- -- coordinates and time zone info for the location are required to display
-- -- sun rise and set info and calculate moon phase.
--        station_code          = 'EGLL',
-- -- For full weather report you need to specify the location city using
-- -- the following setting. Format of the city string is
-- -- "<country>, <city name>" or just "<city name>" if the city name is
-- -- globally unique (in this case the first matching city name from
-- -- the GWeather location database is used). The city (and optional
-- -- country) is matched against the GWeather XML database which contains
-- -- additional information about the location (ICAO airport code for fetching
-- -- the METAR data, weather station name, coordinates and timezone of
-- -- the location). For this to work, you need to have GWeather and
-- -- LuaExpat installed.
--        city                  = 'United Kingdom, London',
-- -- Timezone offset in seconds from the UTC time. Includes possible
-- -- daylight saving. Positive to East of UTC and negative to West of UTC,
-- -- e.g. "-5 * 3600" for Eastern Standard Time in the US.
-- -- Autodetected by default.
--        timezone_offset_local = 0,
-- -- Program that is launched when the user clicks on the widget area.
-- -- Empty by default.
--        command               = 'gnome-www-browser http://uk.weather.com/weather/today-London-UKXX0085',
-- -- Don't try to display any icons. Default is false (i.e. display icons).
--        no_icon               = true,
-- -- METAR data poll interval in seconds, 20 minutes by default
--        update_interval       = 30 * 60,
-- -- How long to show notifications in seconds, 10 seconds by default
--        notification_delay    = 5,
-- -- Location of the GWeather XML location datafile.
-- -- '/usr/share/libgweather/Locations.xml' by default.
--        gweather_file         = '/usr/local/share/libgweather/Locations.xml',
-- },
-- -- Following is a minimal configuration to display weather in
-- -- Helsinki, Finland with all the default settings and using
-- -- GWeather location database.
-- {
--        city = 'Helsinki',
-- },
--
--
-- Theme:
--
-- The widget uses following icons and fonts if available in the Awesome theme.
--
-- theme.delightful_weather_clear                       - icon shown when the sky is clear and it's day or sun position is not known
-- theme.delightful_weather_clear_night                 - icon shown when it's night and the sky is clear
-- theme.delightful_weather_clear_night_[000..350]      - icon shown when it's night adn the sky is clear and the moon is at given position between 0 and 350
-- theme.delightful_weather_few_clouds                  - icon shown when there're few clouds in the sky and it's day or sun position is not known
-- theme.delightful_weather_few_clouds_night            - icon shown when it's night and there're few clouds in the sky
-- theme.delightful_weather_few_clouds_night_[000..350] - icon shown when it's night and there're few clouds in the sky and the moon is at given position between 0 and 350
-- theme.delightful_weather_overcast                    - icon shown when the sky is overcast
-- theme.delightful_weather_alert                       - icon shown when there's a serious weather condition (ѕand storm, volcanic ash, etc.)
-- theme.delightful_weather_storm                       - icon shown when it's stormy
-- theme.delightful_weather_snow                        - icon shown when there's frozen particles in the air (snow, hale, etc.)
-- theme.delightful_weather_scattered_showers           - icon shown when there's scattered showers
-- theme.delightful_weather_showers                     - icon shown when there's showers
-- theme.delightful_weather_fog                         - icon shown when it's foggy or misty
-- theme.delightful_not_found                           - icon shown when weather status is unknown
-- theme.delightful_error                               - icon shown when critical error has occurred
-- theme.monospace_font                                 - font for status text
--
-------------------------------------------------------------------------------

local awful_button     = require('awful.button')
local awful_util       = require('awful.util')
local beautiful         = require('beautiful')
local capi             = { mouse = mouse, screen = screen }
local image            = require('image')
local naughty          = require('naughty')
local widget           = require('widget')

local io               = { lines = io.lines }
local math             = { floor = math.floor }
local os               = { date = os.date, time = os.time }
local pairs            = pairs
local require          = require
local setmetatable     = setmetatable
local string           = { format = string.format }
local table            = { insert = table.insert }
local tonumber         = tonumber
local tostring         = tostring
local type             = type

local delightful_utils = require('delightful.utils')
local vicious          = require('vicious')

local metar            = require('metar')
local weatherlib       = require('weatherlib')

module('delightful.widgets.weather')

local widgets          = {}
local icons            = {}
local icon_files       = {}
local prev_icons       = {}
local weather_config   = {}
local weather_data     = {}

local icon_description = {
	weather_clear             = { beautiful_name = 'delightful_weather_clear',             default_icon = function() return 'weather-clear' end             },
	weather_clear_night       = { beautiful_name = 'delightful_weather_clear_night',       default_icon = function() return 'weather-clear-night' end       },
	weather_few_clouds        = { beautiful_name = 'delightful_weather_few_clouds',        default_icon = function() return 'weather-few-clouds' end        },
	weather_few_clouds_night  = { beautiful_name = 'delightful_weather_few_clouds_night',  default_icon = function() return 'weather-few-clouds-night' end  },
	weather_overcast          = { beautiful_name = 'delightful_weather_overcast',          default_icon = function() return 'weather-overcast' end          },
	weather_alert             = { beautiful_name = 'delightful_weather_alert',             default_icon = function() return 'weather-severe-alert' end      },
	weather_storm             = { beautiful_name = 'delightful_weather_strom',             default_icon = function() return 'weather-storm' end             },
	weather_snow              = { beautiful_name = 'delightful_weather_snow',              default_icon = function() return 'weather-snow' end              },
	weather_scattered_showers = { beautiful_name = 'delightful_weather_scattered_showers', default_icon = function() return 'weather-showers-scattered' end },
	weather_showers           = { beautiful_name = 'delightful_weather_showers',           default_icon = function() return 'weather-showers' end           },
	weather_fog               = { beautiful_name = 'delightful_weather_fog',               default_icon = function() return 'weather-fog' end               },
	not_found                 = { beautiful_name = 'delightful_not_found',                 default_icon = function() return 'dialog-question' end           },
	error                     = { beautiful_name = 'delightful_error',                     default_icon = function() return 'error' end                     },
}
-- dynamically generate entries for the moon phase icons
local night_icons = { 'weather_clear', 'weather_few_clouds' }
for n = 0, 35 do
	if n ~= 18 then
		for _, day_icon in pairs(night_icons) do
			local night_icon = string.format('%s_night_%03d', day_icon, n * 10)
			icon_description[night_icon] = { beautiful_name = string.format('delightful_%s', night_icon), default_icon = function() return night_icon:gsub('_', '-') end }
		end
	end
end

-- icon mappings for METAR data
local metar_icons = {
	sky = {
		not_found = {
			metar.SKY_STATUS.UNKNOWN,
		},
		weather_fog = {
			metar.SKY_STATUS.OBSCURE,
		},
		weather_clear = {
			metar.SKY_STATUS.CLEAR,
			metar.SKY_STATUS.NO_SIGNIFICANT_CLOUDS,
			metar.SKY_STATUS.NO_CLOUDS_DETECTED,
		},
	},
	clouds = {
		weather_clear = {
			metar.CLOUD_COVERAGE.CLEAR,
		},
		weather_few_clouds = {
			metar.CLOUD_COVERAGE.FEW,
			metar.CLOUD_COVERAGE.SCATTERED,
			metar.CLOUD_COVERAGE.BROKEN_SKY,
		},
		weather_overcast = {
			metar.CLOUD_COVERAGE.OVERCAST,
		},
	},
	phenomena = {
		weather_alert = {
			metar.WEATHER_PHENOMENA.DUST_WHIRLS,
			metar.WEATHER_PHENOMENA.DUST_STORM,
			metar.WEATHER_PHENOMENA.SAND_STORM,
			metar.WEATHER_PHENOMENA.FUNNEL_CLOUD,
			metar.WEATHER_PHENOMENA.WIDESPREAD_DUST,
			metar.WEATHER_PHENOMENA.VOLCANIC_ASH,
			metar.WEATHER_PHENOMENA.SQUALLS,
		},
		weather_snow = {
			metar.WEATHER_PHENOMENA.ICE_CRYSTALS,
			metar.WEATHER_PHENOMENA.ICE_PELLETS,
			metar.WEATHER_PHENOMENA.SNOW,
			metar.WEATHER_PHENOMENA.SNOW_GRAINS,
		},
		weather_scattered_showers = {
			metar.WEATHER_PHENOMENA.DRIZZLE,
			metar.WEATHER_PHENOMENA.SMALL_HAIL,
			metar.WEATHER_PHENOMENA.HAIL,
		},
		weather_showers = {
			metar.WEATHER_PHENOMENA.RAIN,
		},
		weather_fog = {
			metar.WEATHER_PHENOMENA.MIST,
			metar.WEATHER_PHENOMENA.FOG,
			metar.WEATHER_PHENOMENA.SMOKE,
			metar.WEATHER_PHENOMENA.HAZE,
			metar.WEATHER_PHENOMENA.SAND,
			metar.WEATHER_PHENOMENA.SPRAY,
			metar.WEATHER_PHENOMENA.WIDESPREAD_DUST,
		},
		not_found = {
			metar.WEATHER_PHENOMENA.UNKNOWN,
		},
	},
	descriptor = {
		weather_showers = {
			metar.WEATHER_DESCRIPTOR.SHOWERS,
		},
		weather_storm = {
			metar.WEATHER_DESCRIPTOR.THUNDERSTORM,
		},
		weather_snow = {
			metar.WEATHER_DESCRIPTOR.FREEZING,
		},
	}
}
for metar_icon_type, weather_values in pairs(metar_icons) do
	for icon_name, weather_types in pairs(weather_values) do
		for _, weather_type in pairs(weather_types) do
			local metar_icon_key = string.format('weather_%s_%d', metar_icon_type, weather_type)
			icon_description[metar_icon_key] = icon_description[icon_name]
		end
	end
end

-- mappings how to display METAR data in text
local metar_strings = {
	wind_direction     = {},
	cloud_coverage     = {},
	cloud_type         = {},
	sky                = {},
	weather_intensity  = {},
	weather_descriptor = {},
	weather_phenomena  = {},
}
metar_strings.wind_direction[metar.WIND_DIRECTION.VRB]                   = 'Variable direction'
metar_strings.wind_direction[metar.WIND_DIRECTION.N]                     = 'North'
metar_strings.wind_direction[metar.WIND_DIRECTION.NNE]                   = 'North - North East'
metar_strings.wind_direction[metar.WIND_DIRECTION.NE]                    = 'North East'
metar_strings.wind_direction[metar.WIND_DIRECTION.ENE]                   = 'East - North East'
metar_strings.wind_direction[metar.WIND_DIRECTION.E]                     = 'East'
metar_strings.wind_direction[metar.WIND_DIRECTION.ESE]                   = 'East - South East'
metar_strings.wind_direction[metar.WIND_DIRECTION.SE]                    = 'South East'
metar_strings.wind_direction[metar.WIND_DIRECTION.SSE]                   = 'South - South East'
metar_strings.wind_direction[metar.WIND_DIRECTION.S]                     = 'South'
metar_strings.wind_direction[metar.WIND_DIRECTION.SSW]                   = 'South - South West'
metar_strings.wind_direction[metar.WIND_DIRECTION.SW]                    = 'South West'
metar_strings.wind_direction[metar.WIND_DIRECTION.WSW]                   = 'West - South West'
metar_strings.wind_direction[metar.WIND_DIRECTION.W]                     = 'West'
metar_strings.wind_direction[metar.WIND_DIRECTION.WNW]                   = 'West - North West'
metar_strings.wind_direction[metar.WIND_DIRECTION.NW]                    = 'North West'
metar_strings.wind_direction[metar.WIND_DIRECTION.NNW]                   = 'North - North West'
metar_strings.cloud_coverage[metar.CLOUD_COVERAGE.CLEAR]                 = 'Clear'
metar_strings.cloud_coverage[metar.CLOUD_COVERAGE.FEW]                   = 'Few clouds'
metar_strings.cloud_coverage[metar.CLOUD_COVERAGE.SCATTERED]             = 'Scattered clouds'
metar_strings.cloud_coverage[metar.CLOUD_COVERAGE.BROKEN_SKY]            = 'Broken sky'
metar_strings.cloud_coverage[metar.CLOUD_COVERAGE.OVERCAST]              = 'Overcast'
metar_strings.cloud_type[metar.CLOUD_TYPE.CUMULONIMBUS]                  = 'Cumulonimbus'
metar_strings.cloud_type[metar.CLOUD_TYPE.TOWERING_CUMULUS]              = 'Towering Cumulus'
metar_strings.sky[metar.SKY_STATUS.UNKNOWN]                              = 'Unknown'
metar_strings.sky[metar.SKY_STATUS.OBSCURE]                              = 'Obscured'
metar_strings.sky[metar.SKY_STATUS.CLOUDS]                               = 'Cloudy'
metar_strings.sky[metar.SKY_STATUS.CLEAR]                                = 'Clear'
metar_strings.sky[metar.SKY_STATUS.NO_SIGNIFICANT_CLOUDS]                = 'No significant clouds'
metar_strings.sky[metar.SKY_STATUS.NO_CLOUDS_DETECTED]                   = 'No clouds detected'
metar_strings.weather_intensity[metar.WEATHER_INTENSITY.MODERATE]        = ''
metar_strings.weather_intensity[metar.WEATHER_INTENSITY.LIGHT]           = 'Light'
metar_strings.weather_intensity[metar.WEATHER_INTENSITY.HEAVY]           = 'Heavy'
metar_strings.weather_intensity[metar.WEATHER_INTENSITY.VICINITY]        = 'In vicinity'
metar_strings.weather_descriptor[metar.WEATHER_DESCRIPTOR.SHALLOW]       = 'Shallow'
metar_strings.weather_descriptor[metar.WEATHER_DESCRIPTOR.PARTIAL]       = 'Partial'
metar_strings.weather_descriptor[metar.WEATHER_DESCRIPTOR.PATCHES]       = 'Patches'
metar_strings.weather_descriptor[metar.WEATHER_DESCRIPTOR.DRIFTING]      = 'Drifting'
metar_strings.weather_descriptor[metar.WEATHER_DESCRIPTOR.BLOWING]       = 'Blowing'
metar_strings.weather_descriptor[metar.WEATHER_DESCRIPTOR.SHOWERS]       = 'Showers'
metar_strings.weather_descriptor[metar.WEATHER_DESCRIPTOR.THUNDERSTORM]  = 'Thunder storm'
metar_strings.weather_descriptor[metar.WEATHER_DESCRIPTOR.FREEZING]      = 'Freezing'
metar_strings.weather_phenomena[metar.WEATHER_PHENOMENA.DRIZZLE]         = 'Drizzle'
metar_strings.weather_phenomena[metar.WEATHER_PHENOMENA.RAIN]            = 'Rain'
metar_strings.weather_phenomena[metar.WEATHER_PHENOMENA.SNOW]            = 'Snow'
metar_strings.weather_phenomena[metar.WEATHER_PHENOMENA.SNOW_GRAINS]     = 'Snow grains'
metar_strings.weather_phenomena[metar.WEATHER_PHENOMENA.ICE_CRYSTALS]    = 'Ice crystals'
metar_strings.weather_phenomena[metar.WEATHER_PHENOMENA.ICE_PELLETS]     = 'Ice pellets'
metar_strings.weather_phenomena[metar.WEATHER_PHENOMENA.HAIL]            = 'Hail'
metar_strings.weather_phenomena[metar.WEATHER_PHENOMENA.SMALL_HAIL]      = 'Small Hail'
metar_strings.weather_phenomena[metar.WEATHER_PHENOMENA.UNKNOWN]         = 'Unknown'
metar_strings.weather_phenomena[metar.WEATHER_PHENOMENA.MIST]            = 'Mist'
metar_strings.weather_phenomena[metar.WEATHER_PHENOMENA.FOG]             = 'Fog'
metar_strings.weather_phenomena[metar.WEATHER_PHENOMENA.SMOKE]           = 'Smoke'
metar_strings.weather_phenomena[metar.WEATHER_PHENOMENA.VOLCANIC_ASH]    = 'Volcanic ash'
metar_strings.weather_phenomena[metar.WEATHER_PHENOMENA.WIDESPREAD_DUST] = 'Widespread dust'
metar_strings.weather_phenomena[metar.WEATHER_PHENOMENA.SAND]            = 'Sand'
metar_strings.weather_phenomena[metar.WEATHER_PHENOMENA.HAZE]            = 'Haze'
metar_strings.weather_phenomena[metar.WEATHER_PHENOMENA.SPRAY]           = 'Spray'
metar_strings.weather_phenomena[metar.WEATHER_PHENOMENA.DUST_WHIRLS]     = 'Dust whirls'
metar_strings.weather_phenomena[metar.WEATHER_PHENOMENA.SQUALLS]         = 'Squalls'
metar_strings.weather_phenomena[metar.WEATHER_PHENOMENA.FUNNEL_CLOUD]    = 'Funnel cloud'
metar_strings.weather_phenomena[metar.WEATHER_PHENOMENA.SAND_STORM]      = 'Sand storm'
metar_strings.weather_phenomena[metar.WEATHER_PHENOMENA.DUST_STORM]      = 'Dust storm'

-- lists supported units, their mapping with configuration options
-- and how to display the units in text
local unit_data = {
	temperature = {
		units = {
			weatherlib.TEMPERATURE_UNITS.CELCIUS,
			weatherlib.TEMPERATURE_UNITS.FAHRENHEIT,
		},
		config_options = {
			'c',
			'f',
		},
		display_texts = {
			'Celcius',
			'Fahrenheit',
		},
		display_units = {
			' °C',
			' °F',
		},
	},
	speed = {
		units = {
			weatherlib.SPEED_UNITS.KNOT,
			weatherlib.SPEED_UNITS.MS,
			weatherlib.SPEED_UNITS.KMH,
			weatherlib.SPEED_UNITS.MPH,
		},
		config_options = {
			'kn',
			'ms',
			'kmh',
			'mph',
		},
		display_texts = {
			'knots',
			'meters per second',
			'kilometers per hour',
			'miles per hour',
		},
		display_units = {
			' kn',
			' m/s',
			' km/h',
			' mph',
		},
	},
	pressure = {
		units = {
			weatherlib.PRESSURE_UNITS.HPA,
			weatherlib.PRESSURE_UNITS.ATM,
			weatherlib.PRESSURE_UNITS.INHG,
		},
		config_options = {
			'hpa',
			'atm',
			'inhg',
		},
		display_texts = {
			'hectopascal',
			'Standard atmosphere',
			'Inches of Merucry',
		},
		display_units = {
			' hPa',
			' atm',
			' inHg',
		},
	},
	length = {
		units = {
			weatherlib.LENGTH_UNITS.METER,
			weatherlib.LENGTH_UNITS.KILOMETER,
			weatherlib.LENGTH_UNITS.FOOT,
			weatherlib.LENGTH_UNITS.YARD,
			weatherlib.LENGTH_UNITS.MILE,
		},
		config_options = {
			'm',
			'km',
			'ft',
			'yd',
			'mi',
		},
		display_texts = {
			'meters',
			'kilometers',
			'feet',
			'yards',
			'miles',
		},
		display_units = {
			'm',
			'km',
			'ft',
			'yd',
			'mi',
		},
	},
}

-- define fields in the summary text
local summary_field_data = {
	{
		title    = 'Location',
		data_key = 'location',
	},
	{
		title    = 'Updated',
		data_key = 'updated',
	},
	{
		title    = 'Conditions',
		data_key = 'weather',
	},
	{
		title    = 'Sky',
		data_key = 'sky',
	},
	{
		title    = 'Temperature',
		data_key = 'temperature',
	},
	{
		title    = 'Dew point',
		data_key = 'dewpoint',
	},
	{
		title    = 'Relative humidity',
		data_key = 'humidity',
	},
	{
		title    = 'Wind',
		data_key = 'wind',
	},
	{
		title    = 'Pressure',
		data_key = 'pressure',
	},
	{
		title    = 'Visibility',
		data_key = 'visibility',
	},
	{
		title    = 'Sunrise',
		data_key = 'sunrise',
	},
	{
		title    = 'Sunset',
		data_key = 'sunset',
	},
}

-- Configuration handler
function check_config_unit(data, config_unit)
	if not data then
		return false, 'internal error, data is nil'
	end
	if not config_unit then
		return false, 'internal error, config unit is nil'
	end
	local found = false
	for _, check_config_unit in pairs(data.units) do
		if config_unit == data.config_options[check_config_unit] then
			found = true
			break
		end
	end
	if found then
		return true
	end
	local error_string = 'needs to be one of the following: '
	for _, unit in pairs(data.units) do
		error_string = string.format('%s"%s" (%s), ', error_string, data.config_options[unit], data.display_texts[unit])
	end
	return false, error_string:gsub(',%s*$', '')
end

local config_description = {
	{
		name     = 'temperature_unit',
		required = true,
		default  = 'c',
		validate = function(value) if not value then return true end return check_config_unit(unit_data.temperature, value) end
	},
	{
		name     = 'wind_speed_unit',
		required = true,
		default  = 'ms',
		validate = function(value) if not value then return true end return check_config_unit(unit_data.speed, value) end
	},
	{
		name     = 'pressure_unit',
		required = true,
		default  = 'hpa',
		validate = function(value) if not value then return true end return check_config_unit(unit_data.pressure, value) end
	},
	{
		name     = 'visibility_unit',
		required = true,
		default  = 'm',
		validate = function(value) if not value then return true end return check_config_unit(unit_data.length, value) end
	},
	{
		name     = 'city',
		validate = function(value) return delightful_utils.config_string(value) end
	},
	{
		name     = 'timezone_offset_local',
		required = true,
		default  = function(config_data) return weatherlib.calc_timezone_offset() end,
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
		default  = 20 * 60,
		validate = function(value) return delightful_utils.config_int(value) end
	},
	{
		name     = 'notification_delay',
		required = true,
		default  = 10,
		validate = function(value) return delightful_utils.config_int(value) end
	},
	{
		name     = 'gweather_file',
		default  = function(config_data) if config_data.city then return '/usr/share/libgweather/Locations.xml' end return nil end,
		validate = function(value) return delightful_utils.config_file(value) end
	},
	-- User is not supposed to supply configuration of this setting
	{
		name     = 'gweather_data',
		default  = function(config_data)
			if not config_data or not config_data.gweather_file then
				return nil
			end
			local country, city, station_code
			if config_data.city then
				country, city = config_data.city:match('^([^/]+)/(.+)$')
				if not country then
					city = config_data.city
				end
			elseif config_data.station_code then
				station_code = config_data.station_code
			else
				return nil
			end
			-- parse station code, station name, coordinates, city, country,
			-- and timezone from GWeather XML location datafile
			local check_data = { country = country, city = city, station_code = station_code }
			local gweather_data = {}
			local open = { country = false, city = false, location = false }
			local skip = { country = false, city = false }
			local session_data = {}
			local data_gatherers = {
				country = function(p, v)
					session_data.country = v
				end,
				city = function(p, v)
					session_data.city = v
				end,
				coordinates = function(p, v)
					if check_data.station_code then
						session_data.coordinates = v
					else
						if not gweather_data.coordinates then
							gweather_data.coordinates = v
						end
					end
				end,
				location_name = function(p, v)
					if check_data.station_code then
						session_data.station_name = v
					else
						if not gweather_data.station_name then
							gweather_data.station_name = v
						end
					end
				end,
				location_code = function(p, v)
					if check_data.station_code then
						session_data.station_code = v
					else
						if not gweather_data.station_code then
							gweather_data.station_code = v
						end
					end
				end,
				tz = function(p, v)
					if not gweather_data.station_code then
						gweather_data.timezone = v
					end
				end,
			}
			local elements = { 'country', 'city', 'location' }
			local location_elements = { 'name', 'code' }
			local callbacks
			callbacks = {
				StartElement = function(parser, name)
					if gweather_data.station_code then
						return
					end
					for _, element in pairs(elements) do
						if name == element then
							open[element] = true
							return
						end
						if open[element] and name == 'name' and data_gatherers[element] then
							callbacks.CharacterData = data_gatherers[element]
							open[element] = false
							return
						end
						if element == 'location' and open[element] then
							for _, location_element in pairs(location_elements) do
								if name == location_element then
									local data_gatherer_key = 'location_' .. location_element
									callbacks.CharacterData = data_gatherers[data_gatherer_key]
									break
								end
							end
						end
						if skip[element] then
							return
						end
					end
					if not gweather_data.coordinates and name == 'coordinates' then
						callbacks.CharacterData = data_gatherers.coordinates
						return
					end
					if name == 'tz-hint' then
						callbacks.CharacterData = data_gatherers.tz
						return
					end
				end,
				EndElement = function(parser, name)
					callbacks.CharacterData = false
					if gweather_data.station_code then
						return
					end
					if check_data.station_code and open.location and name == 'code' and session_data.station_code:lower() == check_data.station_code:lower() then
						for element in pairs(session_data) do
							gweather_data[element] = session_data[element]
						end
						return
					else
						for _, element in pairs(elements) do
							if session_data[element] then
								if check_data[element] then
									skip[element] = session_data[element]:lower() ~= check_data[element]:lower()
								end
								if not skip[element] and element ~= 'location' then
									gweather_data[element] = session_data[element]
								end
								session_data[element] = nil
								return
							end
							if element == name then
								open[element] = false
								if skip[element] ~= nil then
									skip[element] = false
								end
								return
							end
						end
					end
				end,
				CharacterData = false,
			}
			local lxp = require('lxp')
			local parser = lxp.new(callbacks)
			for line in io.lines(config_data.gweather_file) do
				parser:parse(line)
			end
			parser:parse()
			parser:close()
			local required_entries = { 'station_code', 'station_name', 'coordinates', 'city', 'country', 'timezone' }
			for _, entry in pairs(required_entries) do
				if not gweather_data[entry] then
					return nil
				end
			end
			return gweather_data
		end
	},
	{
		name     = 'station_code',
		default  = function(config_data) if config_data.gweather_data then return config_data.gweather_data.station_code end return nil end,
		validate = function(value) return delightful_utils.config_string(value) end
	},
	-- User is not supposed to supply configuration of these settings
	{
		name     = 'station_name',
		default  = function(config_data) if config_data.gweather_data and config_data.gweather_data.station_name:lower() ~= config_data.city:lower() then return config_data.gweather_data.station_name end return nil end,
		validate = function(value) return delightful_utils.config_string(value) end
	},
	{
		name     = 'latitude',
		default  = function(config_data) if config_data.gweather_data then local l = config_data.gweather_data.coordinates:match('^([+-]?%d+\.%d+)%s+[+-]?%d+\.%d+$') if l then return tonumber(l) else return nil end end return nil end,
		validate = function(value) return delightful_utils.config_number(value) end
	},
	{
		name     = 'longitude',
		default  = function(config_data) if config_data.gweather_data then local l = config_data.gweather_data.coordinates:match('^[+-]?%d+\.%d+%s+([+-]?%d+\.%d+)$') if l then return tonumber(l) else return nil end end return nil end,
		validate = function(value) return delightful_utils.config_number(value) end
	},
	{
		name     = 'country',
		default  = function(config_data) if config_data.gweather_data then return config_data.gweather_data.country end return nil end,
		validate = function(value) return delightful_utils.config_string(value) end
	},
	{
		name     = 'timezone',
		default  = function(config_data) if config_data.gweather_data then return config_data.gweather_data.timezone end return nil end,
		validate = function(value) return delightful_utils.config_string(value) end
	},
	{
		name     = 'timezone_offset',
		default  = function(config_data) if config_data.timezone then return weatherlib.calc_timezone_offset(config_data.timezone) end return nil end,
		validate = function(value) return delightful_utils.config_int(value) end
	},
	{
		name     = 'font',
		required = true,
		default  = function(config_data) return beautiful.monospace_font or 'monospace' end,
		validate = function(value) return delightful_utils.config_string(value) end
	},
}

-- get current METAR data and use populate weather data
function update_data(station_index)
	if not weather_data[station_index].metar then
		return
	end
	weather_data[station_index].status_string = nil
	weather_data[station_index].error_string  = nil
	weather_data[station_index].summary       = nil
	weather_data[station_index].metar_data    = nil
	-- download METAR data
	local metar_data, metar_error = weather_data[station_index].metar:get_metar_data()
	if not metar_data then
		if not metar_error then
			metar_error = 'Unknown error'
		end
		weather_data[station_index].error_string = string.format('Failed to read weather data: %s', metar_error)
		return
	end
	weather_data[station_index].metar_data = metar_data

	-- update summary data...
	weather_data[station_index].summary= {}
	-- ...location
	weather_data[station_index].summary.location = string.format('Weather station %s', weather_config[station_index].station_code)
	if weather_config[station_index].city then
		weather_data[station_index].summary.location = weather_config[station_index].city
		if weather_config[station_index].country then
			weather_data[station_index].summary.location = string.format('%s, %s', weather_data[station_index].summary.location, weather_config[station_index].country)
		end
		if weather_config[station_index].station_name then
			weather_data[station_index].summary.location = string.format('%s (weather station %s)', weather_data[station_index].summary.location, weather_config[station_index].station_name)
		end
	end
	-- ...time stamp
	if metar_data.timestamp then
		-- print in local time of timezone available, otherwise display in UTC
		if weather_config[station_index].timezone_offset_local then
			weather_data[station_index].summary.updated = os.date('%c', metar_data.timestamp + weather_config[station_index].timezone_offset_local)
		else
			weather_data[station_index].summary.updated = os.date('%c UTC', metar_data.timestamp)
		end
	else
		weather_data[station_index].summary.updated = 'N/A'
		delightful_utils.print_error('weather', 'No timestamp in METAR data')
	end
	-- ..weather
	if metar_data.weather then
		local weather_words = {}
		if metar_data.weather.intensity then
			if #metar_strings.weather_intensity[metar_data.weather.intensity] > 0 then
				table.insert(weather_words, metar_strings.weather_intensity[metar_data.weather.intensity])
			end
			if metar_data.weather.phenomena then
				table.insert(weather_words, metar_strings.weather_phenomena[metar_data.weather.phenomena])
				if metar_data.weather.descriptor then
					table.insert(weather_words, string.format('/ %s', metar_strings.weather_descriptor[metar_data.weather.descriptor]))
				end
				local weather_string = ''
				for weather_word_index, weather_word in pairs(weather_words) do
					if weather_word_index > 1 then
						weather_word = weather_word:lower()
					end
					weather_string = string.format('%s%s ', weather_string, weather_word)
				end
				weather_data[station_index].summary.weather = weather_string:gsub('%s*$', '')
			else
				weather_data[station_index].summary.weather = 'N/A'
				delightful_utils.print_error('weather', 'No weather.phenomena in METAR data')
			end
		else
			weather_data[station_index].summary.weather = 'N/A'
			delightful_utils.print_error('weather', 'No weather.intensity in METAR data')
		end
	else
		weather_data[station_index].summary.weather = 'N/A'
	end
	-- ...sky
	if metar_data.sky then
		local sky = metar_strings.sky[metar_data.sky]
		if metar_data.sky == metar.SKY_STATUS.CLOUDS then
			if metar_data.clouds and type(metar_data.clouds) == 'table' and metar_data.clouds[1] then
				if metar_data.clouds[1].coverage then
					sky = metar_strings.cloud_coverage[metar_data.clouds[1].coverage]
					if metar_data.clouds[1].type then
						sky = string.format('%s, %s', sky, metar_strings.cloud_type[metar_data.clouds[1].type])
					end
				else
					weather_data[station_index].summary.sky = 'N/A'
					delightful_utils.print_error('weather', 'No clouds[1].coverage in METAR data')
				end
			else
				weather_data[station_index].summary.sky = 'N/A'
				delightful_utils.print_error('weather', 'No clouds in METAR data')
			end
		end
		weather_data[station_index].summary.sky = sky
	else
		weather_data[station_index].summary.sky = 'N/A'
		delightful_utils.print_error('weather', 'No sky in METAR data')
	end
	-- ...feels like temperature
	if metar_data.temperature
			and metar_data.dewpoint
			and metar_data.wind
			and metar_data.wind.speed then
		weather_data[station_index].summary.feels_like = string.format('%g%s',
				weatherlib.convert_temperature(weatherlib.TEMPERATURE_UNITS.FAHRENHEIT, weather_data[station_index].units.temperature,
						weatherlib.calc_apparent_temperature(
								weatherlib.convert_temperature(weatherlib.TEMPERATURE_UNITS.CELCIUS, weatherlib.TEMPERATURE_UNITS.FAHRENHEIT, metar_data.temperature),
								weatherlib.convert_speed(weatherlib.SPEED_UNITS.KNOT, weatherlib.SPEED_UNITS.MPH, metar_data.wind.speed),
								weatherlib.calc_humidity(metar_data.temperature, metar_data.dewpoint)
						)
				),
				weather_data[station_index].display_units.temperature)
	end
	-- ...temperature
	if metar_data.temperature then
		local temperature = string.format('%g%s',
				weatherlib.convert_temperature(weatherlib.TEMPERATURE_UNITS.CELCIUS, weather_data[station_index].units.temperature, metar_data.temperature),
				weather_data[station_index].display_units.temperature)
		if weather_data[station_index].summary.feels_like and weather_data[station_index].summary.feels_like ~= temperature then
			temperature = string.format('%s, feels like %s', temperature, weather_data[station_index].summary.feels_like)
		end
		weather_data[station_index].summary.temperature = temperature
	else
		weather_data[station_index].summary.temperature = 'N/A'
		delightful_utils.print_error('weather', 'No temperature in METAR data')
	end
	-- ...dew point
	if metar_data.dewpoint then
		weather_data[station_index].summary.dewpoint = string.format('%g%s',
				weatherlib.convert_temperature(weatherlib.TEMPERATURE_UNITS.CELCIUS, weather_data[station_index].units.temperature, metar_data.dewpoint),
				weather_data[station_index].display_units.temperature)
	else
		weather_data[station_index].summary.dewpoint = 'N/A'
		delightful_utils.print_error('weather', 'No dewpoint in METAR data')
	end
	-- ...humidity
	if metar_data.temperature and metar_data.dewpoint then
		weather_data[station_index].summary.humidity = string.format('%s%%', weatherlib.calc_humidity(metar_data.temperature, metar_data.dewpoint))
	else
		weather_data[station_index].summary.humidity = 'N/A'
	end
	-- ..‥wind
	if metar_data.wind and type(metar_data.wind) == 'table' then
		if metar_data.wind.direction then
			if metar_data.wind.speed then
				local gust = ''
				if metar_data.wind.gust then
					gust = string.format(', gust %g%s',
							metar_data.wind.gust,
							weather_data[station_index].display_units.speed)

				end
				weather_data[station_index].summary.wind = string.format('%s, %g%s%s',
						metar_strings.wind_direction[metar_data.wind.direction],
						weatherlib.convert_speed(weatherlib.SPEED_UNITS.KNOT, weather_data[station_index].units.speed, metar_data.wind.speed),
						weather_data[station_index].display_units.speed,
						gust)
			else
				weather_data[station_index].summary.wind = 'N/A'
				delightful_utils.print_error('weather', 'No wind.speed in METAR data')
			end
		else
			weather_data[station_index].summary.wind = 'N/A'
			delightful_utils.print_error('weather', 'No wind.direction in METAR data')
		end
	else
		weather_data[station_index].summary.wind = 'N/A'
		delightful_utils.print_error('weather', 'No wind in METAR data')
	end
	-- ...pressure
	if metar_data.pressure then
		weather_data[station_index].summary.pressure = string.format('%g%s',
				weatherlib.convert_pressure(weatherlib.PRESSURE_UNITS.HPA, weather_data[station_index].units.pressure, metar_data.pressure),
				weather_data[station_index].display_units.pressure)
	else
		weather_data[station_index].summary.pressure = 'N/A'
		delightful_utils.print_error('weather', 'No pressure in METAR data')
	end
	-- ...visibility
	if metar_data.visibility and type(metar_data.visibility) == 'table' and metar_data.visibility[1] then
		local visibility_value = metar_data.visibility[1].distance
		local visibility_prefix = ''
		if visibility_value== 9999 then
			visibility_value = 10000
			visibility_prefix = 'More than '
		end
		weather_data[station_index].summary.visibility = string.format('%s%g%s',
				visibility_prefix,
				weatherlib.convert_length(weatherlib.LENGTH_UNITS.METER, weather_data[station_index].units.length, visibility_value),
				weather_data[station_index].display_units.length)
	else
		weather_data[station_index].summary.visibility = 'N/A'
		delightful_utils.print_error('weather', 'No visibility in METAR data')
	end

	if weather_config[station_index].timezone_offset then
		local now = os.date('!*t')
		weather_data[station_index].now = os.date('*t', os.time(now) + weather_config[station_index].timezone_offset)
	end

	-- ...sunrise and sunset using weatherlib
	if weather_data[station_index].now and weather_config[station_index].timezone_offset and weather_config[station_index].latitude and weather_config[station_index].longitude then
		local timezone = weatherlib.convert_time(weatherlib.TIME_UNITS.S, weatherlib.TIME_UNITS.H, weather_config[station_index].timezone_offset)
		local sunrise = weatherlib.calc_sunrise(weather_data[station_index].now, weather_config[station_index].latitude, weather_config[station_index].longitude, timezone)
		local sunset  = weatherlib.calc_sunset(weather_data[station_index].now, weather_config[station_index].latitude, weather_config[station_index].longitude, timezone)
		if sunrise and sunset then
			local sky_text
			weather_data[station_index].summary.sunrise = os.date('%H:%M', os.time(sunrise))
			weather_data[station_index].summary.sunset  = os.date('%H:%M', os.time(sunset))
			weather_data[station_index].sunrise = sunrise
			weather_data[station_index].sunset  = sunset
			if weather_data[station_index].now.year == sunrise.year and weather_data[station_index].now.month == sunrise.month and weather_data[station_index].now.day == sunrise.day and weather_data[station_index].now.year == sunset.year and weather_data[station_index].now.month == sunset.month and weather_data[station_index].now.day == sunset.day then
				weather_data[station_index].daytime = os.time(weather_data[station_index].now) >= os.time(sunrise) and os.time(weather_data[station_index].now) < os.time(sunset)
			elseif os.time(weather_data[station_index].now) >= os.time(sunset) and os.time(weather_data[station_index].now) < os.time(sunrise) then
				-- polar night
				weather_data[station_index].daytime = false
				weather_data[station_index].summary.sunrise = os.date('Next sunrise %a %b %d %Y', os.time(sunrise))
				weather_data[station_index].summary.sunset  = os.date('Last sunset %a %b %d %Y', os.time(sunset))
				sky_text = 'Polar night'
			elseif os.time(weather_data[station_index].now) >= os.time(sunrise) and os.time(weather_data[station_index].now) < os.time(sunset) then
				-- midnight sun
				weather_data[station_index].daytime = true
				weather_data[station_index].summary.sunrise = os.date('Last sunrise %a %b %d %Y', os.time(sunrise))
				weather_data[station_index].summary.sunset  = os.date('Next sunset %a %b %d %Y', os.time(sunset))
				sky_text = 'Midnight sun'
			end
			-- update sky summary if required
			if sky_text then
				if weather_data[station_index].summary.sky then
					weather_data[station_index].summary.sky = string.format('%s, %s', weather_data[station_index].summary.sky, sky_text)
				else
					weather_data[station_index].summary.sky = sky_text
				end
			end
		else
			delightful_utils.print_error('weather', 'Failed to calculate sunrise and sunset')
		end
	end

	-- ...moonphase using weatherlib
	if weather_data[station_index].now and weather_config[station_index].latitude and weather_config[station_index].longitude then
		weather_data[station_index].moonphase = calc_moon_phase(weather_data[station_index].now, weather_config[station_index].latitude)
	end

	-- and finally set status string to the temperature
	if weather_data[station_index].summary.temperature then
		weather_data[station_index].status_string = weather_data[station_index].summary.temperature:gsub(', .+$', '')
	else
		weather_data[station_index].status_string = 'N/A'
	end
end

-- Update widget icon based on the weather
function update_icon(station_index)
	if not icon_files.not_found or not icon_files.error then
		return
	end
	if not station_index or not icons[station_index] or not weather_data[station_index] then
		return
	end
	local icon_file
	-- 1st try: use sky icon
	if weather_data[station_index].metar_data and weather_data[station_index].metar_data.sky then
		local sky_key = string.format('weather_sky_%d', weather_data[station_index].metar_data.sky)
		if icon_files[sky_key] then
			icon_file = icon_files[sky_key]
		end
	end
	-- 2nd try: use clouds icon
	if weather_data[station_index].metar_data and weather_data[station_index].metar_data.clouds and weather_data[station_index].metar_data.clouds[1] then
		local clouds_key = string.format('weather_clouds_%d', weather_data[station_index].metar_data.clouds[1].coverage)
		if icon_files[clouds_key] then
			icon_file = icon_files[clouds_key]
		end
	end
	-- 3rd try: search weather for the icon
	if weather_data[station_index].metar_data and weather_data[station_index].metar_data.weather then
		local weather_key, weather_icon_file
		if weather_data[station_index].metar_data.weather.descriptor then
			weather_key = string.format('weather_descriptor_%d', weather_data[station_index].metar_data.weather.descriptor)
			if icon_files[weather_key] then
				weather_icon_file = icon_files[weather_key]
			end
		end
		if not weather_icon_file or (weather_data[station_index].metar_data.weather.descriptor ~= metar.WEATHER_PHENOMENA.THUNDERSTORM) then
			if weather_data[station_index].metar_data.weather.intensity == metar.WEATHER_INTENSITY.LIGHT and
					weather_data[station_index].metar_data.weather.phenomena == metar.WEATHER_PHENOMENA.RAIN then
				weather_key = 'weather_scattered_showers'
			else
				weather_key = string.format('weather_phenomena_%d', weather_data[station_index].metar_data.weather.phenomena)
			end
			if icon_files[weather_key] then
				weather_icon_file = icon_files[weather_key]
			end
		end
		if weather_icon_file then
			icon_file = weather_icon_file
		end
	end
	-- 4th try: errors override the weather icon
	if weather_data[station_index].error_string then
		icon_file = icon_files.error
	end
	-- 5th try: still no icon, use the not found icon
	if not icon_file then
		icon_file = icon_files.not_found
		delightful_utils.print_error('weather', 'Weather icon not found')
	end
	-- 6th try: replace clear and few clouds icons with night versions if sun is set
	if icon_file and weather_data[station_index].daytime ~= nil and not weather_data[station_index].daytime then
		for _, day_icon in pairs(night_icons) do
			local night_icon = string.format('%s_night', day_icon)
			if icon_file == icon_files[day_icon] and icon_files[night_icon] then
				icon_file = icon_files[night_icon]
				break
			end
		end
	end
	-- 7th try: add moon phase icons if it's night
	if icon_file and weather_data[station_index].moonphase then
		for _, day_icon in pairs(night_icons) do
			local night_icon = string.format('%s_night', day_icon)
			if icon_file == icon_files[night_icon] then
				local night_icon_moon = string.format('%s_%03d', night_icon, weather_data[station_index].moonphase)
				if weather_data[station_index].moonphase == 180 then
					night_icon_moon = night_icon_moon:gsub('_180$', '')
				end
				if icon_files[night_icon_moon] then
					icon_file = icon_files[night_icon_moon]
					break
				end
			end
		end
	end
	-- apply the icon
	if icon_file and
			(not prev_icons[station_index] or prev_icons[station_index] ~= icon_file) then
		prev_icons[station_index] = icon_file
		icons[station_index].image = image(icon_file)
	end
end

-- Text for the hover notification
function summary_text(station_index)
	local text = ''
	if not station_index or not weather_data[station_index] then
		return text
	end
	if weather_data[station_index].error_string then
		text = weather_data[station_index].error_string;
	else
		for _, summary_field_entry in pairs(summary_field_data) do
			local value = weather_data[station_index].summary[summary_field_entry.data_key]
			if value then
				local formatted_value
				if summary_field_entry.format then
					formatted_value = summary_field_entry.format(value, station_index)
				else
					formatted_value = value
				end
				text = string.format('%s<span font_weight="bold">%s</span>: %s\n', text, pad_summary(summary_field_entry.title), formatted_value)
			end
		end
	end
	return text:gsub('\n*$', '')
end

-- Configuration handler
function handle_config(user_config)
	local empty_config = delightful_utils.get_empty_config(config_description)
	if not user_config or #user_config == 0 then
		table.insert(weather_data,   { error_string = 'No weather locations configured' })
		table.insert(weather_config, empty_config)
		return
	end
	for station_index, user_config_data in pairs(user_config) do
		weather_data[station_index] = {}
		local config_data = delightful_utils.normalize_config(user_config_data, config_description)
		local validation_errors = delightful_utils.validate_config(config_data, config_description)
		if not config_data.station_code then
			if config_data.city then
				if not config_data.gweather_data then
					if not validation_errors then
						validation_errors = {}
					end
					table.insert(validation_errors, string.format('City "%s" not found in GWeather XML location datafile', config_data.city))
				end
			else
				if not validation_errors then
					validation_errors = {}
				end
				table.insert('Must specify city name if no weather station code given given')
			end
		end
		if validation_errors then
			weather_data[station_index].error_string =
					string.format('Configuration errors:\n%s',
							delightful_utils.format_validation_errors(validation_errors))
			weather_config[station_index] = empty_config
			return
		end
		if not config_data.city and config_data.gweather_data then
			config_data.city = config_data.gweather_data.city
		end
		weather_config[station_index] = config_data
	end
end

-- Initalization
function load(self, config)
	handle_config(config)
	icon_files = delightful_utils.find_icon_files(icon_description)
	local units_config_mapping = {
		temperature_unit = 'temperature',
		wind_speed_unit  = 'speed',
		pressure_unit    = 'pressure',
		visibility_unit  = 'length'
	}
	for station_index, data in pairs(weather_data) do
		-- init data
		if weather_config[station_index].station_code then
			weather_data[station_index].metar = metar.new(weather_config[station_index].station_code)
		end
		weather_data[station_index].units         = {}
		weather_data[station_index].display_units = {}
		for config_unit_key, weather_unit_key in pairs(units_config_mapping) do
			local config_unit = weather_config[station_index][config_unit_key]
			if config_unit then
				for unit_index, unit_config_value in pairs(unit_data[weather_unit_key].config_options) do
					if config_unit == unit_config_value then
						weather_data[station_index].units[weather_unit_key]         = unit_data[weather_unit_key].units[unit_index]
						weather_data[station_index].display_units[weather_unit_key] = unit_data[weather_unit_key].display_units[unit_index]
					end
				end
			end
		end

		local icon
		if not weather_config[station_index].no_icon and icon_files.not_found and icon_files.error then
			icon = widget({ type = 'imagebox', name = 'weather_' .. station_index })
		end

		local popup_enter = function()
			local popup_title
			if data.error_string then
				popup_title = 'Error'
			end
			data.popup = naughty.notify({
					title   = popup_title,
					text    = summary_text(station_index),
					font    = weather_config[station_index].font or 'monospace',
					timeout = weather_config[station_index].notification_delay,
					screen  = capi.mouse.screen
			})
		end
		local popup_leave = function() naughty.destroy(data.popup) end

		local widget = widget({ type = 'textbox'})

		widget:add_signal('mouse::enter', popup_enter)
		widget:add_signal('mouse::leave', popup_leave)
		if icon then
			icon:add_signal('mouse::enter', popup_enter)
			icon:add_signal('mouse::leave', popup_leave)
		end
	
		if weather_config[station_index].command then
			local buttons = awful_button({}, 1, function()
					awful_util.spawn(weather_config[station_index].command, true)
			end)
			widget:buttons(buttons)
			if icon then
				icon:buttons(buttons)
			end
		end

		widgets[station_index] = widget
		icons[station_index]   = icon

		vicious.register(widget, self, '$1', weather_config[station_index].update_interval, station_index)
	end
	return widgets, icons
end

-- Vicious worker function
local function vicious_worker(format, station_index)
	update_data(station_index)
	update_icon(station_index)
	local status
	local error_status = '<span color="red">'
	if icons[station_index] then
		error_status = string.format('%s ', error_status);
	end
	error_status = string.format('%s!</span>', error_status);
	if not weather_data[station_index] then
		status = error_status
		delightful_utils.print_error('weather', string.format('No weather_data[%d]', station_index))
	else
		if weather_data[station_index].error_string then
			status = '<span color="red"> !</span>';
			delightful_utils.print_error('weather', weather_data[station_index].error_string)
		elseif weather_data[station_index].status_string then
			status = weather_data[station_index].status_string
		else
			weather_data[station_index].error_string = string.format('No weather_data[%s][status_string] or weather_data[%s][error_string]', station_index, station_index)
			status = '<span color="red"> !</span>';
			delightful_utils.print_error('weather', weather_data[station_index].error_string)
		end
	end
	return status
end

-- Helpers

function pad_summary(line)
	return delightful_utils.pad_string_with_spaces(line, 20)
end

function calc_moon_phase(time, latitude)
	local moon_phases = 36
	local moon_phase, moon_latitude = weatherlib.calc_moon(time)
	local phase = math.floor((moon_phase * moon_phases / 360) + 0.5)

	if phase == moon_phases then
		phase = 0
	elseif phase > 0 and weatherlib.convert_angle(weatherlib.ANGLE_UNITS.RAD, weatherlib.ANGLE_UNITS.DEG, latitude) < moon_latitude then
		-- Locations south of the moon's latitude will see the moon in the
		-- northern sky. The moon waxes and wanes from left to right
		-- so we reference an icon running in the opposite direction.
		phase = moon_phases - phase
	end
	return math.floor(phase * 360 / moon_phases)
end

setmetatable(_M, { __call = function(_, ...) return vicious_worker(...) end })
