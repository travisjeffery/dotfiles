-------------------------------------------------------------------------------
-- Lua class to parse METAR coded weather reports and fetch current METAR
-- reports from <a href="http://www.noaa.gov">NOAA</a> <a href="http://weather.noaa.gov">Internet Weather Service</a>.
-- The parser is pretty simple and by no means claims to support every feature
-- one might find in METAR coded weather reports. For example, weather forecasts
-- and automatic weather reports are not detected. Unsupported features in
-- the weather reports are silently dropped.
-- @author Tuomas Jormola
-- @copyright © 2011 Tuomas Jormola <a href="mailto:tj@solitudo.net">tj@solitudo.net</a> <a href="http://solitudo.net">http://solitudo.net</a>
--  Licensed under the terms of
-- the <a href="http://www.gnu.org/licenses/gpl-2.0.html">GNU General Public License Version 2.0</a>.
--
-------------------------------------------------------------------------------

local socket_http  = require('socket.http')

local weatherlib   = require('weatherlib')

local os           = { date = os.date, time = os.time }
local pairs        = pairs
local print        = print
local setmetatable = setmetatable
local string       = { format = string.format }
local table        = { insert = table.insert, maxn = table.maxn}
local tonumber     = tonumber
local type         = type

module('metar')

local token_data = {
	wind = {
		directions = {
			{ key = 'N',   min = 349, max = 11                              },
			{ key = 'NNE', min = 12,  max = 33                              },
			{ key = 'NE',  min = 34,  max = 56                              },
			{ key = 'ENE', min = 57,  max = 78                              },
			{ key = 'E',   min = 79,  max = 101                             },
			{ key = 'ESE', min = 102, max = 123                             },
			{ key = 'SE',  min = 124, max = 146                             },
			{ key = 'SSE', min = 147, max = 168                             },
			{ key = 'S',   min = 169, max = 191                             },
			{ key = 'SSW', min = 192, max = 213                             },
			{ key = 'SW',  min = 214, max = 236                             },
			{ key = 'WSW', min = 237, max = 258                             },
			{ key = 'W',   min = 259, max = 281                             },
			{ key = 'WNW', min = 282, max = 303                             },
			{ key = 'NW',  min = 304, max = 326                             },
			{ key = 'NNW', min = 327, max = 348                             },
		},
		speeds = {
			KT  = weatherlib.SPEED_UNITS.KNOT,
			MPS = weatherlib.SPEED_UNITS.MS,
			KMH = weatherlib.SPEED_UNITS.KMH,
		},
		offset = 1,
	},
	clouds = {
		coverages = {
			{ token = 'SKC', alias = 'CLR',   key = 'CLEAR'                 },
			{ token = 'FEW',                  key = 'FEW'                   },
			{ token = 'SCT',                  key = 'SCATTERED'             },
			{ token = 'BKN',                  key = 'BROKEN_SKY'            },
			{ token = 'OVC',                  key = 'OVERCAST'              },
		},
		types = {
			{ token = 'CB',                   key = 'CUMULONIMBUS'          },
			{ token = 'TCU',                  key = 'TOWERING_CUMULUS'      },
		},
	},
	sky = {
		types = {
			{ token = 'CAVOK', alias = 'SKC', key = 'CLEAR'                 },
			{ token = 'NSC',                  key = 'NO_SIGNIFICANT_CLOUDS' },
			{ token = 'NCD',                  key = 'NO_CLOUDS_DETECTED'    },
		},
		offset = 3,
	},
	weather_intensity = {
		types = {
			{ token = '-',                    key = 'LIGHT'                 },
			{ token = '+',                    key = 'HEAVY'                 },
			{ token = 'VC',                   key = 'VICINITY'              },
		},
		offset = 1,
	},
	weather_descriptor = {
		types = {
			{ token = 'MI',                   key = 'SHALLOW'               },
			{ token = 'PR',                   key = 'PARTIAL'               },
			{ token = 'BC',                   key = 'PATCHES'               },
			{ token = 'DR',                   key = 'DRIFTING'              },
			{ token = 'BL',                   key = 'BLOWING'               },
			{ token = 'SH',                   key = 'SHOWERS'               },
			{ token = 'TS',                   key = 'THUNDERSTORM'          },
			{ token = 'FZ',                   key = 'FREEZING'              },
		},
	},
	weather_phenomena = {
		types = {
			{ token = 'DZ',                   key = 'DRIZZLE'               },
			{ token = 'RA',                   key = 'RAIN'                  },
			{ token = 'SN',                   key = 'SNOW'                  },
			{ token = 'SG',                   key = 'SNOW_GRAINS'           },
			{ token = 'IC',                   key = 'ICE_CRYSTALS'          },
			{ token = 'PL', alias = 'PE',     key = 'ICE_PELLETS'           },
			{ token = 'GR',                   key = 'HAIL'                  },
			{ token = 'GS',                   key = 'SMALL_HAIL'            },
			{ token = 'UP',                   key = 'UNKNOWN'               },
			{ token = 'BR',                   key = 'MIST'                  },
			{ token = 'FG',                   key = 'FOG'                   },
			{ token = 'FU',                   key = 'SMOKE'                 },
			{ token = 'VA',                   key = 'VOLCANIC_ASH'          },
			{ token = 'DU',                   key = 'WIDESPREAD_DUST'       },
			{ token = 'SA',                   key = 'SAND'                  },
			{ token = 'HZ',                   key = 'HAZE'                  },
			{ token = 'PY',                   key = 'SPRAY'                 },
			{ token = 'PO',                   key = 'DUST_WHIRLS'           },
			{ token = 'SQ',                   key = 'SQUALLS'               },
			{ token = 'FC',                   key = 'FUNNEL_CLOUD'          },
			{ token = 'SS',                   key = 'SAND_STORM'            },
			{ token = 'DS',                   key = 'DUST_STORM'            },
		},
	}
}

-------------------------------------------------------------------------------
-- Wind direction table.
-- Values from this table are used in the result table with key <code>wind.direction</code>
-- returned by <a href="#metatable.__index:get_metar_data"><code>get_metar_data()</code></a>.
-- @class table
-- @name WIND_DIRECTION
-- @field VRB Index value for variable speed direction
-- @field N Index value for North
-- @field NNE Index value for North - North East
-- @field NE Index value for Nort - East
-- @field ENE Index value for East - North East
-- @field E Index value for East
-- @field ESE Index value for East - South East
-- @field SE Index value for South East
-- @field SSE Index value for South - South Est
-- @field S Index value for South
-- @field SSW Index value for South - South West
-- @field SW Index value for South West
-- @field WSW Index value for West - South West
-- @field W  Index value for West
-- @field WNW Index value for West - North West
-- @field NW Index value for North West
-- @field NNW Index value for North - North West
WIND_DIRECTION      = { VRB = 1 }

-------------------------------------------------------------------------------
-- Could coverage table.
-- Values from this table are used in the result table with key <code>clouds[n].coverage</code> returned by
-- <a href="#metatable.__index:get_metar_data"><code>get_metar_data()</code></a>.
-- @class table
-- @name CLOUD_COVERAGE
-- @field CLEAR Clear
-- @field FEW Few clouds
-- @field SCATTERED Scattered clouds
-- @field BROKEN_SKY Broken sky
-- @field OVERCAST Overcast
CLOUD_COVERAGE      = {}

-------------------------------------------------------------------------------
-- Could type table.
-- Values from this table are used in the result table with key <code>clouds[n].type</code> returned by
-- <a href="#metatable.__index:get_metar_data"><code>get_metar_data()</code></a>.
-- @class table
-- @name CLOUD_TYPE
-- @field CUMULONIMBUS Cumulonimbus clouds
-- @field TOWERING_CUMULUS Towering Cumulus clouds
CLOUD_TYPE          = {}

-------------------------------------------------------------------------------
-- Could type table.
-- Values from this table are used in the result table with key <code>sky</code> returned by
-- <a href="#metatable.__index:get_metar_data"><code>get_metar_data()</code></a>.
-- @class table
-- @name SKY_STATUS
-- @field UNKNOWN Sky type is unknown
-- @field OBSCURE Obscured sky
-- @field CLOUDS Clouds in the sky
-- @field CLEAR Clear sky
-- @field NO_SIGNIFICANT_CLOUDS No significant clouds detected
-- @field NO_CLOUDS_DETECTED No clouds detected
SKY_STATUS          = { UNKNOWN = 1, OBSCURE = 2, CLOUDS = 3}

-------------------------------------------------------------------------------
-- Weather intensity table.
-- Values from this table are used in the result table with key <code>weather.intensity</code> returned by
-- <a href="#metatable.__index:get_metar_data"><code>get_metar_data()</code></a>.
-- @class table
-- @name WEATHER_INTENSITY
-- @field MODERATE Moderate phenomena
-- @field LIGHT Light phenomena
-- @field HEAVY Heavy phenomena
-- @field VICINITY In the vicinity of the weather observation point
WEATHER_INTENSITY   = { MODERATE = 1 }

-------------------------------------------------------------------------------
-- Weather descriptor table.
-- Values from this table are used in the result table with key <code>weather.descriptor</code> returned by
-- <a href="#metatable.__index:get_metar_data"><code>get_metar_data()</code></a>.
-- @class table
-- @name WEATHER_DESCRIPTOR
-- @field SHALLOW Shallow phenomena
-- @field PARTIAL Partial phenomena
-- @field PATCHES Patches phenomena
-- @field DRIFTING Drifring phenomena
-- @field BLOWING Blowing phenomena
-- @field SHOWERS Showers phenomena
-- @field THUNDERSTORM Thunderstorm phenomena
-- @field FREEZING Freezing phenomena
WEATHER_DESCRIPTOR   = {}

-------------------------------------------------------------------------------
-- Weather phenomena table.
-- Values from this table are used in the result table with key <code>weather.phenomena</code> returned by
-- <a href="#metatable.__index:get_metar_data"><code>get_metar_data()</code></a>.
-- @class table
-- @name WEATHER_PHENOMENA
-- @field DRIZZLE Drizzle
-- @field RAIN Rain
-- @field SNOW Snow
-- @field SNOW_GRAINS Snow grains
-- @field ICE_CRYSTALS Ice crystals
-- @field ICE_PELLETS Ice pellets
-- @field HAIL Hail
-- @field SMALL_HAIL Small hail
-- @field UNKNOWN Unknown phenomena
-- @field MIST Mist
-- @field FOG Fog
-- @field SMOKE Smoke
-- @field VOLCANIC_ASH Volcanic ash
-- @field WIDESPREAD_DUST Widespread dust
-- @field SAND Sand
-- @field HAZE Haze
-- @field SPRAY Spray
-- @field DUST_WHIRLS Dust whirls
-- @field SQUALLS Squalls
-- @field FUNNEL_CLOUD Funnel cloud
-- @field SAND_STORM Sand storm
-- @field DUST_STORM Dust storm
WEATHER_PHENOMENA   = {}

-- Fill the constant tables
for i, key in pairs(token_data.wind.directions) do
	WIND_DIRECTION[key.key] = i + token_data.wind.offset
end
for i, key in pairs(token_data.clouds.coverages) do
	CLOUD_COVERAGE[key.key] = i
end
for i, key in pairs(token_data.clouds.types) do
	CLOUD_TYPE[key.key] = i
end
for i, key in pairs(token_data.sky.types) do
	SKY_STATUS[key.key] = i + token_data.sky.offset
end
for i, key in pairs(token_data.weather_intensity.types) do
	WEATHER_INTENSITY[key.key] = i + token_data.weather_intensity.offset
end
for i, key in pairs(token_data.weather_descriptor.types) do
	WEATHER_DESCRIPTOR[key.key] = i
end
for i, key in pairs(token_data.weather_phenomena.types) do
	WEATHER_PHENOMENA[key.key] = i
end

local function parse_metar_date(day, hour, min)
	if not day or not hour or not min then
		return
	end
	day  = tonumber(day)
	hour = tonumber(hour)
	min  = tonumber(min)
	local now = os.date('!*t')
	if day > now.day and now.day == 1 then
		now.month = _get_previous_month(now)
		now.day = _get_last_day_of_month(now)
		if now.month == 12 then
			now.year = now.year - 1
		end
	else
		now.day = day
	end
	now.hour = hour
	now.min  = min
	now.sec  = 0
	return { timestamp = os.time(now) }
end

-- The parse_* routines parse snippets of METAR data

local function parse_metar_wind(dir, speed, gust, unit)
	local direction
	if gust and token_data.wind.speeds[gust] then
			unit = gust
			gust = nil
	end
	if not dir or not speed or not unit or not token_data.wind.speeds[unit] then
		return
	end
	if dir:match('^%d%d%d$') then
		dir = tonumber(dir)
		for _, test_direction in pairs(token_data.wind.directions) do
			if (
							(test_direction.min > test_direction.max)
						and
							(test_direction.min <= dir or dir <= test_direction.max)
					or
							(test_direction.max > test_direction.min)
						and
							(test_direction.min <= dir and dir <= test_direction.max)
					) then
				direction = WIND_DIRECTION[test_direction.key]
				break
			end
		end
	else
		direction = WIND_DIRECTION[dir]
	end
	if not direction then
		return
	end
	local data = { direction = direction, speed = weatherlib.convert_speed(token_data.wind.speeds[unit], weatherlib.SPEED_UNITS.KNOT, tonumber(speed)) }
	if gust ~= nil then
		data['gust'] = weatherlib.convert_speed(token_data.wind.speeds[unit], weatherlib.SPEED_UNITS.KNOT, tonumber(gust))
	end
	return { wind = data }
end

local function parse_metar_visibility(visibility, direction)
	if visibility == 'CAVOK' then
		visibility = 9999
	end
	local data = { distance = tonumber(visibility) }
	if direction and direction:len() > 0 then
		if WIND_DIRECTION[direction] then
			data['direction'] = WIND_DIRECTION[direction]
		else
			return
		end
	end
	return { visibility = data }
end

local function parse_metar_runway_visual_range(runway, visibility)
	return { runway_visual_range = { runway = tonumber(runway), visibility = tonumber(visibility) } } 
end

local function parse_metar_clouds(coverage, altitude, type)
	local coverage_key
	for _, coverage_data in pairs(token_data.clouds.coverages) do
		if coverage == coverage_data.token or coverage == coverage_data.alias then
			coverage_key = coverage_data.key
			break
		end
	end
	if not coverage_key then
		return
	end
	if not CLOUD_COVERAGE[coverage_key] then
		return
	end
	local data = { coverage = CLOUD_COVERAGE[coverage_key], altitude = tonumber(altitude) }
	if type and type:len() > 0 then
		local type_key
		for _, type_data in pairs(token_data.clouds.types) do
			if type == type_data.token then
				type_key = type_data.key
				break
			end
		end
		if not type_key then
			return
		end
		if not CLOUD_TYPE[type_key] then
			return
		end
		data['type'] = CLOUD_TYPE[type_key]
	end
	return { clouds = data }
end

local function parse_metar_vertical_visibility(visibility)
	if visibility and visibility:find('^%d%d%d$') then
		visibility = tonumber(visibility)
	elseif visibility == '///' then
		visibility = 0
	else
		return
	end
	return { vertical_visibility = visibility }
end

local function parse_metar_sky(status)
	local sky_key
	for _, sky_data in pairs(token_data.sky.types) do
		if status == sky_data.token or status == sky_data.alias then
			sky_key = sky_data.key
			break
		end
	end
	if not sky_key then
		return
	end
	if not SKY_STATUS[sky_key] then
		return
	end
	return { sky = SKY_STATUS[sky_key] }
end

local function parse_metar_weather(intensity, descriptor, phenomena)
	if not phenomena or phenomena:len() == 0 then
		phenomena = descriptor
		descriptor = nil
	end
	local intensity_key, descriptor_key, phenomena_key
	if not intensity or intensity:len() == 0 then
		intensity = WEATHER_INTENSITY.MODERATE
	else
		for _, intensity_data in pairs(token_data.weather_intensity.types) do
			if intensity == intensity_data.token then
				intensity_key = intensity_data.key
				break
			end
		end
		if not intensity_key or not WEATHER_INTENSITY[intensity_key] then
			return
		end
		intensity = WEATHER_INTENSITY[intensity_key]
	end
	if descriptor and descriptor:len() > 0 then
		for _, descriptor_data in pairs(token_data.weather_descriptor.types) do
			if descriptor == descriptor_data.token then
				descriptor_key = descriptor_data.key
				break
			end
		end
		if not descriptor_key or not WEATHER_DESCRIPTOR[descriptor_key] then
			return
		end
		descriptor = WEATHER_DESCRIPTOR[descriptor_key]
	end
	if not phenomena or phenomena:len() == 0 then
		return
	end
	for _, phenomena_data in pairs(token_data.weather_phenomena.types) do
		if phenomena == phenomena_data.token or phenomena == phenomena_data.alias then
			phenomena_key = phenomena_data.key
			break
		end
	end
	if not phenomena_key or not WEATHER_PHENOMENA[phenomena_key] then
		return
	end
	phenomena = WEATHER_PHENOMENA[phenomena_key]
	local weather_data = { intensity = intensity, phenomena = phenomena }
	if descriptor then
		weather_data['descriptor'] = descriptor
	end
	return { weather = weather_data }
end

local function parse_metar_temperature(temperature, dewpoint)
	temperature = temperature:gsub('^M', '-')
	dewpoint = dewpoint:gsub('^M', '-')
	return { temperature = tonumber(temperature), dewpoint = tonumber(dewpoint) }
end

local function parse_metar_pressure(unit, pressure)
	if unit == 'A' then
		pressure = weatherlib.convert.pressure[weatherlib.PRESSURE_UNITS.INHG][weatherlib.PRESSURE_UNITS.HPA](tonumber(pressure) / 100)
	else
		pressure = tonumber(pressure)
	end
	return { pressure = pressure }
end

-- Define patterns for each snippet of METAR data to parse
local metar_token_handlers = {
	{
		pattern = '^(%d%d)(%d%d)(%d%d)Z$',
		handler = parse_metar_date,
	},
	{
		pattern = '^(%w%w%w)(%d+)G(%d+)([A-Z]+)$',
		handler = parse_metar_wind,
	},
	{
		pattern = '^(%w%w%w)(%d+)([A-Z]+)$',
		handler = parse_metar_wind,
	},
	{
		pattern = '^(%d%d%d%d)([A-Z]*)$',
		handler = parse_metar_visibility,
	},
	{
		pattern = '^(CAVOK)$',
		handler = parse_metar_visibility,
	},
	{
		pattern = '^R(%d+)/(%d+)$',
		handler = parse_metar_runway_visual_range,
	},
	{
		pattern = '^([A-Z][A-Z][A-Z])(%d%d%d)([A-Z]*)$',
		handler = parse_metar_clouds,
	},
	{
		pattern = '^VV([%d/][%d/][%d/])$',
		handler = parse_metar_vertical_visibility,
	},
	{
		pattern = '^([A-Z]+)$',
		handler = parse_metar_sky,
	},
	{
		pattern = '^([+-]*)([A-Z][A-Z])$',
		handler = parse_metar_weather,
	},
	{
		pattern = '^([+-]*)([A-Z][A-Z])([A-Z][A-Z])$',
		handler = parse_metar_weather,
	},
	{
		pattern = '^(VC)([A-Z][A-Z])*$',
		handler = parse_metar_weather,
	},
	{
		pattern = '^(VC)([A-Z][A-Z])([A-Z][A-Z])*$',
		handler = parse_metar_weather,
	},
	{
		pattern = '^(M*%d+)/(M*%d+)$',
		handler = parse_metar_temperature,
	},
	{
		pattern = '^([QA])(%d+)$',
		handler = parse_metar_pressure,
	},
}

local function _get_previous_month(date)
	if date.month == 1 then
		return 12
	end
	return date.month - 1
end

local function _get_last_day_of_month(date)
	date.day  = 1
	date.hour = 0
	date.min  = 0
	date.sec  = 0
	date.wday = 0
	date.yday = 0
	local day_seconds = 60 * 60 * 24
	local timestamp = os.time(date)
	local test_date = date
	-- count days forward until month changes
	while(date.month == test_date.month) do
		timestamp = os.time(test_date) + day_seconds
		test_date = os.date('*t', timestamp)
	end
	-- rewind one day to get the last day of the month
	timestamp = timestamp - day_seconds
	return os.date('*t', timestamp).day
end

local metatable = { __index = {} }

-------------------------------------------------------------------------------
-- Create a new METAR object
-- @param args String that is either the METAR data string (one line) to parse
-- or the four-letter, upper-case <a href="http://en.wikipedia.org/wiki/International_Civil_Aviation_Organization_airport_code">ICAO code</a>
-- for the weather station. If weather station code is given, the current
-- METAR data for the station is downloaded from <a href="http://weather.noaa.gov">IWS</a>.
-- @return A table which is the metar object for METAR data given or
-- downloaded from IWS for the given weather station code
function new(args)
	local attributes = {}
	if args and type(args) == 'string' then
		if args:find('^[A-Z][A-Z][A-Z][A-Z]$') then
			attributes.station_id = args
		else
			attributes.metar_string = args
		end
	end
	return setmetatable(attributes, metatable)
end

-------------------------------------------------------------------------------
-- Return parsed METAR data as a table
-- @return Table containing the data parsed from the METAR data. 
-- If an error occurs, returns nil as the first return value.
-- The table may contain following entries
-- <ul>
-- <li><code>timestamp</code> <code>os.time</code> table which represents the timestamp when the METAR data was generated. Time is in UTC. Always included.</li>
-- <li><code>wind</code> A table representing the wind phenomena with the following keys. Optional, but usually included.</li>
--     <ul>
--     <li><code>direction</code> Wind direction as a value of the <a href="#WIND_DIRECTION">WIND_DIRECTION</a> table.</li>
--     <li><code>speed</code> Wind speed in knots.</li>
--     <li><code>gust</code> Gust speed in knots, optional.</li>
--     </ul>
-- <li><code>visibility</code> A list of tables that represent the visibility towards different directions. Tables contain the following keys. Optional, but if defined, at least one visibility entry exists in the list. Usually included.</li>
--     <ul>
--     <li><code>direction</code> Direction as a value of the <a href="#WIND_DIRECTION">WIND_DIRECTION</a> table. Optional.</li>
--     <li><code>distance</code> Visibility distance in meters</li>
--     </ul>
-- <li><code>vertical_visibility</code> Vertical visibility in meters. Optional.</li>
-- <li><code>runway_visual_range</code> A table representing runway visual range with the following keys. Optional.</li>
--     <ul>
--     <li><code>runway</code> Runway code</li>
--     <li><code>visibility</code> Visibility in meters</li>
--     </ul>
-- <li><code>clouds</code> A list of tables that represent clouds at different altitudes. Tables contain the following keys. Optional, but if defined, at least one cloud entry exists in the list. Usually included.</li>
--     <ul>
--     <li><code>coverage</code> Cloud coverate as a value of the <a href="#CLOUD_COVERAGE">CLOUD_COVERAGE</a> table.</li>
--     <li><code>altitude</code> Altitude of the clouds in feet.</li>
--     <li><code>type</code> Cloud type as a value of the <a href="#CLOUD_TYPE">CLOUD_TYPE</a> table.</li>
--     </ul>
-- <li><code>weather</code> A table representing weather conditions with the following keys. Optional, but usually included.</li>
--     <ul>
--     <li><code>intensity</code> Weather intensity as a value of the <a href="#WEATHER_INTENSITY">WEATHER_INTENSITY</a> table. Optional.</li></li>
--     <li><code>descriptor</code> Weather descriptor as a value of the <a href="#WEATHER_DESCRIPTOR">WEATHER_DESCRIPTOR</a> table. Optional.</li>
--     <li><code>phenomena</code> Weather phenomena as a value of the <a href="#WEATHER_PHENOMENA">WEATHER_PHENOMENA</a> table. Always included.</li>
--     </ul>
-- <li><code>sky</code> Sky status as a value of the <a href="#SKY_STATUS">SKY_STATUS</a> table. Always included.</li>
-- <li><code>temperature</code> Temperature in Celcius. Always  included.</li>
-- <li><code>dewpoint</code> Dewpoint temperature in Celcius. Always included.</li>
-- <li><code>pressure</code> Pressure in hectopascals. Optional, but usually included.</li>
--</ul>
-- @return Error string in case an error occurred and nil METAR table is returned
-- @usage var m = metar.new('EFHF')          -- Weather station Helsinki/Malmi
-- @usage var md = m:get_metar_data()        -- metardata.temperature contains the temperature etc.
-- @usage if md.temperature >= 30 then print("It's hot!") end
-- @usage if md.weather.intensity and md.weather.intensity == m.WEATHER_INTENSITY.HEAVY and md.weather.phenomena and md.weather.phenomena == m.WEATHER_PHENOMENA.RAIN then print("It's raining a lot!") end
function metatable.__index:get_metar_data()
	if self.station_id then
		local metar_string, error_string = self:_fetch_metar_string(self.station_id)
		if not metar_string then
			return nil, error_string
		end
		self.metar_string = metar_string
	end
	if not self.metar_string then
		return nil, 'No METAR string or station id'
	end
	local metar_data = self:_parse_metar_string(self.metar_string)
	if self.station_id then
		metar_data['station'] = self.station_id
	end
	metar_data['metar_string'] = self.metar_string
	return metar_data
end

function metatable.__index:_parse_metar_string(metar_string)
	local multi_entry = { visibility = 1, runway_visual_range = 1, clouds = 1 }
	local metar_data  = {}
	local token_data
	-- tokenize METAR data
	metar_string:gsub('%S+', function(token)
			local data = {}
			-- test each parser against the token and
			-- if it matches, pass to the handler
			for _, token_handler in pairs(metar_token_handlers) do
				token:gsub(token_handler.pattern,
					function(...)
						local value = token_handler.handler(...)
						table.insert(data, value)
					end)
			end
			-- save results
			for _, data_entry in pairs(data) do
				if data_entry then
					for key, value in pairs(data_entry) do
						if multi_entry[key] then
							if not metar_data[key] then
								metar_data[key] = {}
							end
							table.insert(metar_data[key], value)
						else
						metar_data[key] = value
						end
					end
				end
			end
	end)
	-- METAR data parsed, do some post processing
	if metar_data.clouds then
		metar_data.sky = SKY_STATUS.CLOUDS
	elseif metar_data.vertical_visibility then
		metar_data.sky = SKY_STATUS.OBSCURE
	end
	if not metar_data.sky then
		metar_data.sky = SKY_STATUS.UNKNOWN
	end
	return metar_data
end

-- Download and validate METAR string for a weather station
function metatable.__index:_fetch_metar_string(station_id)
	local body, error_string = self:_download_metar_file(station_id)
	if not body then
		return nil, error_string
	end
	local metar_string
	local metar_pattern = string.format('^%s%%s+%%d%%d%%d%%d%%d%%dZ%%s+', self.station_id)
	body:gsub('(.-)\n', function (line)
			if line:find(metar_pattern) then
				metar_string = line
				return
			end
	end)
	if metar_string then
		return metar_string
	else
		return nil, 'Failed to find METAR string from input'
	end
end

-- Download the METAR data for a weather station from IWS
function metatable.__index:_download_metar_file(station_id)
	local metar_url = string.format('http://weather.noaa.gov/pub/data/observations/metar/stations/%s.TXT', station_id)
	local body, status, _, status_string = socket_http.request(metar_url)
	local error_details
	if not status or (type(status) ~= 'number' and type(status) ~= 'string') then
		error_details = 'Unknown error'
	elseif type(status) == 'string' then
		error_details = status
	elseif type(status) == 'number' and status ~= 200 then
		error_details = status_string
	end
	if error_details then
		local error_string = string.format('Failed to fetch METAR data for station %s: %s', self.station_id, error_details)
		self:log_error(error_string)
		return nil, error_string
	end
	return body
end

function metatable.__index:log_error(error_string)
	print(string.format('[metar] %s', error_string))
end
