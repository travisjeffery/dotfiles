-------------------------------------------------------------------------------
--
-- Date and time widget with calendar popup display for Awesome 3.4
-- Copyright (C) 2011 Tuomas Jormola <tj@solitudo.net>
--
-- Licensed under the terms of GNU General Public License Version 2.0.
--
-- Description:
--
-- This widget displays date and time and when the mouse cursor
-- hovers over the widget, a calendar for current month is displayed.
-- You can navigate between months by using mouse scroll wheel.
--
-- Widget uses calendar2 module by Bzed.
-- http://awesome.naquadah.org/wiki/Calendar_widget#Module_for_3.4
--
--
-- Theme:
--
-- The widget uses following colors if available in the Awesome theme.
--
-- theme.fg_focus - text color of the current date in calendar
-- theme.bg_focus - background color of the current date in calendar
--
-------------------------------------------------------------------------------

local awful_widget = require('awful.widget')
local beautiful    = require('beautiful')

local calendar2    = require('calendar2')

local string       = { format = string.format }

module('delightful.widgets.datetime')

function load()
	local widget = awful_widget.textclock({ align = 'right' })
	local calendar_format = '%s'
	if beautiful.fg_focus and beautiful.bg_focus then
		calendar_format = string.format('<span color="%s" background="%s">%%s</span>',
				beautiful.fg_focus, beautiful.bg_focus)
	end
	calendar2.addCalendarToWidget(widget, calendar_format)
	return { widget } -- no icon
end
