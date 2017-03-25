-------------------------------------------------------------------------------
--
-- IMAP widget for Awesome 3.4
-- Copyright (C) 2011 Tuomas Jormola <tj@solitudo.net>
--
-- Licensed under the terms of GNU General Public License Version 2.0.
--
-- Description:
--
-- Displays status of IMAP mailboxes by showing the amount of unread mails
-- in each configured mailbox. When the mouse cursor is hovering over
-- the widget, summary of unread mails in each mailbox is displayed
-- (sender and subject). When new unread mails are discovered,
-- a notification is displayed with summary of the new messages.
-- Also displays an icon indicating whether there are unread messages
-- in one of the monitored mailboxes. Clicking the icon launches
-- an external application (if configured).
--
-- Widget uses Vicious widget framework to gather widget data.
--
-- This widget uses imap.lua by David Maus <dmaus@ictsoc.de>
-- http://github.com/dmj/misc/tree/master/lua/imap.lua/
--
-- Widget tries to use icons from the package gnome-icon-theme
-- if available.
--
--
-- Configuration:
--
-- The load() function expects to get the IMAP server configuration as
-- the 1st argument. You can specify as many IMAP server as you wish
-- with each polling arbitrary amount of folders.
--
-- Format of the configuration is as follows.
-- {
-- -- User name of the IMAP account, mandatory
--        user               = 'exampleuser1',
-- -- Password of the IMAP account, mandatory
--        password           = 'examplepassword1',
-- -- Host name or IP address of the IMAP server, mandatory
--        host               = 'mail1.example.com',
-- -- Port of the IMAP server. 143 by default if SSL disabled,
-- -- 993 if SSL enabled.
--        port               = 993,
-- -- Enable SSL for the IMAP connection.
-- -- Note that only IMAPS is supported, not IMAP with STARTTLS.
--        ssl                = true,
-- -- Mailboxes to poll. INBOX is used by default.
--        mailboxes          = { 'INBOX', 'folder1', 'folder2' },
-- -- Show summary for this many unread mails per mailbox in the summary
-- -- popup, default is 5
--        show_mail_coun     = 3,
-- -- Program that is launched when the user clicks on the widget area.
-- -- Empty by default.
--        command            = 'evolution -c mail',
-- -- Don't try to display any icons. Default is false (i.e. display icons).
--        no_icon            = true,
-- -- Poll interval in seconds, 5 minutes by default
--        update_interval    = 3600,
-- -- How long to show notifications in seconds, 10 seconds by default
--        notification_delay = 5,
-- },
-- -- Another example, minimal configuration using exampleuser2 as user,
-- -- examplepassword2 as password, mail2.example.com as IMAP server,
-- -- 143 as port, no SSL, polling INBOX, no program is run when clicking
-- -- the widget, 5 minutes polling interval, show notifications for 10
-- -- seconds and show 5 unread mails per mailbox in the summary
-- {
--        user         = 'exampleuser2',
--        password     = 'examplepassword2',
--        host         = 'mail1.example.com,
-- }
--
--
-- Theme:
--
-- The widget uses following icons and fonts if available in the Awesome theme.
--
-- theme.delightful_imap_mail_read   - icon shown when mail in a mailbox is read
-- theme.delightful_imap_mail_unread - icon shown when unread mail in a mailbox
-- theme.delightful_error            - icon shown when critical error has occurred
-- theme.monospace_font              - font for status text and notifications
--
-------------------------------------------------------------------------------

local awful_button      = require('awful.button')
local awful_util        = require('awful.util')
local beautiful         = require('beautiful')
local image             = require('image')
local naughty           = require('naughty')
local widget            = require('widget')

local delightful_utils  = require('delightful.utils')
local vicious           = require('vicious')

local imap              = require('imap')

local capi              = { mouse = mouse }

local pairs             = pairs
local setmetatable      = setmetatable
local string            = { format = string.format }
local table             = { insert = table.insert, remove = table.remove, sort = table.sort }
local tostring          = tostring
local type              = type

module('delightful.widgets.imap')

local widgets           = {}
local icons             = {}
local icon_files        = {}
local prev_icons        = {}
local imap_config       = {}
local imap_data         = {}
local notification_data = {}

local config_description = {
	{
		name     = 'user',
		required = true,
		validate = function(value) return delightful_utils.config_string(value) end
	},
	{
		name     = 'password',
		required = true,
		validate = function(value) return delightful_utils.config_string(value) end
	},
	{
		name     = 'host',
		required = true,
		validate = function(value) return delightful_utils.config_string(value) end
	},
	{
		name     = 'ssl',
		required = true,
		default  = false,
		validate = function(value) return delightful_utils.config_boolean(value) end
	},
	{
		name     = 'ssl_string',
		required = true,
		default  = function(config_data) if config_data.ssl then return 'sslv3' else return 'none' end end,
		validate = function(value) if not value or (value ~= 'sslv3' and value ~= 'none') then return false, 'needs to be either "sslv3" or "none"' else return true end end
	},
	{
		name     = 'port',
		required = true,
		default  = function(config_data) if config_data.ssl then return 993 else return 143 end end,
		validate = function(value) return delightful_utils.config_int(value) end
	},
	{
		name     = 'mailboxes',
		required = true,
		default  = 'INBOX',
		coerce   = function(value) return delightful_utils.coerce_table(value) end,
		validate = function(value) return delightful_utils.config_table(value) end
	},
	{
		name     = 'show_mail_count',
		required = true,
		default  = 5,
		validate = function(value) return delightful_utils.config_int(value) end
	},
	{
		name     = 'command',
		default  = function(config_data) if mailer_cmd then return mailer_cmd end end,
		validate = function(value) return delightful_utils.config_string(value) end
	},
	{
		name     = 'no_icon',
		validate = function(value) return delightful_utils.config_boolean(value) end
	},
	{
		name     = 'update_interval',
		required = true,
		default  = 5 * 60,
		validate = function(value) return delightful_utils.config_int(value) end
	},
	{
		name     = 'notification_delay',
		required = true,
		default  = 10,
		validate = function(value) return delightful_utils.config_int(value) end
	},
	-- User is not supposed to supply configuration of these settings
	{
		name     = 'font',
		required = true,
		default  = function(config_data) return beautiful.monospace_font or 'monospace' end,
		validate = function(value) return delightful_utils.config_string(value) end,
	},
}

local icon_description = {
	read   = { beautiful_name = 'delightful_imap_mail_read',   default_icon = function() return 'stock_mail-open' end   },
	unread = { beautiful_name = 'delightful_imap_mail_unread', default_icon = function() return 'stock_mail-unread' end },
	error  = { beautiful_name = 'delightful_error',            default_icon = function() return 'error' end             },
}

-- Poll the mailbox
function update_data(imap_index)
	if not imap_data[imap_index] or not imap_data[imap_index].connection or not imap_config[imap_index].mailboxes then
		return
	end
	local connection = imap_data[imap_index].connection
	imap_data[imap_index].unread_total  = 0
	imap_data[imap_index].status_string = ' '
	for mailbox_index, mailbox in pairs(imap_config[imap_index].mailboxes) do
		local mailbox_url = string.format('%s/%s', imap_url(imap_index), mailbox)
		imap_data[imap_index].mailboxes[mailbox_index].messages = nil
		connection.mailbox = mailbox

		local total, unread_num, imap_error
		total, imap_error = connection:total()
		if total then
			unread_num, imap_result = connection:unread()
			if unread_num then
				imap_data[imap_index].mailboxes[mailbox_index].total = total
				imap_data[imap_index].mailboxes[mailbox_index].unread = unread_num
				imap_data[imap_index].unread_total = imap_data[imap_index].unread_total + unread_num
				if unread_num > 0 then
					local unread_messages
					imap_error, unread_messages = connection:fetch(false, true, false)
					if unread_messages then
						local unread_message_ids = {}
						for unread_message_id in pairs(unread_messages) do
							table.insert(unread_message_ids, unread_message_id)
						end
						table.sort(unread_message_ids)
						imap_data[imap_index].mailboxes[mailbox_index].messages = {}
						for unread_message_index, unread_message_id in pairs(awful_util.table.reverse(unread_message_ids)) do
							imap_data[imap_index].mailboxes[mailbox_index].messages[unread_message_index] =
									unread_messages[unread_message_id]
							imap_data[imap_index].mailboxes[mailbox_index].messages[unread_message_index].uid =
									unread_message_id
						end
						if not imap_data[imap_index].mailboxes[mailbox_index].latest_message
								or imap_data[imap_index].mailboxes[mailbox_index].latest_message ~= imap_data[imap_index].mailboxes[mailbox_index].messages[1].uid then
							local n = 1
							while imap_data[imap_index].mailboxes[mailbox_index].latest_message
									and imap_data[imap_index].mailboxes[mailbox_index].messages[n]
									and imap_data[imap_index].mailboxes[mailbox_index].messages[n].uid ~= imap_data[imap_index].mailboxes[mailbox_index].latest_message do
								if not notification_data[imap_index] then
									notification_data[imap_index] = {}
								end
								if not notification_data[imap_index][mailbox_index] then
									notification_data[imap_index][mailbox_index] = {}
								end
								table.insert(notification_data[imap_index][mailbox_index],
										imap_data[imap_index].mailboxes[mailbox_index].messages[n])
								n = n + 1
							end
							imap_data[imap_index].mailboxes[mailbox_index].latest_message =
									imap_data[imap_index].mailboxes[mailbox_index].messages[1].uid
							imap_data[imap_index].mailboxes[mailbox_index].error_string = nil
						end
					else
						imap_data[imap_index].mailboxes[mailbox_index].error_string =
								string.format('Failed to fetch unread messages in %s: %s',
										mailbox_url, imap_error)
					end
				end
			else
				imap_data[imap_index].mailboxes[mailbox_index].error_string =
						string.format('Failed to check the number of unread messages in %s: %s',
								mailbox_url, imap_error)
			end
		else
			imap_data[imap_index].mailboxes[mailbox_index].error_string =
					string.format('Failed to check the total number of messages in %s: %s',
							mailbox_url, imap_error)
		end
		local mailbox_status = '-'
		if imap_data[imap_index].mailboxes[mailbox_index].error_string then
			mailbox_status = string.format('<span color="red">%s</span>', mailbox_status);
		elseif unread_num then
			mailbox_status = tostring(unread_num)
		end
		imap_data[imap_index].status_string =
				string.format('%s%s', imap_data[imap_index].status_string,
						mailbox_status);
		if mailbox_index < #imap_config[imap_index].mailboxes then
			imap_data[imap_index].status_string =
					string.format('%s ', imap_data[imap_index].status_string);
		end
	end
end

-- Update widget icon based on the IMAP status data
function update_icon(imap_index)
	if not icon_files.read or not icon_files.unread or not icon_files.error then
		return
	end
	if not imap_index or not icons[imap_index] or not imap_data[imap_index] then
		return
	end
	if not imap_data[imap_index].unread_total and not imap_data[imap_index].error_string then
		return
	end
	local icon_file
	if imap_data[imap_index].unread_total then
		icon_file = icon_files.read 
		if imap_data[imap_index].unread_total > 0 then
			icon_file = icon_files.unread 
		end
	end
	if imap_data[imap_index].error_string then
		icon_file = icon_files.error 
	end
	if icon_file and
			(not prev_icons[imap_index] or prev_icons[imap_index] ~= icon_file) then
		prev_icons[imap_index] = icon_file
		icons[imap_index].image = image(icon_file)
	end
end

-- Text for the hover notification
function summary_text(imap_index)
	local text = ''
	if not imap_index or not imap_data[imap_index] then
		return text
	end
	if imap_data[imap_index].error_string then
		text = imap_data[imap_index].error_string
	else
		for mailbox_index, mailbox in pairs(imap_data[imap_index].mailboxes) do
			text = string.format('%s<span font_style="italic">%s</span>', text, mailbox.name)
			if mailbox.error_string then
				text = string.format('%s\n  <span color="red">%s</span>\n',
						text, mailbox.error_string
				)
			else
				text = string.format('%s, <span font_weight="bold">%d</span> unread, <span font_weight="bold">%d</span> total\n',
						text, mailbox.unread, mailbox.total
				)
				if mailbox.messages then
					for message_count, message in pairs(mailbox.messages) do
						text = string.format('%s  %s <span font_weight="bold">%s</span>\n',
								text,
								pad_message_detail(message.from),
								pad_message_detail(message.subject)
						)
						if message_count >= imap_config[imap_index].show_mail_count then
							total_message_count = #mailbox.messages
							if(total_message_count > message_count) then
								text = string.format('%s  ... and <span font_weight="bold">%d</span> more\n',
										text,
										total_message_count - message_count
								)
							end
							break
						end
					end
				end
			end
			text = string.format('%s\n', text)
		end
	end
	return text:gsub('\n*$', '')
end

-- Notification of new messages
function show_notifications(imap_index)
	if not imap_index or
			not notification_data[imap_index] or
			not imap_data[imap_index] or
			not imap_data[imap_index].mailboxes then
		return
	end
	local text = string.format('<span font_weight="bold">%s</span>\n', imap_url(imap_index))
	for mailbox_index, notifications in pairs(notification_data[imap_index]) do
		local mailbox_name = imap_data[imap_index].mailboxes[mailbox_index].name
		text = string.format('%s<span font_style="italic">%s</span>, <span font_weight="bold">%d</span> new\n', text, mailbox_name, #notifications)
		while notifications[1] do
			local message = table.remove(notifications, 1)
			text = string.format('%s  %s <span font_weight="bold">%s</span>\n',
					text,
					pad_message_detail(message.from),
					pad_message_detail(message.subject)
			)
		end
		if mailbox_index < #notification_data[imap_index] then
			text = string.format('%s\n', text)
		end
	end
	notification_data[imap_index] = nil
	naughty.notify({
			text    = text,
			icon    = icon_files.unread,
			font    = imap_config[imap_index].font or 'monospace',
			timeout = imap_config[imap_index].notification_delay,
			screen  = capi.mouse.screen
	})
end

-- Configuration handler
function handle_config(user_config)
	local empty_config = delightful_utils.get_empty_config(config_description)
	if not user_config or #user_config == 0 then
		table.insert(imap_data,   { error_string = 'No IMAP configuration' })
		table.insert(imap_config, empty_config)
		return
	end
	for imap_index, user_config_data in pairs(user_config) do
		imap_data[imap_index] = {}
		local config_data = delightful_utils.normalize_config(user_config_data, config_description)
		local validation_errors = delightful_utils.validate_config(config_data, config_description)
		if validation_errors then
			imap_data[imap_index].error_string =
					string.format('Configuration errors:\n%s',
							delightful_utils.format_validation_errors(validation_errors))
			imap_config[imap_index] = empty_config
			return
		end
		imap_config[imap_index] = config_data

		-- check that connection to the IMAP server works
		local imap_error
		local connection = imap.new(imap_config[imap_index].host,
				imap_config[imap_index].port,
				imap_config[imap_index].ssl_string
		)
		_, imap_error = connection:connect()
		if imap_error then
			imap_data[imap_index].error_string =
					string.format('Failed to connect to %s: %s',
							imap_url(imap_config[imap_index]), imap_error)
			return
		end
		_, imap_error = connection:login(imap_config[imap_index].user, imap_config[imap_index].password)
		if imap_error then
			imap_data[imap_index].error_string =
					string.format('Failed to login to %s as user %s: %s',
							imap_url(imap_config[imap_index]), imap_config[imap_index].user, imap_error)
			return
		end
		imap_data[imap_index].connection = connection
		imap_data[imap_index].mailboxes  = {}
		for mailbox_index, mailbox_name in pairs(imap_config[imap_index].mailboxes) do
			imap_data[imap_index].mailboxes[mailbox_index] = { name = mailbox_name }
		end
	end
end

-- Initalization
function load(self, config)
	handle_config(config)
	icon_files = delightful_utils.find_icon_files(icon_description)
	for imap_index, data in pairs(imap_data) do
		local icon
		if not imap_config[imap_index].no_icon and icon_files.read and icon_files.unread and icon_files.error then
			icon = widget({ type = 'imagebox', name = 'imap_' .. imap_index })
		end

		local popup_enter = function()
			local popup_title
			if data.error_string then
				popup_title = 'Error'
			else
				popup_title = imap_url(imap_index)
			end
			data.popup = naughty.notify({
					title   = popup_title,
					text    = summary_text(imap_index),
					font    = imap_config[imap_index].font or 'monospace',
					timeout = imap_config[imap_index].notification_delay,
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

		if imap_config[imap_index].command then
			local buttons = awful_button({}, 1, function()
					awful_util.spawn(imap_config[imap_index].command, true)
			end)
			widget:buttons(buttons)
			if icon then
				icon:buttons(buttons)
			end
		end

		widgets[imap_index] = widget
		icons[imap_index]   = icon

		vicious.register(widget, self, '$1', imap_config[imap_index].update_interval, imap_index)
	end
	return widgets, icons
end

-- Vicious worker function
function vicious_worker(format, imap_index)
	update_data(imap_index)
	update_icon(imap_index)
	show_notifications(imap_index)
	local status
	local error_status = '<span color="red">'
	if icons[imap_index] then
		error_status = string.format('%s ', error_status);
	end
	error_status = string.format('%s!</span>', error_status);
	if not imap_data[imap_index] then
		status = error_status
		delightful_utils.print_error('imap', string.format('No imap_data[%d]', imap_index))
	else
		if imap_data[imap_index].error_string then
			status = '<span color="red"> !</span>';
			delightful_utils.print_error('imap', imap_data[imap_index].error_string)
		elseif imap_data[imap_index].status_string then
			status = imap_data[imap_index].status_string
		else
			imap_data[imap_index].error_string = string.format('No imap_data[%s][status_string] or imap_data[%s][error_string]', imap_index, imap_index)
			status = '<span color="red"> !</span>';
			delightful_utils.print_error('imap', imap_data[imap_index].error_string)
		end
	end
	if imap_data[imap_index].mailboxes then
		for _, mailbox in pairs(imap_data[imap_index].mailboxes) do
			if mailbox.error_string then
				delightful_utils.print_error('imap', mailbox.error_string)
			end
		end
	end
	return status
end

-- Helpers

function imap_url(data)
	if type(data) == 'number' then
		data = imap_config[data]
	end
	if not data then
		return
	end
	if not data.host or not data.port or not data.ssl then
		return
	end
	local url = 'imap'
	if data.ssl then
		url = string.format('%ss', url)
	end
	url = string.format('%s://%s', url, data.host)
	if (data.ssl and data.port ~= 993) or
			(not data.ssl and data.port ~= 143) then
		url = string.format('%s:%s', url, data.port);
	end
	return url
end

function pad_message_detail(line)
	return delightful_utils.pad_string_with_spaces(line, 48)
end

setmetatable(_M, { __call = function(_, ...) return vicious_worker(...) end })
