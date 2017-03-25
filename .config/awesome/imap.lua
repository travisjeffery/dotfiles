--- Simple IMAP client library.
--
-- Copyright 2009 by David Maus <maus.david@gmail.com>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.
--
-- @release $Revision: 0.2 $
--
--

local socket = require("socket")
local ssl = require("ssl")

local os = os
local pairs = pairs
local print = print
local setmetatable = setmetatable
local table = table

module("imap")

imap = {}
imap.__index = imap

--- Default values.
PORT = 993
SSL = "sslv3"
MAILBOX = "Inbox"
TIMEOUT = 5

--- Create and return new imap client object.
-- @param server Hostname or ip address of remote server
-- @param port Port to connect to (Default: 993)
-- @param ssl_proto SSL/TLS protocol to use (Default: "sslv3")
-- @param mailbox Mailbox to check (Default: Inbox)
-- @param timeout Timeout value for TCP connection (Default: 5s)
-- @return A shiny new imap client control object
function new(server, port, ssl_proto, mailbox, timeout)

   _imap = {}
   setmetatable(_imap, imap.__index)
   _imap.server = server
   _imap.port = port or PORT
   _imap.mailbox = mailbox or MAILBOX
   if ssl_proto == "none" then
      _imap.ssl = nil
   else
      _imap.ssl = ssl_proto or SSL
   end
   _imap.timeout = timeout or TIMEOUT
   _imap.cmd_count = 0
   _imap.logged_in = false

   return _imap

end

--- Connect to remote server.
-- @return True in case of success, nil followed by the errormessage in case of failure.
function imap:connect()

   local res, msg = socket.connect(self.server, self.port)
   if not res then return nil, msg end
   self.socket = res

   if self.ssl then
      res, msg = ssl.wrap(self.socket, { mode = "client", protocol = self.ssl })
      if not res then return nil, msg end
      self.socket = res
      res, msg = self.socket:dohandshake()
      if not res then return nil, msg end
   end

   -- set socket timeout
   self.socket:settimeout(self.timeout)

   return true

end

--- Login using username and password.
-- @param user Username
-- @param pass Password
-- @return True in case of success, nil followed by the errormessage in case of failure.
function imap:login(user, pass)
   local res, msg = self:request("LOGIN " .. user .. " \"" .. pass .. "\"")
   if not res then return nil, msg end

   -- select mailbox
   local res, msg = self:request("EXAMINE " .. self.mailbox)
   if not res then return nil, msg end

   self.logged_in = true

   return true
end

--- Logout.
-- @return True in case of success, nil followed by the errormessage in case of failure.
function imap:logout()
   local res, msg = self:request("LOGOUT")
   if not res then return nil, msg end

   self.logged_in = false

   return true
end

--- Send command to server and return answer.
-- @param command Client command
-- @param unprefixed If true, don't prefix command and don't increase command counter
-- @return True in case of success, nil followed by the errormessage in case of failure.
function imap:request(command, unprefixed)

   -- check if we the socket exists, return error if not
   if not self.socket then return nil, "Not connected" end

   local prefix = ""
   if not unprefixed then
      self.cmd_count = self.cmd_count + 1
      prefix = "0x0" .. self.cmd_count .. " "
   end

   local res, msg = self.socket:send(prefix .. command .. "\r\n")
   if not res then return nil, msg end

   local answer = {}

   local res, msg = self.socket:receive("*l")
   if not res then return nil, msg end
   while not res:match(prefix) do
      table.insert(answer, res)
      res, msg = self.socket:receive("*l")
      if not res then return nil, msg end
   end

   if not res:match(prefix .. "OK ") then
      return nil, res
   end

   return true, answer
end

--- Return number of new messages in mailbox.
-- @return Number of new messages in case of success, nil followed by the errormessage in case of failure.
function imap:recent()
   
   local res, msg = self:request("EXAMINE " .. self.mailbox)
   if not res then return nil, msg end
   
   local n = 0
   local k,v
   for k,v in pairs(msg) do
      if v:match("^* %d+ RECENT") then n = v:match("^* (%d+) RECENT") end
   end

   return n

end

--- Return total number of message in mailbox.
-- @return Total number of messages in case of success, nil followed by the errormessage in case of failure.
function imap:total()

   local res, msg = self:request("EXAMINE " .. self.mailbox)
   if not res then return nil, msg end

   local n = 0
   local k,v
   for k,v in pairs(msg) do
      if v:match("^* %d+ EXISTS") then n = v:match("^* (%d+) EXISTS") end
   end

   return n

end

--- Return number of unread message in mailbox.
-- Determining the number of unread messages requires to perform a
-- SEARCH query.
-- @return Number of unread messages in case of success, nil followed by the errormessage in case of failure.
function imap:unread()

   -- perform an EXAMINE to select the mailbox
   local res, msg = self:request("SEARCH (UNSEEN)")
   if not res then return nil, msg end

   local n = 0
   local k,v
   for k,v in pairs(msg) do
      if v:match("^* SEARCH %d+") then
	 while v:find("%d+") do
	    local s, e = v:find("%d+")

	    n = n + 1
	    v = v:sub(e + 1, #v)
	    
	 end
      end
   end

   return n

end

--- Check for total number, number of unread and new messages.
-- @return Table with number of total, new and unread messages in case
-- of success, nil followed by the errormessage in case of failure.
function imap:check()

   local messages = { total = 0, unread = 0, recent = 0 }

   local res, msg = self:total()
   if not res then return nil, msg end
   messages.total = res
   
   local res, msg = self:unread()
   if not res then return nil, msg end
   messages.unread = res
   
   local res, msg = self:recent()
   if not res then return nil, msg end
   messages.recent = res

   return messages

end

--- Return information about messages in mailbox.
-- @param recent If true, return information an recent messages (Default: true)
-- @param unread If true, return information on unread messages (Default: false)
-- @param total If true, return information on all messages (Default: false)
-- @return Table with information on all messages that matched the criteria.
function imap:fetch(recent, unread, total)

   if recent == nil then recent = true end
   if unread == nil then unread = false end
   if total == nil then total = false end

   -- build a table with all search queries that we have to issue
   local query = {}
   if recent then table.insert(query, "RECENT") end
   if unread then table.insert(query, "UNSEEN") end
   if total then query = { "ALL" } end

   local messages = {}
   local _, q
   for _, q in pairs(query) do
      print ("Perform search for: " .. q)
      local res, msg = self:request("SEARCH (" .. q .. ")")
      if not res then return nil, msg end
      local k,v
      for k,v in pairs(msg) do
	 if v:match("^* SEARCH %d+") then
	    while v:find("%d+") do
	       local s, e = v:find("%d+")
	       local uid = v:sub(s, e)

	       messages[uid] = {}

	       local r,m = self:request("FETCH " .. uid .. " (FLAGS RFC822.SIZE BODY[HEADER.FIELDS (FROM SUBJECT)])")
	       if not r then return nil, m end
	       local l,w
	       for l,w in pairs(m) do
		  if w:match("RFC822.SIZE %d+") then messages[uid].size = w:match("RFC822.SIZE (%d+)") end
		  if w:match("^From:") then messages[uid].from = w:match("^From:%s+(.*)") end
		  if w:match("^Subject:") then messages[uid].subject = w:match("^Subject:%s+(.*)") end
		  if w:match("FLAGS") then
		     if w:match("\Recent") then messages[uid].recent = true end
		     if not w:match("\Seen") then messages[uid].unread = true end
		  end
	       end

	       v = v:sub(e + 1, #v)
	    end
	 end
      end
   end


   return true, messages

end
