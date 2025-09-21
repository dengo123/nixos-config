local awful = require("awful")
local gears = require("gears")

local MRU = {
	max = 8,
	pinned = {}, -- { {text, cmd, icon}, ... }
	list = {}, -- recent
	path = gears.filesystem.get_cache_dir() .. "menu_recent.txt",
}

local function norm_cmd(cmd)
	-- fürs Matching: nur binärname (ohne args)
	if not cmd then
		return ""
	end
	local first = cmd:match("^%S+")
	return first or cmd
end

local function save(self)
	local f = io.open(self.path, "w")
	if not f then
		return
	end
	for _, e in ipairs(self.list) do
		f:write((e.text or ""), "|", (e.cmd or ""), "|", (e.icon or ""), "\n")
	end
	f:close()
end

local function load(self)
	local f = io.open(self.path, "r")
	if not f then
		return
	end
	for line in f:lines() do
		local t, c, i = line:match("^(.-)|(.-)|(.*)$")
		if c and c ~= "" then
			table.insert(self.list, { text = t, cmd = c, icon = i })
		end
	end
	f:close()
end

function MRU:set_max(n)
	self.max = tonumber(n) or self.max
end

function MRU:set_pinned(tbl)
	self.pinned = tbl or {}
end

local function push(self, entry)
	local key = norm_cmd(entry.cmd)
	for i = #self.list, 1, -1 do
		if norm_cmd(self.list[i].cmd) == key then
			table.remove(self.list, i)
		end
	end
	table.insert(self.list, 1, entry)
	while #self.list > self.max do
		table.remove(self.list)
	end
	save(self)
end

function MRU:add(entry)
	push(self, entry)
end

function MRU:get(mode)
	mode = mode or "mixed" -- "pinned" | "recent" | "mixed"
	if mode == "pinned" then
		return gears.table.clone(self.pinned, true)
	elseif mode == "recent" then
		return gears.table.clone(self.list, true)
	else
		local out = {}
		for _, v in ipairs(self.pinned) do
			table.insert(out, v)
		end
		for _, v in ipairs(self.list) do
			table.insert(out, v)
		end
		return out
	end
end

function MRU:spawn(item)
	if item.cmd and item.cmd ~= "" then
		awful.spawn(item.cmd)
		self:add({ text = item.text, cmd = item.cmd, icon = item.icon })
	end
end

load(MRU)
return MRU
