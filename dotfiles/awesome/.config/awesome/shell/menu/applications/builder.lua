-- ~/.config/awesome/shell/menu/applications/builder.lua
local awful = require("awful")

local M = {}

local runtime = {
	api = {},
	cfg = {},
	ui = {},
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function api()
	return runtime.api or {}
end

local function mod(name)
	return api()[name]
end

local function normalize_exec(cmd)
	if type(cmd) ~= "string" then
		return nil
	end

	cmd = cmd:gsub("%%[%a%%]", "")
	cmd = cmd:gsub("%s+$", "")

	if cmd == "" then
		return nil
	end

	return cmd
end

local function first_string(...)
	for i = 1, select("#", ...) do
		local v = select(i, ...)
		if type(v) == "string" and v ~= "" then
			return v
		end
	end

	return nil
end

local function first_function(...)
	for i = 1, select("#", ...) do
		local v = select(i, ...)
		if type(v) == "function" then
			return v
		end
	end

	return nil
end

local function to_menu_item(entry)
	if type(entry) ~= "table" then
		return nil
	end

	local label = first_string(entry.Name, entry.name, entry.label, entry.text, entry[1])

	local action_fn = first_function(entry[2], entry.callback, entry.action)

	local exec = normalize_exec(
		first_string(
			entry.cmdline,
			entry.cmd,
			entry.Exec,
			entry.exec,
			entry.command,
			type(entry[2]) == "string" and entry[2] or nil
		)
	)

	local icon = first_string(entry.icon_path, entry.icon, entry.Icon, entry[3])

	if not label then
		return nil
	end

	if type(action_fn) == "function" then
		return {
			label,
			action_fn,
			icon,
		}
	end

	if exec then
		return {
			label,
			function()
				awful.spawn.with_shell(exec)
			end,
			icon,
		}
	end

	return nil
end

local function ensure_bucket(buckets, name)
	if type(buckets[name]) ~= "table" then
		buckets[name] = {}
	end

	return buckets[name]
end

local function sort_items(items)
	table.sort(items, function(a, b)
		return tostring(a[1] or ""):lower() < tostring(b[1] or ""):lower()
	end)
end

local function sort_groups(groups)
	table.sort(groups, function(a, b)
		return tostring(a[1] or ""):lower() < tostring(b[1] or ""):lower()
	end)
end

local function should_hide_entry(entry)
	local Overrides = mod("overrides")

	if Overrides and type(Overrides.hide) == "function" then
		return Overrides.hide(entry) == true
	end

	return false
end

local function category_for_entry(entry)
	local Overrides = mod("overrides")
	local Categories = mod("categories")

	if Overrides and type(Overrides.category) == "function" then
		local forced = Overrides.category(entry)
		if type(forced) == "string" and forced ~= "" then
			return forced
		end
	end

	if Categories and type(Categories.resolve) == "function" then
		return Categories.resolve(entry)
	end

	return "Other"
end

local function group_order()
	return {
		"Accessories",
		"Development",
		"Education",
		"Games",
		"Graphics",
		"Internet",
		"Multimedia",
		"Office",
		"Science",
		"System",
		"Terminal",
		"Other",
	}
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	args = args or {}

	runtime.api = args.api or {}
	runtime.cfg = args.cfg or {}
	runtime.ui = args.ui or {}

	return M
end

function M.build(entries)
	local buckets = {}

	for _, entry in ipairs(entries or {}) do
		if not should_hide_entry(entry) then
			local item = to_menu_item(entry)
			if item then
				local group = category_for_entry(entry)
				table.insert(ensure_bucket(buckets, group), item)
			end
		end
	end

	local groups = {}
	local seen = {}

	for _, name in ipairs(group_order()) do
		local items = buckets[name]
		if type(items) == "table" and #items > 0 then
			sort_items(items)
			table.insert(groups, { name, items, nil })
			seen[name] = true
		end
	end

	for name, items in pairs(buckets) do
		if not seen[name] and type(items) == "table" and #items > 0 then
			sort_items(items)
			table.insert(groups, { name, items, nil })
		end
	end

	sort_groups(groups)
	return groups
end

return M
