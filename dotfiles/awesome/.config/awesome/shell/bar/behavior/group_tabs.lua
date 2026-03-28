-- ~/.config/awesome/shell/bar/behavior/group_tabs.lua
local M = {}

local runtime = {
	cfg = {},
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function cfg()
	return runtime.cfg or {}
end

local function bar_cfg()
	return cfg().bar or {}
end

local function tabs_cfg()
	return bar_cfg().tabs or {}
end

local function group_tabs_cfg()
	local tabs = tabs_cfg()
	local group = tabs.group or {}

	return {
		enable = (group.enable == true),
		by = tostring(group.by or "class"):lower(),
	}
end

local function visible_clients_on_tag(s)
	local t = s and s.selected_tag
	if not t then
		return {}
	end

	local out = {}

	for _, c in ipairs(t:clients() or {}) do
		if c and c.valid and not c.skip_taskbar and c.screen == s then
			table.insert(out, c)
		end
	end

	return out
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

local function group_key_for_client(c, mode)
	if not (c and c.valid) then
		return nil
	end

	if mode == "instance" then
		return first_string(c.instance, c.class, c.name)
	end

	if mode == "name" then
		return first_string(c.name, c.class, c.instance)
	end

	return first_string(c.class, c.instance, c.name)
end

local function group_label_for_client(c, mode)
	if not (c and c.valid) then
		return "App"
	end

	if mode == "name" then
		return first_string(c.name, c.class, c.instance, "App")
	end

	if mode == "instance" then
		return first_string(c.instance, c.class, c.name, "App")
	end

	return first_string(c.class, c.name, c.instance, "App")
end

local function build_single_entries(clients)
	local out = {}

	for _, c in ipairs(clients or {}) do
		table.insert(out, {
			label = first_string(c.name, c.class, c.instance, "App"),
			clients = { c },
		})
	end

	return out
end

local function build_grouped_entries(clients, mode)
	local grouped = {}
	local order = {}

	for _, c in ipairs(clients or {}) do
		local key = group_key_for_client(c, mode) or tostring(c.window or c)
		local bucket = grouped[key]

		if not bucket then
			bucket = {
				label = group_label_for_client(c, mode),
				clients = {},
			}
			grouped[key] = bucket
			table.insert(order, key)
		end

		table.insert(bucket.clients, c)
	end

	local out = {}

	for _, key in ipairs(order) do
		table.insert(out, grouped[key])
	end

	return out
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(opts)
	opts = opts or {}
	runtime.cfg = opts.cfg or runtime.cfg
	return M
end

function M.is_enabled(opts)
	local conf = (opts and opts.cfg) or cfg()
	local tabs = (conf.bar or {}).tabs or {}
	local group = tabs.group or {}

	return group.enable == true
end

function M.collect(s, opts)
	opts = opts or {}

	local conf = opts.cfg or cfg()
	local tabs = (conf.bar or {}).tabs or {}
	local group = tabs.group or {}
	local mode = tostring(group.by or "class"):lower()

	local clients = visible_clients_on_tag(s)

	if group.enable ~= true then
		return build_single_entries(clients)
	end

	return build_grouped_entries(clients, mode)
end

function M.collect_single(s)
	return build_single_entries(visible_clients_on_tag(s))
end

function M.collect_grouped(s, mode)
	return build_grouped_entries(visible_clients_on_tag(s), tostring(mode or group_tabs_cfg().by or "class"):lower())
end

return M
