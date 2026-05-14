-- ~/.config/awesome/system/session_state/app_restore.lua
local awful = require("awful")
local gears = require("gears")

local M = {}

local runtime = {
	cfg = {},
	store = nil,
	started = false,
	marker_path = nil,
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function cfg()
	return runtime.cfg or {}
end

local function store()
	return runtime.store
end

local function session_state_cfg()
	return ((cfg() or {}).system or {}).session_state or {}
end

local function restore_cfg()
	return session_state_cfg().restore or {}
end

local function restore_on_start_enabled()
	return restore_cfg().on_start ~= false
end

local function lower(value)
	return string.lower(tostring(value or ""))
end

local function shquote(s)
	s = tostring(s or "")
	return "'" .. s:gsub("'", "'\"'\"'") .. "'"
end

local function marker_path()
	if type(runtime.marker_path) == "string" and runtime.marker_path ~= "" then
		return runtime.marker_path
	end

	return gears.filesystem.get_cache_dir() .. "session-app-restore-marker.lua"
end

local function write_marker(data)
	local path = marker_path()
	local file = io.open(path, "w")
	if not file then
		return false
	end

	file:write("return {\n")
	file:write("\tskip_once = " .. tostring(data and data.skip_once == true) .. ",\n")
	file:write("}\n")
	file:close()

	return true
end

local function read_marker()
	local path = marker_path()
	local chunk = loadfile(path)
	if not chunk then
		return nil
	end

	local ok, data = pcall(chunk)
	if not ok or type(data) ~= "table" then
		return nil
	end

	return data
end

local function clear_marker()
	local path = marker_path()
	os.remove(path)
end

local function should_skip_this_start()
	local marker = read_marker()
	if not marker then
		return false
	end

	local skip = (marker.skip_once == true)
	clear_marker()

	return skip
end

local function spawn_later(cmd, delay)
	delay = tonumber(delay) or 2.0
	awful.spawn.with_shell("(sleep " .. tostring(delay) .. "; " .. cmd .. ") &")
end

local function spawn_once_later(cmd, match_pattern, delay)
	delay = tonumber(delay) or 2.0

	awful.spawn.with_shell(
		"(sleep "
			.. tostring(delay)
			.. "; pgrep -af "
			.. shquote(match_pattern)
			.. " >/dev/null 2>&1 || "
			.. cmd
			.. ") &"
	)
end

local function read_state()
	local Store = store()

	if not (Store and type(Store.read) == "function") then
		return nil
	end

	local data = Store.read()
	if type(data) ~= "table" then
		return nil
	end

	return data
end

local function rule_matches(rule, client_state)
	if type(rule) ~= "table" or type(client_state) ~= "table" then
		return false
	end

	if type(rule.match) == "function" then
		local ok, matched = pcall(rule.match, client_state)
		return ok and matched == true
	end

	if rule.class and lower(client_state.class) ~= lower(rule.class) then
		return false
	end

	if rule.instance and lower(client_state.instance) ~= lower(rule.instance) then
		return false
	end

	if rule.role and lower(client_state.role) ~= lower(rule.role) then
		return false
	end

	if rule.type and lower(client_state.type) ~= lower(rule.type) then
		return false
	end

	if rule.class or rule.instance or rule.role or rule.type then
		return true
	end

	return false
end

local function rule_matches_client(rule, c)
	if type(rule) ~= "table" or not (c and c.valid) then
		return false
	end

	if type(rule.match_client) == "function" then
		local ok, matched = pcall(rule.match_client, c)
		return ok and matched == true
	end

	if type(rule.match) == "function" then
		local ok, matched = pcall(rule.match, {
			class = c.class,
			instance = c.instance,
			role = c.role,
			type = c.type,
			name = c.name,
			pid = c.pid,
			startup_id = c.startup_id,
		})
		return ok and matched == true
	end

	if rule.class and lower(c.class) ~= lower(rule.class) then
		return false
	end

	if rule.instance and lower(c.instance) ~= lower(rule.instance) then
		return false
	end

	if rule.role and lower(c.role) ~= lower(rule.role) then
		return false
	end

	if rule.type and lower(c.type) ~= lower(rule.type) then
		return false
	end

	if rule.class or rule.instance or rule.role or rule.type then
		return true
	end

	return false
end

local function normalize_mode(value)
	value = lower(value)

	if value == "per_client" then
		return "per_client"
	end

	return "once"
end

local function app_rules()
	if type(M.apps) == "function" then
		return M.apps() or {}
	end

	return {}
end

local function current_matching_clients(rule)
	local count = 0

	for _, c in ipairs(client.get()) do
		if rule_matches_client(rule, c) then
			count = count + 1
		end
	end

	return count
end

local function build_plan(data)
	local out = {}
	local rules = app_rules()
	local clients = data.clients or {}

	for name, rule in pairs(rules) do
		if type(rule) == "table" and type(rule.cmd) == "string" and rule.cmd ~= "" then
			local wanted = 0

			for _, client_state in ipairs(clients) do
				if rule_matches(rule, client_state) then
					wanted = wanted + 1
				end
			end

			if wanted > 0 then
				local mode = normalize_mode(rule.mode)
				local existing = current_matching_clients(rule)
				local missing = 0

				if mode == "per_client" then
					missing = math.max(0, wanted - existing)
				else
					missing = (existing > 0) and 0 or 1
				end

				if missing > 0 then
					table.insert(out, {
						name = tostring(name),
						cmd = rule.cmd,
						count = missing,
						mode = mode,
						delay = tonumber(rule.delay) or 2.5,
						stagger = tonumber(rule.stagger) or 0.35,
						match_pattern = rule.match_pattern,
					})
				end
			end
		end
	end

	table.sort(out, function(a, b)
		return a.name < b.name
	end)

	return out
end

local function run_plan(plan)
	for _, item in ipairs(plan or {}) do
		if item.mode == "per_client" then
			for i = 1, item.count do
				spawn_later(item.cmd, item.delay + ((i - 1) * item.stagger))
			end
		else
			if type(item.match_pattern) == "string" and item.match_pattern ~= "" then
				spawn_once_later(item.cmd, item.match_pattern, item.delay)
			else
				spawn_later(item.cmd, item.delay)
			end
		end
	end
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	args = args or {}

	runtime.cfg = args.cfg or runtime.cfg or {}
	runtime.store = args.store or runtime.store
	runtime.marker_path = args.marker_path or runtime.marker_path

	return M
end

function M.note_exit(is_restart)
	if is_restart == true then
		write_marker({
			skip_once = true,
		})
	else
		clear_marker()
	end
end

function M.run()
	if runtime.started then
		return false
	end

	runtime.started = true

	if not restore_on_start_enabled() then
		return false
	end

	if should_skip_this_start() then
		return false
	end

	local data = read_state()
	if not data then
		return false
	end

	run_plan(build_plan(data))
	return true
end

function M.apps()
	return {
		firefox = {
			class = "firefox",
			cmd = "firefox",
			mode = "per_client",
			delay = 2.4,
			match_pattern = "[f]irefox",
		},

		emacsclient = {
			match = function(cs)
				local class = lower(cs.class)
				local instance = lower(cs.instance)
				return class == "emacs" or instance == "emacs"
			end,
			cmd = "emacsclient -c -a ''",
			mode = "per_client",
			delay = 2.8,
			stagger = 0.35,
			match_pattern = "emacsclient",
		},
	}
end

return M
