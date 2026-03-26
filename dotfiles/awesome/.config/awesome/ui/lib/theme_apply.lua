-- ~/.config/awesome/ui/lib/theme_apply.lua
local awful = require("awful")
local gears = require("gears")

local M = {}

local connected = false
local apply_pending = false

-- =========================================================================
-- Helpers
-- =========================================================================

local function shquote(s)
	s = tostring(s or "")
	return "'" .. s:gsub("'", "'\"'\"'") .. "'"
end

local function with_nix_path(cmd)
	local parts = {}

	if os.getenv("HOME") then
		parts[#parts + 1] = os.getenv("HOME") .. "/.nix-profile/bin"
	end

	if os.getenv("USER") then
		parts[#parts + 1] = "/etc/profiles/per-user/" .. os.getenv("USER") .. "/bin"
	end

	parts[#parts + 1] = "/run/current-system/sw/bin"
	parts[#parts + 1] = "$PATH"

	local path = table.concat(parts, ":")

	return "PATH=" .. shquote(path) .. "; export PATH; " .. cmd
end

local function run_script(name, log_file)
	local cmd = with_nix_path(name .. " >" .. shquote(log_file) .. " 2>&1")
	awful.spawn.with_shell(cmd)
end

local function apply_all()
	run_script("apply-gtk-theme", "/tmp/apply-gtk-theme.log")
	run_script("apply-starship-theme", "/tmp/apply-starship-theme.log")
end

local function schedule_apply()
	if apply_pending then
		return
	end

	apply_pending = true

	gears.timer.start_new(0.1, function()
		apply_pending = false
		apply_all()
		return false
	end)
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(opts)
	opts = opts or {}

	if connected then
		return M
	end

	connected = true

	awesome.connect_signal("theme::state_updated", function(_path)
		schedule_apply()
	end)

	awesome.connect_signal("theme::apply", function()
		schedule_apply()
	end)

	if opts.apply_on_init ~= false then
		schedule_apply()
	end

	return M
end

function M.apply_now()
	schedule_apply()
end

return M
