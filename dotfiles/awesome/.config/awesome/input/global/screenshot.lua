-- ~/.config/awesome/input/global/screenshot.lua
local awful = require("awful")
local gears = require("gears")
local naughty = require("naughty")

local M = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function timestamp()
	return os.date("%Y-%m-%d_%H-%M-%S") .. ".png"
end

local function sh_quote(s)
	s = tostring(s or "")
	return "'" .. s:gsub("'", "'\\''") .. "'"
end

local function notify_error(msg, timeout)
	naughty.notify({
		title = "Screenshot Failed",
		text = msg,
		timeout = timeout,
		preset = naughty.config.presets.critical,
		skip_history = true,
	})
end

local function notify_saved(path, timeout)
	naughty.notify({
		title = "Screenshot Saved",
		text = path,
		timeout = timeout,
		skip_history = true,
	})
end

local function notify_info(msg, timeout)
	naughty.notify({
		title = "Screenshot",
		text = msg,
		timeout = timeout,
		skip_history = true,
	})
end

local function ensure_dir(save_dir)
	awful.spawn.with_shell("mkdir -p " .. sh_quote(save_dir))
end

local function file_path(save_dir)
	return save_dir .. "/" .. timestamp()
end

local function ensure_command(cmd, timeout, cb)
	awful.spawn.easy_async_with_shell("command -v " .. cmd, function(_, _, _, code)
		if code ~= 0 then
			notify_error(cmd .. " not found", timeout)
			return
		end

		cb()
	end)
end

local function run_shell_capture(cmd, timeout, on_success, on_failure)
	awful.spawn.easy_async({ "bash", "-lc", cmd }, function(_, _, _, code)
		if code == 0 then
			on_success()
			return
		end

		notify_error(on_failure, timeout)
	end)
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.build(modkey, cfg)
	cfg = cfg or {}

	local input_cfg = cfg.input or {}
	local screenshot_cfg = input_cfg.screenshot or {}

	local save_dir = screenshot_cfg.save_dir or ((os.getenv("HOME") or "") .. "/Pictures/screenshots")
	local notify_cfg = cfg.notify or {}
	local notify_timeout = tonumber(notify_cfg.timeout) or 3

	ensure_dir(save_dir)

	return gears.table.join(
		awful.key({}, "Print", function()
			local file = file_path(save_dir)

			run_shell_capture("maim -u " .. sh_quote(file), notify_timeout, function()
				notify_saved(file, notify_timeout)
			end, "maim failed (fullscreen)")
		end, {
			description = "Screenshot Fullscreen",
			group = "Screenshot",
		}),

		awful.key({ modkey }, "Print", function()
			local file = file_path(save_dir)

			run_shell_capture("maim -u -s " .. sh_quote(file), notify_timeout, function()
				notify_saved(file, notify_timeout)
			end, "maim/slop failed (selection)")
		end, {
			description = "Screenshot Selection",
			group = "Screenshot",
		}),

		awful.key({ "Shift" }, "Print", function()
			ensure_command("xdotool", notify_timeout, function()
				local file = file_path(save_dir)
				local cmd = 'WID=$(xdotool getactivewindow) && [ -n "$WID" ] && maim -u -i "$WID" ' .. sh_quote(file)

				run_shell_capture(cmd, notify_timeout, function()
					notify_saved(file, notify_timeout)
				end, "window capture failed (xdotool/maim)")
			end)
		end, {
			description = "Screenshot Window",
			group = "Screenshot",
		}),

		awful.key({ "Control" }, "Print", function()
			ensure_command("xclip", notify_timeout, function()
				run_shell_capture("maim -u -s | xclip -selection clipboard -t image/png", notify_timeout, function()
					notify_info("Copied To Clipboard", notify_timeout)
				end, "clipboard capture failed (xclip)")
			end)
		end, {
			description = "Screenshot To Clipboard",
			group = "Screenshot",
		})
	)
end

return function(modkey, cfg)
	return M.build(modkey, cfg)
end
