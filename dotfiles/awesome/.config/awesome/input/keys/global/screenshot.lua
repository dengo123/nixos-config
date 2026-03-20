-- ~/.config/awesome/input/keys/global/screenshot.lua
local awful = require("awful")
local gears = require("gears")
local naughty = require("naughty")

return function(modkey, cfg)
	local input_cfg = (cfg and cfg.input) or {}
	local screenshot_cfg = input_cfg.screenshot or {}

	local save_dir = screenshot_cfg.save_dir or ((os.getenv("HOME") or "") .. "/Pictures/screenshots")
	local notify_cfg = (cfg and cfg.notify) or {}
	local notify_timeout = tonumber(notify_cfg.timeout) or 3

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

	local function notify_error(msg)
		naughty.notify({
			title = "Screenshot Failed",
			text = msg,
			timeout = notify_timeout,
			preset = naughty.config.presets.critical,
			skip_history = true,
		})
	end

	local function notify_saved(path)
		naughty.notify({
			title = "Screenshot Saved",
			text = path,
			timeout = notify_timeout,
			skip_history = true,
		})
	end

	local function notify_info(msg)
		naughty.notify({
			title = "Screenshot",
			text = msg,
			timeout = notify_timeout,
			skip_history = true,
		})
	end

	local function ensure_dir()
		awful.spawn.with_shell("mkdir -p " .. sh_quote(save_dir))
	end

	local function file_path()
		return save_dir .. "/" .. timestamp()
	end

	local function ensure_command(cmd, cb)
		awful.spawn.easy_async_with_shell("command -v " .. cmd, function(_, _, _, code)
			if code ~= 0 then
				notify_error(cmd .. " not found")
				return
			end

			cb()
		end)
	end

	local function run_shell_capture(cmd, on_success, on_failure)
		awful.spawn.easy_async({ "bash", "-lc", cmd }, function(_, _, _, code)
			if code == 0 then
				on_success()
				return
			end

			notify_error(on_failure)
		end)
	end

	ensure_dir()

	-- =========================================================================
	-- Public API
	-- =========================================================================

	return gears.table.join(
		awful.key({}, "Print", function()
			local file = file_path()

			run_shell_capture("maim -u " .. sh_quote(file), function()
				notify_saved(file)
			end, "maim failed (fullscreen)")
		end, { description = "Screenshot (Fullscreen)", group = "Screenshot" }),

		awful.key({ modkey }, "Print", function()
			local file = file_path()

			run_shell_capture("maim -u -s " .. sh_quote(file), function()
				notify_saved(file)
			end, "maim/slop failed (selection)")
		end, { description = "Screenshot (Selection)", group = "Screenshot" }),

		awful.key({ "Shift" }, "Print", function()
			ensure_command("xdotool", function()
				local file = file_path()
				local cmd = 'WID=$(xdotool getactivewindow) && [ -n "$WID" ] && maim -u -i "$WID" ' .. sh_quote(file)

				run_shell_capture(cmd, function()
					notify_saved(file)
				end, "window capture failed (xdotool/maim)")
			end)
		end, { description = "Screenshot (Window)", group = "Screenshot" }),

		awful.key({ "Control" }, "Print", function()
			ensure_command("xclip", function()
				run_shell_capture("maim -u -s | xclip -selection clipboard -t image/png", function()
					notify_info("Copied to clipboard")
				end, "clipboard capture failed (xclip)")
			end)
		end, { description = "Screenshot to Clipboard", group = "Screenshot" })
	)
end
