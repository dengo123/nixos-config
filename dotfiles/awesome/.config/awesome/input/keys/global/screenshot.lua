-- in input/keys/global/screenshot.lua
local awful = require("awful")
local gears = require("gears")
local naughty = require("naughty")

return function(modkey)
	local SAVE_DIR = os.getenv("HOME") .. "/Pictures/screenshots"
	local function ts()
		return os.date("%Y-%m-%d_%H-%M-%S") .. ".png"
	end
	local function q(s)
		return "'" .. s:gsub("'", "'\\''") .. "'"
	end

	awful.spawn.with_shell("mkdir -p " .. q(SAVE_DIR))

	local function notify_err(msg)
		naughty.notify({ title = "Screenshot fehlgeschlagen", text = msg, preset = naughty.config.presets.critical })
	end

	local function ensure(cmd, cb)
		awful.spawn.easy_async_with_shell("command -v " .. cmd, function(_, _, _, code)
			if code ~= 0 then
				notify_err(cmd .. " nicht gefunden")
				return
			end
			cb()
		end)
	end

	return gears.table.join(
		-- Vollbild
		awful.key({}, "Print", function()
			local file = SAVE_DIR .. "/" .. ts()
			awful.spawn.easy_async({ "bash", "-lc", "maim -u " .. q(file) }, function(_, _, _, code)
				if code == 0 then
					naughty.notify({ title = "Screenshot gespeichert", text = file })
				else
					notify_err("maim fehlgeschlagen (Vollbild)")
				end
			end)
		end, { description = "Screenshot (Vollbild)", group = "screenshot" }),

		-- Bereich (Mod + Print)
		awful.key({ modkey }, "Print", function()
			local file = SAVE_DIR .. "/" .. ts()
			awful.spawn.easy_async({ "bash", "-lc", "maim -u -s " .. q(file) }, function(_, _, _, code)
				if code == 0 then
					naughty.notify({ title = "Screenshot gespeichert", text = file })
				else
					notify_err("maim/slop fehlgeschlagen (Bereich)")
				end
			end)
		end, { description = "Screenshot (Bereich)", group = "screenshot" }),

		-- Aktives Fenster (Shift + Print)
		awful.key({ "Shift" }, "Print", function()
			ensure("xdotool", function()
				local file = SAVE_DIR .. "/" .. ts()
				local cmd = 'WID=$(xdotool getactivewindow) && [ -n "$WID" ] && maim -u -i "$WID" ' .. q(file)
				awful.spawn.easy_async({ "bash", "-lc", cmd }, function(_, _, _, code)
					if code == 0 then
						naughty.notify({ title = "Screenshot gespeichert", text = file })
					else
						notify_err("Fenster-Screenshot fehlgeschlagen (xdotool/maim)")
					end
				end)
			end)
		end, { description = "Screenshot (Fenster)", group = "screenshot" }),

		-- In Zwischenablage (Ctrl + Print)
		awful.key({ "Control" }, "Print", function()
			ensure("xclip", function()
				awful.spawn.easy_async(
					{ "bash", "-lc", "maim -u -s | xclip -selection clipboard -t image/png" },
					function(_, _, _, code)
						if code == 0 then
							naughty.notify({ title = "Screenshot", text = "In Zwischenablage kopiert" })
						else
							notify_err("Clipboard fehlgeschlagen (xclip)")
						end
					end
				)
			end)
		end, { description = "Screenshot → Clipboard", group = "screenshot" })
	)
end
