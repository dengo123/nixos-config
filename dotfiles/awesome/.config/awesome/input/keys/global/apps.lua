-- ~/.config/awesome/input/keys/global/apps.lua
local awful = require("awful")

return function(modkey, cfg)
	local apps = cfg.apps or {}
	local input = cfg.input or {}

	local terminal = apps.terminal
	local editor = apps.editor
	local browser = apps.browser
	local files = apps.files
	local return_app = input.return_app or "terminal"

	local function spawn_cmd(cmd)
		if type(cmd) == "table" then
			awful.spawn(cmd)
		elseif type(cmd) == "string" and cmd ~= "" then
			if cmd:find("||") or cmd:find("~") then
				awful.spawn.with_shell(cmd)
			else
				awful.spawn(cmd)
			end
		end
	end

	local function primary_cmd()
		if return_app == "editor" then
			return editor
		end

		return terminal
	end

	local function secondary_cmd()
		if return_app == "editor" then
			return terminal
		end

		return editor
	end

	return awful.util.table.join(
		awful.key({ modkey }, "Return", function()
			spawn_cmd(primary_cmd())
		end, { description = "open primary app", group = "App" }),

		awful.key({ modkey, "Shift" }, "Return", function()
			spawn_cmd(secondary_cmd())
		end, { description = "open secondary app", group = "App" }),

		awful.key({ modkey }, "b", function()
			spawn_cmd(browser)
		end, { description = "Browser", group = "App" }),

		awful.key({ modkey }, "e", function()
			spawn_cmd(files)
		end, { description = "File Manager", group = "App" })
	)
end
