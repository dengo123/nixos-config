-- ~/.config/awesome/input/keys/global/apps.lua
local awful = require("awful")

return function(modkey, cfg)
	local term, launcher, browser, files = cfg.terminal, cfg.launcher, cfg.browser, cfg.files
	local emacs_client = (cfg.emacs and cfg.emacs.client) or { "emacsclient", "-c", "-a", "" }

	local function spawn_cmd(cmd)
		if type(cmd) == "table" then
			awful.spawn(cmd)
		elseif type(cmd) == "string" then
			if cmd:find("||") or cmd:find("~") then
				awful.spawn.with_shell(cmd)
			else
				awful.spawn(cmd)
			end
		end
	end

	local function first_token(x)
		if type(x) == "table" then
			return x[1]
		elseif type(x) == "string" then
			return x:match("^(%S+)")
		end
	end

	local function cmd_exists(x)
		local exe = first_token(x)
		if not exe or #exe == 0 then
			return false
		end
		local h = io.popen("command -v " .. exe .. ' >/dev/null 2>&1; printf %s "$?"')
		if not h then
			return false
		end
		local rc = h:read("*a")
		h:close()
		return rc == "0"
	end

	local function open_primary()
		if cmd_exists(term) then
			spawn_cmd(term)
		else
			-- kein Terminal installiert → Emacs-Client
			spawn_cmd(emacs_client)
		end
	end

	return awful.util.table.join(
		awful.key(
			{ modkey },
			"Return",
			open_primary,
			{ description = "open terminal or emacs (auto)", group = "launcher" }
		),

		awful.key({ modkey }, "space", function()
			spawn_cmd(launcher)
		end, { description = "launcher/menu", group = "launcher" }),

		awful.key({ modkey }, "b", function()
			spawn_cmd(browser)
		end, { description = "browser", group = "launcher" }),

		awful.key({ modkey }, "e", function()
			spawn_cmd(files)
		end, { description = "file manager", group = "launcher" }),

		awful.key({ modkey }, "Print", function()
			spawn_cmd("screenshot")
		end, { description = "screenshot", group = "launcher" })
	)
end
