-- ~/.config/awesome/shell/menu/items.lua
local awful = require("awful")
local hotkeys_popup = require("awful.hotkeys_popup")

local Items = {}

function Items.build_start(ctx)
	local ui, cfg = ctx.ui, ctx.cfg
	local theme_menu = ui and ui.theme and ui.theme.menu
	local items = (theme_menu and type(theme_menu.items) == "table" and theme_menu.items)
		or (cfg and cfg.menus and type(cfg.menus.items) == "table" and cfg.menus.items)
	if items then
		return items
	end

	-- helpers
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

	-- cfg defaults
	local files_cmd = (cfg and cfg.files_cmd and #cfg.files_cmd > 0) and cfg.files_cmd or "nemo"
	local term = (cfg and cfg.terminal) or "xterm"
	local browser_cmd = (cfg and cfg.browser) or "firefox"
	local emacs_client = (cfg and cfg.emacs and cfg.emacs.client) or { "emacsclient", "-c", "-a", "" }

	-- browser: name + availability
	local browser_name = first_token(browser_cmd) or "browser"
	local browser_is_available = cmd_exists(browser_cmd)

	-- dynamic label + action: prefer terminal, fall back to emacsclient
	local term_is_available = cmd_exists(term)
	local term_label = term_is_available and ("terminal (" .. (first_token(term) or "…") .. ")") or "emacs client"

	return {
		{
			"files",
			function()
				spawn_cmd(files_cmd)
			end,
		},

		{
			term_label,
			function()
				if term_is_available then
					spawn_cmd(term)
				else
					spawn_cmd(emacs_client)
				end
			end,
		},
		{
			browser_name,
			function()
				if browser_is_available then
					spawn_cmd(browser_cmd)
				else
					-- robuster Fallback
					spawn_cmd({ "xdg-open", "about:blank" })
				end
			end,
		},
		{
			"run",
			function()
				local Launchers = require("shell.launchers")
				Launchers.open.run()
			end,
		},
		{
			"screensaver",
			function()
				awful.spawn.with_shell("xscreensaver-settings")
			end,
		},
		{
			"reload",
			function()
				awesome.restart()
			end,
		},
		{
			"hotkeys",
			function()
				hotkeys_popup.show_help(nil, awful.screen.focused())
			end,
		},
		{
			"lock",
			function()
				awful.spawn({ "dm-tool", "lock" })
			end,
		},
		{
			"power",
			function()
				local Launchers = require("shell.launchers")
				Launchers.open.power()
			end,
		},
	}
end

-- Kontextmenü für Tabs: Close / Float-Tile / Fullscreen
function Items.build_clients(clients, _ctx)
	local c = nil
	for _, cc in ipairs(clients or {}) do
		if cc and cc.valid then
			c = cc
			break
		end
	end

	if not c then
		return {}
	end

	return {
		{
			"Schließen",
			function()
				if c.valid then
					c:kill()
				end
			end,
			c.icon,
		},
		{
			"Floating / Tiling",
			function()
				if not c.valid then
					return
				end
				c.floating = not c.floating
				if not c.floating then
					-- zurück in Tile-Hierarchie
					awful.client.setslave(c)
				end
			end,
			c.icon,
		},
		{
			"Fullscreen",
			function()
				if not c.valid then
					return
				end
				c.fullscreen = not c.fullscreen
				c:raise()
			end,
			c.icon,
		},
	}
end

return Items
