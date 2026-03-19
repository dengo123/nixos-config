-- ~/.config/awesome/shell/menu/items.lua
local awful = require("awful")
local hotkeys_popup = require("awful.hotkeys_popup")

local Items = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function needs_shell(cmd)
	if type(cmd) ~= "string" then
		return false
	end

	return cmd:find("[%s][\"']")
		or cmd:find("||", 1, true)
		or cmd:find("&&", 1, true)
		or cmd:find("~", 1, true)
		or cmd:find("%$")
		or cmd:find("[<>|;&]")
end

local function spawn_cmd(cmd)
	if type(cmd) == "table" then
		awful.spawn(cmd)
		return
	end

	if type(cmd) == "string" then
		if needs_shell(cmd) then
			awful.spawn.with_shell(cmd)
		else
			awful.spawn(cmd)
		end
	end
end

local function first_token(cmd)
	if type(cmd) == "table" then
		return cmd[1]
	end

	if type(cmd) == "string" then
		return cmd:match("^(%S+)")
	end

	return nil
end

local function cmd_exists(cmd)
	local exe = first_token(cmd)
	if not exe or exe == "" then
		return false
	end

	local handle = io.popen("command -v " .. exe .. ' >/dev/null 2>&1; printf %s "$?"')
	if not handle then
		return false
	end

	local rc = handle:read("*a")
	handle:close()

	return rc == "0"
end

local function label_for(cmd, fallback)
	local token = first_token(cmd)
	if token and token ~= "" then
		return token
	end

	return fallback
end

local function resolve_label(dynamic_labels, cmd, fallback)
	if dynamic_labels then
		return label_for(cmd, fallback)
	end

	return fallback
end

-- =========================================================================
-- Public API
-- =========================================================================

function Items.build_start(ctx)
	local ui = ctx.ui or {}
	local cfg = ctx.cfg or {}

	local theme_menu = ui.theme and ui.theme.menu
	local items = (theme_menu and type(theme_menu.items) == "table" and theme_menu.items)
		or (cfg.menus and type(cfg.menus.items) == "table" and cfg.menus.items)

	if items then
		return items
	end

	local apps_cfg = cfg.apps or {}
	local menu_cfg = cfg.menu or {}
	local dynamic_labels = (menu_cfg.dynamic_labels ~= false)

	local files_cmd = apps_cfg.files or "nemo"
	local terminal_cmd = apps_cfg.terminal or "xterm"
	local editor_cmd = apps_cfg.editor or "nano"
	local browser_cmd = apps_cfg.browser or "firefox"

	local files_name = resolve_label(dynamic_labels, files_cmd, "files")
	local terminal_name = resolve_label(dynamic_labels, terminal_cmd, "terminal")
	local editor_name = resolve_label(dynamic_labels, editor_cmd, "editor")
	local browser_name = resolve_label(dynamic_labels, browser_cmd, "browser")

	local browser_available = cmd_exists(browser_cmd)

	return {
		{
			files_name,
			function()
				spawn_cmd(files_cmd)
			end,
		},
		{
			terminal_name,
			function()
				spawn_cmd(terminal_cmd)
			end,
		},
		{
			editor_name,
			function()
				spawn_cmd(editor_cmd)
			end,
		},
		{
			browser_name,
			function()
				if browser_available then
					spawn_cmd(browser_cmd)
				else
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
			"Close",
			function()
				if c.valid then
					c:kill()
				end
			end,
		},
		{
			"Floating / Tiling",
			function()
				if not c.valid then
					return
				end

				c.floating = not c.floating

				if not c.floating then
					awful.client.setslave(c)
				end
			end,
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
		},
	}
end

return Items
