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

local function mk_item(label, fn_or_submenu, icon)
	return { label, fn_or_submenu, icon }
end

local function client_actions(cfg)
	return ((((cfg or {}).actions or {}).windowing or {}).clients or {})
end

local function layout_state_label(cfg)
	local actions = client_actions(cfg)

	if type(actions.layout_state_mode) == "function" and actions.layout_state_mode(cfg) == "maximized" then
		return "Maximize / Restore"
	end

	return "Floating / Tiling"
end

local function toggle_layout_state(c, cfg)
	local actions = client_actions(cfg)

	if type(actions.toggle_layout_state) == "function" then
		actions.toggle_layout_state(c, cfg)
		return
	end

	awful.client.floating.toggle(c)

	if not c.floating then
		awful.client.setslave(c)
	end

	c:raise()
end

local function applications_submenu(ctx)
	local Apps = ctx.api and ctx.api.applications
	if not Apps then
		return {
			mk_item("Unavailable", function() end, nil),
		}
	end

	local items = (type(Apps.items) == "function" and Apps.items()) or {}
	local loaded = (type(Apps.is_loaded) == "function" and Apps.is_loaded()) or false

	if #items > 0 then
		return items
	end

	if not loaded then
		return {
			mk_item("Loading...", function() end, nil),
		}
	end

	return {
		mk_item("No applications found", function() end, nil),
	}
end

local function has_items(items)
	return type(items) == "table" and #items > 0
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

	local files_cmd = apps_cfg.files or os.getenv("FILE_MANAGER")
	local terminal_cmd = apps_cfg.terminal or os.getenv("TERMINAL")
	local editor_cmd = apps_cfg.editor or os.getenv("EDITOR")
	local browser_cmd = apps_cfg.browser or os.getenv("BROWSER")

	local files_name = resolve_label(dynamic_labels, files_cmd, "Files")
	local terminal_name = resolve_label(dynamic_labels, terminal_cmd, "Terminal")
	local editor_name = resolve_label(dynamic_labels, editor_cmd, "Editor")
	local browser_name = resolve_label(dynamic_labels, browser_cmd, "Browser")

	local browser_available = cmd_exists(browser_cmd)
	local desktop_items = applications_submenu(ctx)

	local out = {
		mk_item(files_name, function()
			spawn_cmd(files_cmd)
		end, nil),

		mk_item(terminal_name, function()
			spawn_cmd(terminal_cmd)
		end, nil),

		mk_item(editor_name, function()
			spawn_cmd(editor_cmd)
		end, nil),

		mk_item(browser_name, function()
			if browser_available then
				spawn_cmd(browser_cmd)
			else
				spawn_cmd({ "xdg-open", "about:blank" })
			end
		end, nil),
	}

	table.insert(
		out,
		mk_item("Run", function()
			local launchers = require("shell.launchers")
			launchers.open.run()
		end, nil)
	)

	table.insert(
		out,
		mk_item("Screensaver", function()
			awful.spawn.with_shell("xscreensaver-settings")
		end, nil)
	)

	table.insert(
		out,
		mk_item("Awesome Reload", function()
			awesome.restart()
		end, nil)
	)

	table.insert(
		out,
		mk_item("Hotkeys", function()
			hotkeys_popup.show_help(nil, awful.screen.focused())
		end, nil)
	)

	table.insert(out, mk_item("Applications", desktop_items, nil))

	table.insert(
		out,
		mk_item("Log Off", function()
			local launchers = require("shell.launchers")
			launchers.open.logoff()
		end, nil)
	)

	table.insert(
		out,
		mk_item("Shut Down", function()
			local launchers = require("shell.launchers")
			launchers.open.power()
		end, nil)
	)

	return out
end

function Items.build_clients(clients, ctx)
	local cfg = (ctx and ctx.cfg) or {}
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
		mk_item("Close", function()
			if c.valid then
				c:kill()
			end
		end, nil),

		mk_item(layout_state_label(cfg), function()
			if not c.valid then
				return
			end

			toggle_layout_state(c, cfg)
		end, nil),

		mk_item("Fullscreen", function()
			if not c.valid then
				return
			end

			c.fullscreen = not c.fullscreen
			c:raise()
		end, nil),
	}
end

return Items
