-- ~/.config/awesome/shell/menu/items.lua
local awful = require("awful")
local beautiful = require("beautiful")
local hotkeys_popup = require("awful.hotkeys_popup")

local Items = {}

-- Start-Menü (Theme > cfg > Defaults). ctx = { ui, cfg }
function Items.build_start(ctx)
	local ui, cfg = ctx.ui, ctx.cfg
	local theme_menu = ui and ui.theme and ui.theme.menu
	local items = (theme_menu and type(theme_menu.items) == "table" and theme_menu.items)
		or (cfg and cfg.menus and type(cfg.menus.items) == "table" and cfg.menus.items)
	if items then
		return items
	end

	local launcher_cmd = (cfg and type(cfg.launcher) == "string" and #cfg.launcher > 0) and cfg.launcher
		or "rofi -show drun"
	local files_cmd = (cfg and cfg.files_cmd and #cfg.files_cmd > 0) and cfg.files_cmd or "nemo"

	return {
		{
			"files",
			function()
				awful.spawn.with_shell(files_cmd)
			end,
		},
		{
			"hotkeys",
			function()
				hotkeys_popup.show_help(nil, awful.screen.focused())
			end,
		},
		{
			"launcher",
			function()
				awful.spawn.with_shell(launcher_cmd)
			end,
		},
		{
			"power",
			function()
				awesome.emit_signal("menu::power")
			end,
		},
	}
end

-- Tabs-Menü: aus Client-Liste bauen
function Items.build_clients(clients)
	local items = {}
	for _, c in ipairs(clients or {}) do
		local label = c.name or c.class or "App"
		table.insert(items, {
			label,
			function()
				if c.valid then
					c:emit_signal("request::activate", "group_menu", { raise = true })
				end
			end,
			c.icon,
		})
	end
	return items
end

return Items
