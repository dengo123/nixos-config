-- ~/.config/awesome/shell/menu/lib/items.lua
-- Quelle der Wahrheit für Menüinhalte (Start + Tabs)
local awful = require("awful")
local hotkeys_popup = require("awful.hotkeys_popup")

local M = {}

-- Start-Menü: baue die Liste aus deinem Kontext (ui/cfg/dialogs).
-- Du kannst hier natürlich deine eigene Logik/Order/Labels hinterlegen.
function M.build_start(ctx)
	local cfg = ctx and ctx.cfg or {}
	local dialogs = ctx and ctx.dialogs or {}

	-- Befehle (wenn du GAR KEINE Fallbacks willst, trage hier feste Strings ein)
	local launcher_cmd = (type(cfg.launcher) == "string" and #cfg.launcher > 0) and cfg.launcher or "rofi -show drun"
	local files_cmd = (cfg.files_cmd and #cfg.files_cmd > 0) and cfg.files_cmd or "nemo"

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
				if dialogs and dialogs.launcher then
					dialogs.launcher()
				else
					awful.spawn.with_shell(launcher_cmd)
				end
			end,
		},
		{
			"power",
			function()
				if dialogs and dialogs.power then
					dialogs.power()
				end
			end,
		},
	}
end

-- Tabs-Menü: alle Clients der Gruppe
function M.build_clients(clients, _ctx)
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

return M
