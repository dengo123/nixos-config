-- features/shell/menu/dialogs/power/init.lua
-- Zentrale Verkabelung:
--  - Theme wird HIER via Theme.get(...) aufgelöst
--  - Base/Layout arbeiten nur mit einem bereits gemergten Theme-Objekt
--  - Popup bleibt theme-neutral (nur Lifecycle)

local Theme = require("features.shell.menu.dialogs.power.theme")
local Widgets = require("features.shell.menu.dialogs.parts.widgets")
local Popup = require("features.shell.menu.dialogs.parts.popup")

-- Base: kann Factory oder fertiges Modul sein
local BaseMod = require("features.shell.menu.dialogs.power.base")
local Base = (type(BaseMod) == "function") and BaseMod(Theme, Widgets, Popup) or BaseMod

-- Layouts: können Factory oder fertiges Modul sein
local LayoutsMod = require("features.shell.menu.dialogs.power.layouts")
local Layouts = (type(LayoutsMod) == "function")
		and LayoutsMod({ Theme = Theme, W = Widgets, Popup = Popup, Base = Base })
	or LayoutsMod

-- Hilfswrapper: Theme zentral auflösen und an das Layout weiterreichen
local function with_theme(layout_fn, name_for_error)
	return function(overrides)
		if type(layout_fn) ~= "function" then
			error(("power module does not export %s"):format(name_for_error or "the requested layout"))
		end
		local th = Theme.get(overrides or {})
		return layout_fn(th)
	end
end

-- Öffentliche API
return {
	-- Dialoge
	power = with_theme(Layouts.power, "power"),
	logout = with_theme(Layouts.logout, "logout"),

	-- Nützliche Re-Exporte
	theme = Theme,
	widgets = Widgets,
	popup = Popup,
	base = Base,
}
