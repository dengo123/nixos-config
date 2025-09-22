-- features/shell/menu/dialogs/power/init.lua
-- Verkabelt: /power/{theme,base,layouts}.lua + /parts/{widgets,popup}.lua

local Theme = require("features.shell.menu.dialogs.power.theme")
local Widgets = require("features.shell.menu.dialogs.parts.widgets")
local Popup = require("features.shell.menu.dialogs.parts.popup")

-- Base kann entweder eine Factory (Theme, Widgets, Popup) sein ODER bereits ein fertiges Modul
local BaseMod = require("features.shell.menu.dialogs.power.base")
local Base = (type(BaseMod) == "function") and BaseMod(Theme, Widgets, Popup) or BaseMod

-- Layouts können entweder Funktionen erwarten (power(theme_overrides))
-- oder eine Factory sein, der man Abhängigkeiten übergibt. Wir unterstützen beides.
local LayoutsMod = require("features.shell.menu.dialogs.power.layouts")
local Layouts = (type(LayoutsMod) == "function")
		and LayoutsMod({ Theme = Theme, W = Widgets, Popup = Popup, Base = Base })
	or LayoutsMod

-- Öffentliche API dieses Pakets:
--  - power(theme_overrides)
--  - logout_confirm(theme_overrides)
--  - plus nützliche Referenzen auf die Bausteine
return {
	power = function(overrides)
		return Layouts.power(overrides)
	end,
	logout_confirm = function(overrides)
		return Layouts.logout(overrides)
	end,

	-- Exporte für gezielte Weiterverwendung
	theme = Theme,
	widgets = Widgets,
	popup = Popup,
	base = Base,
}
