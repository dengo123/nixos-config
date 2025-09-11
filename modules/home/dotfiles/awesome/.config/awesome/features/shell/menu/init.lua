-- features/shell/menu/init.lua
local core = require("features.shell.menu.core")
local defaults = require("features.shell.menu.data")

local M = {}

-- interner Helfer: Theme auflösen (Funktion | Tabelle | ui.theme Fallback)
local function resolve_theme(theme)
	if type(theme) == "function" then
		theme = theme()
	end
	if type(theme) ~= "table" then
		local ok, tmod = pcall(require, "ui.theme")
		if ok and type(tmod.startmenu) == "function" then
			theme = tmod.startmenu()
		elseif ok and type(tmod) == "table" then
			theme = tmod
		else
			theme = {
				bg = "#222222",
				fg = "#ffffff",
				border_color = "#000000",
				border_width = 1,
				total_height = 500,
			}
		end
	end
	return theme
end

-- interner Helfer: Daten mergen
local function merge_data(src)
	src = src or {}
	local function pick(k)
		return (src[k] ~= nil) and src[k] or defaults[k]
	end
	return {
		user = pick("user"),
		left_items = pick("left_items"),
		right_items = pick("right_items"),
		power_items = pick("power_items"),
	}
end

-- Low-level: nur Popup/API bauen
function M.create(opts)
	opts = opts or {}
	local theme = resolve_theme(opts.theme)
	local data = merge_data(opts.data)

	local build_popup = (type(core) == "function") and core or core.build_popup
	assert(type(build_popup) == "function", "menu.core export mismatch: expected function 'build_popup'")

	local api = build_popup({
		theme = theme,
		data = data,
		cfg = opts.cfg, -- zur Weitergabe falls core es nutzt
	})

	-- launcher kann später ergänzt werden; API ist Hauptsache
	return { menu = api, launcher = nil }
end

-- High-level: vollständiges Menü für die App initialisieren
-- gibt {menu, launcher} zurück, sodass shell/init nichts wissen muss
function M.setup(cfg)
	return M.create({
		cfg = cfg,
		theme = cfg and cfg.menu_theme, -- optional: Theme via cfg übergeben
		data = cfg and cfg.menu_data, -- optional: Data via cfg übergeben
	})
end

return M
