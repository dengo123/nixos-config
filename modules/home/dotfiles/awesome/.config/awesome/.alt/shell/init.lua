-- features/shell/init.lua
-- Orchestrator: verbindet menu, model, view

local menu = require("features.shell.menu") -- falls du eins hast
local model = require("features.shell.model")
local view = require("features.shell.view")

local M = {}

-- init(cfg): bau Menü/Launcher (optional)
function M.init(cfg)
	local mw = {}
	if menu and menu.create then
		mw = menu.create({ cfg = cfg })
	end
	return {
		menu = mw.menu,
		launcher = mw.launcher,
	}
end

-- setup(s, opts): baut Sections & rendert Wibar
-- opts: { cfg, launcher, keyboardlayout, systray=true, position, height }
function M.setup(s, opts)
	opts = opts or {}
	local sections = model.build(s, opts) -- -> {left, center, right}
	view.apply(s, sections, opts) -- Wibar zeichnen
end

return M
