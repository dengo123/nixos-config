-- ~/.config/awesome/shell/workspaces/init.lua
local awful = require("awful")
local tags = require("shell.workspaces.tags")
local layouts = require("shell.workspaces.layouts")

local M = {}

function M.init(cfg)
	cfg = cfg or {}

	-- Defaults
	local opts = {
		ensure_one_tag = (cfg.ensure_one_tag ~= false),
		renumber_on_start = (cfg.renumber_on_start ~= false),
		auto_adapt_layout_on_rotation = (cfg.auto_adapt_layout_on_rotation ~= false),
		desktop_deco_fn = cfg.desktop_deco_fn,
	}

	-- Layouts global setzen
	if layouts and layouts.apply then
		layouts.apply()
	end

	-- Optional: einmalig alle Wallpapers anwenden (Hooking passiert in ui/wallpaper.lua)
	local ui_ok, ui = pcall(require, "ui")
	if ui_ok and ui.wallpaper and type(ui.wallpaper.apply_all) == "function" then
		ui.wallpaper.apply_all()
	end

	-- Optional: Policy-bezogene Signals
	if tags and tags.attach_policy_signals then
		tags.attach_policy_signals()
	end

	-- Pro Screen Initialisierung
	awful.screen.connect_for_each_screen(function(s)
		if opts.ensure_one_tag and tags and tags.ensure then
			tags.ensure(s)
		end
		if opts.renumber_on_start and tags and tags.renumber then
			tags.renumber(s)
		end
		if tags and tags.apply_layout_policy then
			tags.apply_layout_policy(s)
		end
		if opts.desktop_deco_fn then
			opts.desktop_deco_fn(s)
		end
	end)

	-- Automatisch auf Rotation reagieren
	if opts.auto_adapt_layout_on_rotation and tags and tags.on_screen_rotation then
		tags.on_screen_rotation()
	end
end

return M
