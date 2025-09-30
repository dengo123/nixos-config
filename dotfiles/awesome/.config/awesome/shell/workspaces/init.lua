-- ~/.config/awesome/shell/workspaces/init.lua
local awful = require("awful")
local tags = require("shell.workspaces.tags")
local sigs = require("shell.workspaces.screen_signals")
local layouts = require("shell.workspaces.layouts")

local M = {}

function M.init(cfg)
	cfg = cfg or {}

	-- Defaults
	local opts = {
		wallpaper_fn = cfg.wallpaper_fn or require("ui").wallpaper.set,
		ensure_one_tag = true,
		renumber_on_start = true,
		auto_adapt_layout_on_rotation = true,
		desktop_deco_fn = cfg.desktop_deco_fn,
	}

	-- Layouts global setzen
	if layouts and layouts.apply then
		layouts.apply()
	end

	-- Wallpaper-Signale
	if opts.wallpaper_fn and sigs and sigs.attach then
		sigs.attach(opts.wallpaper_fn)
	end

	-- Optional: Policy-bezogene Signals
	if tags and tags.attach_policy_signals then
		tags.attach_policy_signals()
	end

	-- Pro Screen Initialisierung
	awful.screen.connect_for_each_screen(function(s)
		if opts.wallpaper_fn then
			opts.wallpaper_fn(s) -- einmal initial
		end
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
