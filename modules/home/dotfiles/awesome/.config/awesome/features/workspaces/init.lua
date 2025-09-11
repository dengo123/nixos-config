-- ~/.config/awesome/features/workspaces/init.lua
local awful = require("awful") -- <- WICHTIG: fehlte
local M = {}

function M.init(opts)
	opts = opts or {}

	-- sinnvolle Defaults
	local ensure_one_tag = (opts.ensure_one_tag ~= false)
	local renumber_on_start = (opts.renumber_on_start ~= false)
	local auto_adapt = (opts.auto_adapt_layout_on_rotation ~= false)

	local tags = require("features.workspaces.tags")
	local sigs = require("features.workspaces.screen_signals")
	local layouts = require("features.workspaces.layouts")

	-- Layouts global setzen
	layouts.apply()

	-- Wallpaper-Signale nur hier anhängen (vermeide Doppelaufruf)
	if opts.wallpaper_fn then
		sigs.attach(opts.wallpaper_fn)
	end

	awful.screen.connect_for_each_screen(function(s)
		if opts.wallpaper_fn then
			opts.wallpaper_fn(s) -- einmal initial
		end
		if ensure_one_tag and tags.ensure then
			tags.ensure(s)
		end
		if renumber_on_start and tags.renumber then
			tags.renumber(s)
		end
		-- deine Layout-Policy (siehe tags.apply_layout_policy unten)
		if tags.apply_layout_policy then
			tags.apply_layout_policy(s)
		end
		if opts.desktop_deco_fn then
			opts.desktop_deco_fn(s)
		end
	end)

	if auto_adapt and tags.on_screen_rotation then
		tags.on_screen_rotation()
	end
end

return M
