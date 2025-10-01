-- ~/.config/awesome/shell/workspaces/init.lua
local awful = require("awful")
local layouts = require("shell.workspaces.layouts")
local policies = require("shell.workspaces.policies")
local core = require("shell.workspaces.core")

local M = {}

function M.init(cfg)
	cfg = cfg or {}
	local opts = {
		ensure_one_tag = (cfg.ensure_one_tag ~= false),
		renumber_on_start = (cfg.renumber_on_start ~= false),
		auto_adapt_layout_on_rotation = (cfg.auto_adapt_layout_on_rotation ~= false),
		desktop_deco_fn = cfg.desktop_deco_fn,
	}

	-- 1) Globale Layoutliste
	if layouts and layouts.apply then
		layouts.apply()
	end

	-- 2) Policies aktivieren (Hooks etc.)
	if policies and policies.layout and policies.layout.init_enforcement then
		policies.layout.init_enforcement()
	end
	if policies and policies.attach_policy_signals then
		policies.attach_policy_signals()
	end

	-- 3) Core-Hooks setzen (Core ruft diese Policy-Funktionen)
	core.set_hooks({
		kill_clients_in_tag = policies.client and policies.client.kill_clients_in_tag or function(_) end,
		apply_layout_policy = policies.layout and policies.layout.apply_layout_policy or function(_) end,
	})

	-- 4) Pro Screen
	awful.screen.connect_for_each_screen(function(s)
		if opts.ensure_one_tag then
			core.ensure(s)
		end
		if opts.renumber_on_start then
			core.renumber(s)
		end
		if policies.layout and policies.layout.apply_layout_policy then
			policies.layout.apply_layout_policy(s)
		end
		if opts.desktop_deco_fn then
			opts.desktop_deco_fn(s)
		end
	end)

	-- 5) Rotation/Geometrie – Policy hängt bereits Hooks; optional nochmal:
	if opts.auto_adapt_layout_on_rotation and policies.layout and policies.layout.on_screen_rotation then
		policies.layout.on_screen_rotation()
	end
end

return M
