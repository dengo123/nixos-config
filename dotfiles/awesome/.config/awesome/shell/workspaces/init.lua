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

	-- 0) Implementierung wählen: "core" oder "sync"
	local flag = tostring(cfg.workspaces or "core"):lower()
	local useSync = (flag == "sync") or flag:match("sync") ~= nil
	local impl = useSync and require("shell.workspaces.sync") or core

	-- (optional) Diagnose
	local okN, naughty = pcall(require, "naughty")
	if okN then
		naughty.notify({ title = "Workspaces", text = useSync and "SYNC mode" or "CORE mode" })
	end

	-- 1) Globale Layoutliste
	if layouts and layouts.apply then
		layouts.apply()
	end

	-- 2) Policies
	if policies and policies.layout and policies.layout.init_enforcement then
		policies.layout.init_enforcement()
	end
	if policies and policies.attach_policy_signals then
		policies.attach_policy_signals()
	end

	-- 3) Hooks in core setzen (core ruft diese Policies)
	core.set_hooks({
		kill_clients_in_tag = policies.client and policies.client.kill_clients_in_tag or function(_) end,
		apply_layout_policy = policies.layout and policies.layout.apply_layout_policy or function(_) end,
	})

	-- 4) Pro Screen Grundsetup
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

	-- 5) Rotation/Geometrie  (BUGFIX: Leerzeichen vor 'then')
	if opts.auto_adapt_layout_on_rotation and policies.layout and policies.layout.on_screen_rotation then
		policies.layout.on_screen_rotation()
	end

	-- 5b) SELECTION SYNC aktivieren + initial angleichen (nur im Sync-Modus)
	if useSync and type(impl.init_selection_sync) == "function" then
		impl.init_selection_sync()
		-- initial: Primär-Screen-Auswahl auf alle spiegeln
		local primary = awful.screen.primary
		if primary and primary.selected_tag and type(impl.view_tag_abs) == "function" then
			impl.view_tag_abs(primary.selected_tag.index)
		end
	end

	-- 6) Re-Exports (dieses Modul ist die Abstraktion)
	M.add = impl.add or core.add
	M.add_silent = impl.add_silent or core.add_silent
	M.delete_current = impl.delete_current or core.delete_current
	M.delete_current_force = impl.delete_current_force or core.delete_current_force
	-- Sync-spezifisch (nur vorhanden, wenn useSync)
	M.view_tag_idx = impl.view_tag_idx
	M.view_tag_abs = impl.view_tag_abs

	-- Basis-API (aus core)
	M.ensure = core.ensure
	M.renumber = core.renumber

	return M
end

return M
