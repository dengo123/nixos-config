-- ~/.config/awesome/shell/workspaces/init.lua
local awful = require("awful")

local RuntimeActions = require("shell.workspaces.runtime.actions")
local Base = require("shell.workspaces.runtime.base")
local layouts = require("shell.workspaces.runtime.layouts")
local policies = require("shell.workspaces.policies")

local M = {}

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(cfg)
	cfg = cfg or {}

	local tags_cfg = cfg.tags or {}

	Base.set_config({
		tags_mode = tags_cfg.mode,
		tags_fixed_count = tags_cfg.fixed_count,
		default_layout = tags_cfg.default_layout,
	})

	local opts = {
		ensure_one_tag = (cfg.ensure_one_tag ~= false),
		renumber_on_start = (cfg.renumber_on_start ~= false),
		auto_adapt_layout_on_rotation = (cfg.auto_adapt_layout_on_rotation ~= false),
		desktop_deco_fn = cfg.desktop_deco_fn,
	}

	local selection_mode = tostring(tags_cfg.selection or "single"):lower()
	local use_sync = (selection_mode == "sync")
	local impl = use_sync and require("shell.workspaces.runtime.sync") or Base

	if layouts and layouts.apply then
		layouts.apply()
	end

	if policies.layout and policies.layout.init_enforcement then
		policies.layout.init_enforcement()
	end

	if policies.client and policies.client.init then
		policies.client.init(cfg)
	end

	if policies.attach_policy_signals then
		policies.attach_policy_signals()
	end

	Base.set_hooks({
		kill_clients_in_tag = policies.client and policies.client.kill_clients_in_tag or function(_) end,
		apply_layout_policy = policies.layout and policies.layout.apply_layout_policy or function(_) end,
	})

	if tostring(tags_cfg.mode or "fixed"):lower() == "dynamic" then
		local dyn = require("shell.workspaces.runtime.dynamic")

		dyn.enable(Base, {
			kill_clients_in_tag = policies.client and policies.client.kill_clients_in_tag or function(_) end,
			apply_layout_policy = policies.layout and policies.layout.apply_layout_policy or function(_) end,
		})
	end

	awful.screen.connect_for_each_screen(function(s)
		if opts.ensure_one_tag then
			Base.ensure(s)
		end

		if opts.renumber_on_start then
			Base.renumber(s)
		end

		if policies.layout and policies.layout.apply_layout_policy then
			policies.layout.apply_layout_policy(s)
		end

		if opts.desktop_deco_fn then
			opts.desktop_deco_fn(s)
		end
	end)

	if opts.auto_adapt_layout_on_rotation and policies.layout and policies.layout.on_screen_rotation then
		policies.layout.on_screen_rotation()
	end

	if use_sync and type(impl.init_selection_sync) == "function" then
		impl.init_selection_sync()

		local primary = awful.screen.primary
		if primary and primary.selected_tag and type(impl.view_tag_abs) == "function" then
			impl.view_tag_abs(primary.selected_tag.index)
		end
	end

	M.add = impl.add or Base.add
	M.add_silent = impl.add_silent or Base.add_silent
	M.delete_current = impl.delete_current or Base.delete_current
	M.delete_current_force = impl.delete_current_force or Base.delete_current_force
	M.view_tag_idx = impl.view_tag_idx
	M.view_tag_abs = impl.view_tag_abs
	M.ensure = Base.ensure
	M.renumber = Base.renumber

	M.actions = {
		view_tag_idx = RuntimeActions.view_tag_idx,
		move_tag_to_screen = RuntimeActions.move_tag_to_screen,
		move_client_to_neighbor_tag = RuntimeActions.move_client_to_neighbor_tag,
		add = M.add,
		add_silent = M.add_silent,
		delete_current = M.delete_current,
		delete_current_force = M.delete_current_force,
	}

	return M
end

return M
