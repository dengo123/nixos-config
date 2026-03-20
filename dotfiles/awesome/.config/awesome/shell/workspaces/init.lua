-- ~/.config/awesome/shell/workspaces/init.lua
local awful = require("awful")

local function safe_require(path)
	local ok, mod = pcall(require, path)
	if ok then
		return mod
	end

	return nil
end

local Runtime = {
	actions = safe_require("shell.workspaces.runtime.actions"),
	base = safe_require("shell.workspaces.runtime.base"),
	layouts = safe_require("shell.workspaces.runtime.layouts"),
	sync = safe_require("shell.workspaces.runtime.sync"),
	dynamic = safe_require("shell.workspaces.runtime.dynamic"),
}

local Policies = {
	layout = safe_require("shell.workspaces.policies.layout_policy"),
	spacing = safe_require("shell.workspaces.policies.spacing_policy"),
	focus = safe_require("shell.workspaces.policies.focus_policy"),
	client = safe_require("shell.workspaces.policies.client_policy"),
	autorandr = safe_require("shell.workspaces.policies.autorandr_policy"),
}

assert(Runtime.actions and type(Runtime.actions) == "table", "workspaces.init: runtime.actions fehlt")
assert(Runtime.base and type(Runtime.base) == "table", "workspaces.init: runtime.base fehlt")
assert(Runtime.layouts and type(Runtime.layouts) == "table", "workspaces.init: runtime.layouts fehlt")
assert(Policies.layout and type(Policies.layout) == "table", "workspaces.init: policies.layout fehlt")
assert(Policies.spacing and type(Policies.spacing) == "table", "workspaces.init: policies.spacing fehlt")

local M = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function attach_policy_signals(cfg)
	if Policies.focus and type(Policies.focus.attach_policy_signals) == "function" then
		Policies.focus.attach_policy_signals(Policies.layout.apply_layout_policy)
	end

	if Policies.spacing and type(Policies.spacing.init) == "function" then
		Policies.spacing.init(cfg)
	end

	if Policies.autorandr and type(Policies.autorandr.attach_policy_signals) == "function" then
		Policies.autorandr.attach_policy_signals()
	end
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(cfg)
	cfg = cfg or {}

	local tags_cfg = cfg.tags or {}
	local layouts_cfg = tags_cfg.layouts or {}

	Runtime.base.set_config({
		tags_mode = tags_cfg.mode,
		tags_fixed_count = tags_cfg.fixed_count,
		default_layout = layouts_cfg.default or "max",
	})

	local opts = {
		ensure_one_tag = (cfg.ensure_one_tag ~= false),
		renumber_on_start = (cfg.renumber_on_start ~= false),
		auto_adapt_layout_on_rotation = (cfg.auto_adapt_layout_on_rotation ~= false),
		desktop_deco_fn = cfg.desktop_deco_fn,
	}

	local selection_mode = tostring(tags_cfg.selection or "single"):lower()
	local use_sync = (selection_mode == "sync")
	local impl = use_sync and Runtime.sync or Runtime.base

	if use_sync then
		assert(Runtime.sync and type(Runtime.sync) == "table", "workspaces.init: runtime.sync fehlt")
	end

	if Runtime.layouts and type(Runtime.layouts.apply) == "function" then
		Runtime.layouts.apply(cfg)
	end

	if Policies.layout and type(Policies.layout.init_enforcement) == "function" then
		Policies.layout.init_enforcement(cfg)
	end

	if Policies.client and type(Policies.client.init) == "function" then
		Policies.client.init(cfg)
	end

	attach_policy_signals(cfg)

	Runtime.base.set_hooks({
		kill_clients_in_tag = Policies.client and Policies.client.kill_clients_in_tag or function(_) end,
		apply_layout_policy = Policies.layout and Policies.layout.apply_layout_policy or function(_) end,
	})

	if tostring(tags_cfg.mode or "fixed"):lower() == "dynamic" then
		assert(Runtime.dynamic and type(Runtime.dynamic) == "table", "workspaces.init: runtime.dynamic fehlt")

		Runtime.dynamic.enable(Runtime.base, {
			kill_clients_in_tag = Policies.client and Policies.client.kill_clients_in_tag or function(_) end,
			apply_layout_policy = Policies.layout and Policies.layout.apply_layout_policy or function(_) end,
		})
	end

	awful.screen.connect_for_each_screen(function(s)
		if opts.ensure_one_tag then
			Runtime.base.ensure(s)
		end

		if opts.renumber_on_start then
			Runtime.base.renumber(s)
		end

		if Policies.layout and Policies.layout.apply_layout_policy then
			Policies.layout.apply_layout_policy(s)
		end

		if opts.desktop_deco_fn then
			opts.desktop_deco_fn(s)
		end
	end)

	if opts.auto_adapt_layout_on_rotation and Policies.layout and Policies.layout.on_screen_rotation then
		Policies.layout.on_screen_rotation()
	end

	if use_sync and type(impl.init_selection_sync) == "function" then
		impl.init_selection_sync()

		local primary = awful.screen.primary
		if primary and primary.selected_tag and type(impl.view_tag_abs) == "function" then
			impl.view_tag_abs(primary.selected_tag.index)
		end
	end

	M.add = impl.add or Runtime.base.add
	M.add_silent = impl.add_silent or Runtime.base.add_silent
	M.delete_current = impl.delete_current or Runtime.base.delete_current
	M.delete_current_force = impl.delete_current_force or Runtime.base.delete_current_force
	M.view_tag_idx = impl.view_tag_idx
	M.view_tag_abs = impl.view_tag_abs
	M.ensure = Runtime.base.ensure
	M.renumber = Runtime.base.renumber

	M.actions = {
		view_tag_idx = Runtime.actions.view_tag_idx,
		move_tag_to_screen = Runtime.actions.move_tag_to_screen,
		move_client_to_neighbor_tag = Runtime.actions.move_client_to_neighbor_tag,
		add = M.add,
		add_silent = M.add_silent,
		delete_current = M.delete_current,
		delete_current_force = M.delete_current_force,
	}

	return M
end

return M
