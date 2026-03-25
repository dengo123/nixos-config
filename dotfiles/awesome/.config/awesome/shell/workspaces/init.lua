-- ~/.config/awesome/shell/workspaces/init.lua
local awful = require("awful")

local function safe_require(path)
	local ok, mod = pcall(require, path)
	if ok then
		return mod
	end

	return nil
end

local M = {
	api = {},
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function api()
	return M.api or {}
end

local function runtime_api()
	return api().runtime or {}
end

local function policies_api()
	return api().policies or {}
end

local function runtime_mod(name)
	return runtime_api()[name]
end

local function policy_mod(name)
	return policies_api()[name]
end

local function attach_policy_signals(cfg)
	local Focus = policy_mod("focus")
	local Layout = policy_mod("layout")
	local Spacing = policy_mod("spacing")

	if Focus and type(Focus.attach_policy_signals) == "function" then
		Focus.attach_policy_signals(Layout and Layout.apply_layout_policy or nil)
	end

	if Spacing and type(Spacing.init) == "function" then
		Spacing.init(cfg)
	end
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	args = args or {}

	local cfg = args.cfg or args or {}

	M.api = {
		ui = args.ui or {},
		runtime = {
			actions = safe_require("shell.workspaces.runtime.actions"),
			base = safe_require("shell.workspaces.runtime.base"),
			layouts = safe_require("shell.workspaces.runtime.layouts"),
			sync = safe_require("shell.workspaces.runtime.sync"),
			dynamic = safe_require("shell.workspaces.runtime.dynamic"),
		},
		policies = {
			layout = safe_require("shell.workspaces.policies.layout_policy"),
			spacing = safe_require("shell.workspaces.policies.spacing_policy"),
			focus = safe_require("shell.workspaces.policies.focus_policy"),
			client = safe_require("shell.workspaces.policies.client_policy"),
		},
	}

	local RuntimeActions = runtime_mod("actions")
	local RuntimeBase = runtime_mod("base")
	local RuntimeLayouts = runtime_mod("layouts")
	local RuntimeSync = runtime_mod("sync")
	local RuntimeDynamic = runtime_mod("dynamic")

	local PolicyLayout = policy_mod("layout")
	local PolicyClient = policy_mod("client")

	local tags_cfg = cfg.tags or {}
	local layouts_cfg = tags_cfg.layouts or {}

	RuntimeBase.set_config({
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
	local impl = use_sync and RuntimeSync or RuntimeBase

	if RuntimeLayouts and type(RuntimeLayouts.apply) == "function" then
		RuntimeLayouts.apply(cfg)
	end

	if PolicyLayout and type(PolicyLayout.init_enforcement) == "function" then
		PolicyLayout.init_enforcement({
			cfg = cfg,
			api = M.api,
		})
	end

	if PolicyClient and type(PolicyClient.init) == "function" then
		PolicyClient.init(cfg)
	end

	attach_policy_signals(cfg)

	RuntimeBase.set_hooks({
		kill_clients_in_tag = PolicyClient and PolicyClient.kill_clients_in_tag or function(_) end,
		apply_layout_policy = PolicyLayout and PolicyLayout.apply_layout_policy or function(_) end,
	})

	if tostring(tags_cfg.mode or "fixed"):lower() == "dynamic" then
		if RuntimeDynamic and type(RuntimeDynamic.enable) == "function" then
			RuntimeDynamic.enable(RuntimeBase, {
				kill_clients_in_tag = PolicyClient and PolicyClient.kill_clients_in_tag or function(_) end,
				apply_layout_policy = PolicyLayout and PolicyLayout.apply_layout_policy or function(_) end,
			})
		end
	end

	awful.screen.connect_for_each_screen(function(s)
		if opts.ensure_one_tag then
			RuntimeBase.ensure(s)
		end

		if opts.renumber_on_start then
			RuntimeBase.renumber(s)
		end

		if PolicyLayout and PolicyLayout.apply_layout_policy then
			PolicyLayout.apply_layout_policy(s)
		end

		if opts.desktop_deco_fn then
			opts.desktop_deco_fn(s)
		end
	end)

	if opts.auto_adapt_layout_on_rotation and PolicyLayout and PolicyLayout.on_screen_rotation then
		PolicyLayout.on_screen_rotation()
	end

	if use_sync and type(impl.init_selection_sync) == "function" then
		impl.init_selection_sync()

		local primary = awful.screen.primary
		if primary and primary.selected_tag and type(impl.view_tag_abs) == "function" then
			impl.view_tag_abs(primary.selected_tag.index)
		end
	end

	M.add = impl.add or RuntimeBase.add
	M.add_silent = impl.add_silent or RuntimeBase.add_silent
	M.delete_current = impl.delete_current or RuntimeBase.delete_current
	M.delete_current_force = impl.delete_current_force or RuntimeBase.delete_current_force
	M.view_tag_idx = impl.view_tag_idx
	M.view_tag_abs = impl.view_tag_abs
	M.ensure = RuntimeBase.ensure
	M.renumber = RuntimeBase.renumber

	M.actions = {
		view_tag_idx = RuntimeActions and RuntimeActions.view_tag_idx or nil,
		move_tag_to_screen = RuntimeActions and RuntimeActions.move_tag_to_screen or nil,
		move_client_to_neighbor_tag = RuntimeActions and RuntimeActions.move_client_to_neighbor_tag or nil,
		add = M.add,
		add_silent = M.add_silent,
		delete_current = M.delete_current,
		delete_current_force = M.delete_current_force,
	}

	return M
end

return M
