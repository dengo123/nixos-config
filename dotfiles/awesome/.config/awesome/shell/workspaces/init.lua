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
	runtime = {},
	policies = {},
	input = nil,
}

local runtime = {
	ctx = {},
	workspaces = {},
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function ctx()
	return runtime.ctx or {}
end

local function cfg()
	return ctx().cfg or {}
end

local function ui()
	return ctx().ui or {}
end

local function shell()
	return ctx().shell or {}
end

local function build_workspaces()
	local conf = cfg()

	local RuntimeMods = M.runtime or {}
	local Policies = M.policies or {}

	local tags_cfg = conf.tags or {}
	local layouts_cfg = tags_cfg.layouts or {}

	return {
		cfg = conf,
		ui = ui(),
		shell = shell(),

		tags_cfg = tags_cfg,
		layouts_cfg = layouts_cfg,

		runtime = RuntimeMods,
		policies = Policies,

		actions = RuntimeMods.actions,
		controller = RuntimeMods.controller,
		layouts = RuntimeMods.layouts,
		sync = RuntimeMods.sync,
		dynamic = RuntimeMods.dynamic,

		layout_policy = Policies.layout,
		spacing_policy = Policies.spacing,
		focus_policy = Policies.focus,
		delete_policy = Policies.delete,
		max_policy = Policies.max,

		windowing = shell().windowing or nil,
	}
end

local function attach_policy_signals(workspaces)
	local Focus = workspaces.focus_policy
	local Layout = workspaces.layout_policy
	local Spacing = workspaces.spacing_policy

	if Focus and type(Focus.attach_policy_signals) == "function" then
		Focus.attach_policy_signals(Layout and Layout.apply_layout_policy or nil)
	end

	if Spacing and type(Spacing.init) == "function" then
		Spacing.init(workspaces.cfg)
	end
end

local function configure_controller(workspaces)
	local Controller = workspaces.controller
	assert(Controller, "workspaces.init: runtime.controller fehlt")

	if type(Controller.set_config) == "function" then
		Controller.set_config({
			tags_fixed_count = workspaces.tags_cfg.fixed_count,
			default_layout = workspaces.layouts_cfg.default or "max",
		})
	end
end

local function configure_controller_hooks(workspaces)
	local Controller = workspaces.controller
	local DeletePolicy = workspaces.delete_policy
	local LayoutPolicy = workspaces.layout_policy

	assert(Controller, "workspaces.init: runtime.controller fehlt")

	if type(Controller.set_hooks) == "function" then
		Controller.set_hooks({
			kill_clients_in_tag = DeletePolicy and DeletePolicy.kill_clients_in_tag or function(_) end,
			apply_layout_policy = LayoutPolicy and LayoutPolicy.apply_layout_policy or function(_) end,
		})
	end
end

local function build_input(RuntimeActions, Mref)
	return {
		tags = {
			view_tag_idx = RuntimeActions and RuntimeActions.view_tag_idx or nil,
			move_tag_to_screen = RuntimeActions and RuntimeActions.move_tag_to_screen or nil,
			move_client_to_neighbor_tag = RuntimeActions and RuntimeActions.move_client_to_neighbor_tag or nil,
			add = Mref.add,
			add_silent = Mref.add_silent,
			delete_current = Mref.delete_current,
			delete_current_force = Mref.delete_current_force,
		},
	}
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	runtime.ctx = args or {}

	M.runtime = {
		actions = safe_require("shell.workspaces.runtime.actions"),
		controller = safe_require("shell.workspaces.runtime.controller"),
		layouts = safe_require("shell.workspaces.runtime.layouts"),
		sync = safe_require("shell.workspaces.runtime.sync"),
		dynamic = safe_require("shell.workspaces.runtime.dynamic"),
	}

	M.policies = {
		layout = safe_require("shell.workspaces.policies.layout_policy"),
		spacing = safe_require("shell.workspaces.policies.spacing_policy"),
		focus = safe_require("shell.workspaces.policies.focus_policy"),
		delete = safe_require("shell.workspaces.policies.delete_policy"),
		max = safe_require("shell.workspaces.policies.max_policy"),
	}

	runtime.workspaces = build_workspaces()
	local workspaces = runtime.workspaces

	M.workspaces = workspaces

	local RuntimeActions = workspaces.actions
	local RuntimeController = workspaces.controller
	local RuntimeLayouts = workspaces.layouts
	local RuntimeSync = workspaces.sync
	local RuntimeDynamic = workspaces.dynamic

	local PolicyLayout = workspaces.layout_policy
	local PolicyDelete = workspaces.delete_policy
	local PolicyMax = workspaces.max_policy

	assert(RuntimeController, "workspaces.init: runtime.controller fehlt")
	assert(RuntimeLayouts, "workspaces.init: runtime.layouts fehlt")

	configure_controller(workspaces)

	local opts = {
		ensure_one_tag = (workspaces.cfg.ensure_one_tag ~= false),
		renumber_on_start = (workspaces.cfg.renumber_on_start ~= false),
		auto_adapt_layout_on_rotation = (workspaces.cfg.auto_adapt_layout_on_rotation ~= false),
		desktop_deco_fn = workspaces.cfg.desktop_deco_fn,
	}

	local selection_mode = tostring(workspaces.tags_cfg.selection or "single"):lower()
	local use_sync = (selection_mode == "sync")
	local impl = use_sync and RuntimeSync or RuntimeController

	assert(impl, "workspaces.init: runtime implementation fehlt")

	if type(RuntimeLayouts.apply) == "function" then
		RuntimeLayouts.apply(workspaces.cfg)
	end

	if PolicyLayout and type(PolicyLayout.init) == "function" then
		PolicyLayout.init({
			workspaces = workspaces,
			cfg = workspaces.cfg,
		})
	end

	if PolicyLayout and type(PolicyLayout.init_enforcement) == "function" then
		PolicyLayout.init_enforcement({
			workspaces = workspaces,
			cfg = workspaces.cfg,
		})
	end

	if PolicyDelete and type(PolicyDelete.init) == "function" then
		PolicyDelete.init(workspaces.cfg)
	end

	if PolicyMax and type(PolicyMax.init) == "function" then
		PolicyMax.init({
			workspaces = workspaces,
		})
	end

	attach_policy_signals(workspaces)
	configure_controller_hooks(workspaces)

	if RuntimeActions and type(RuntimeActions.init) == "function" then
		RuntimeActions.init({
			workspaces = workspaces,
		})
	end

	if RuntimeSync and type(RuntimeSync.init) == "function" then
		RuntimeSync.init({
			workspaces = workspaces,
		})
	end

	if RuntimeDynamic and type(RuntimeDynamic.init) == "function" then
		RuntimeDynamic.init({
			workspaces = workspaces,
		})
	end

	if tostring(workspaces.tags_cfg.mode or "fixed"):lower() == "dynamic" then
		if RuntimeDynamic and type(RuntimeDynamic.enable) == "function" then
			RuntimeDynamic.enable({
				kill_clients_in_tag = PolicyDelete and PolicyDelete.kill_clients_in_tag or function(_) end,
				apply_layout_policy = PolicyLayout and PolicyLayout.apply_layout_policy or function(_) end,
			})
		end
	end

	awful.screen.connect_for_each_screen(function(s)
		if opts.ensure_one_tag then
			RuntimeController.ensure(s)
		end

		if opts.renumber_on_start then
			RuntimeController.renumber(s)
		end

		if PolicyLayout and PolicyLayout.apply_layout_policy then
			PolicyLayout.apply_layout_policy(s)
		end

		if PolicyMax and type(PolicyMax.apply_current) == "function" then
			PolicyMax.apply_current(s)
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

	M.add = impl.add or RuntimeController.add
	M.add_silent = impl.add_silent or RuntimeController.add_silent
	M.delete_current = impl.delete_current or RuntimeController.delete_current
	M.delete_current_force = impl.delete_current_force or RuntimeController.delete_current_force
	M.view_tag_idx = impl.view_tag_idx
	M.view_tag_abs = impl.view_tag_abs
	M.ensure = RuntimeController.ensure
	M.renumber = RuntimeController.renumber

	M.input = build_input(RuntimeActions, M)

	return M
end

return M
