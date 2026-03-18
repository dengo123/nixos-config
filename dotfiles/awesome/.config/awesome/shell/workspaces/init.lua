-- ~/.config/awesome/shell/workspaces/init.lua
-- ~/.config/awesome/shell/workspaces/init.lua
local awful = require("awful")

local Actions = require("shell.workspaces.actions")
local core = require("shell.workspaces.core")
local layouts = require("shell.workspaces.layouts")
local policies = require("shell.workspaces.policies")

local M = {
	actions = Actions,
}

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(cfg)
	cfg = cfg or {}

	-- ---------------------------------------------------------------------
	-- Config
	-- ---------------------------------------------------------------------

	local tags_cfg = cfg.tags or {}

	core.set_config({
		tags_mode = tags_cfg.mode,
		tags_fixed_count = tags_cfg.fixed_count,
	})

	local opts = {
		ensure_one_tag = (cfg.ensure_one_tag ~= false),
		renumber_on_start = (cfg.renumber_on_start ~= false),
		auto_adapt_layout_on_rotation = (cfg.auto_adapt_layout_on_rotation ~= false),
		desktop_deco_fn = cfg.desktop_deco_fn,
	}

	local selection_mode = tostring(tags_cfg.selection or "single"):lower()
	local use_sync = (selection_mode == "sync")
	local impl = use_sync and require("shell.workspaces.sync") or core

	-- ---------------------------------------------------------------------
	-- Layouts
	-- ---------------------------------------------------------------------

	if layouts and layouts.apply then
		layouts.apply()
	end

	-- ---------------------------------------------------------------------
	-- Policies
	-- ---------------------------------------------------------------------

	if policies and policies.layout and policies.layout.init_enforcement then
		policies.layout.init_enforcement()
	end

	if policies and policies.attach_policy_signals then
		policies.attach_policy_signals()
	end

	core.set_hooks({
		kill_clients_in_tag = policies.client and policies.client.kill_clients_in_tag or function(_) end,
		apply_layout_policy = policies.layout and policies.layout.apply_layout_policy or function(_) end,
	})

	if tostring(tags_cfg.mode or "fixed"):lower() == "dynamic" then
		local dyn = require("shell.workspaces.dynamic")

		dyn.enable(core, {
			kill_clients_in_tag = policies.client and policies.client.kill_clients_in_tag or function(_) end,
			apply_layout_policy = policies.layout and policies.layout.apply_layout_policy or function(_) end,
		})
	end

	-- ---------------------------------------------------------------------
	-- Screens
	-- ---------------------------------------------------------------------

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

	-- ---------------------------------------------------------------------
	-- Rotation
	-- ---------------------------------------------------------------------

	if opts.auto_adapt_layout_on_rotation and policies.layout and policies.layout.on_screen_rotation then
		policies.layout.on_screen_rotation()
	end

	-- ---------------------------------------------------------------------
	-- Selection Sync
	-- ---------------------------------------------------------------------

	if use_sync and type(impl.init_selection_sync) == "function" then
		impl.init_selection_sync()

		local primary = awful.screen.primary

		if primary and primary.selected_tag and type(impl.view_tag_abs) == "function" then
			impl.view_tag_abs(primary.selected_tag.index)
		end
	end

	-- ---------------------------------------------------------------------
	-- Re-Exports
	-- ---------------------------------------------------------------------

	M.add = impl.add or core.add
	M.add_silent = impl.add_silent or core.add_silent
	M.delete_current = impl.delete_current or core.delete_current
	M.delete_current_force = impl.delete_current_force or core.delete_current_force
	M.view_tag_idx = impl.view_tag_idx
	M.view_tag_abs = impl.view_tag_abs
	M.ensure = core.ensure
	M.renumber = core.renumber

	return M
end

return M
