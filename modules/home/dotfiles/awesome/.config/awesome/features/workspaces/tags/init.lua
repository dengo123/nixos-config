-- ~/.config/awesome/features/workspaces/tags/init.lua
local core = require("features.workspaces.tags.core")
local layout_policy = require("features.workspaces.tags.layout_policy")
local focus_policy = require("features.workspaces.tags.focus_policy")
local client_policy = require("features.workspaces.tags.client_policy")

core.set_hooks({
	kill_clients_in_tag = client_policy.kill_clients_in_tag,
	apply_layout_policy = layout_policy.apply_layout_policy,
})

local M = {}

-- Re-Exports (öffentliche API)
M.ensure = core.ensure
M.renumber = core.renumber
M.add = core.add
M.add_silent = core.add_silent
M.delete_current = core.delete_current
M.delete_current_force = core.delete_current_force

M.apply_layout_policy = layout_policy.apply_layout_policy
M.apply_layout_policy_all = layout_policy.apply_layout_policy_all
M.on_screen_rotation = layout_policy.on_screen_rotation

M.focus_master_current = focus_policy.focus_master_current
M.attach_policy_signals = function()
	focus_policy.attach_policy_signals(layout_policy.apply_layout_policy)
end

return M
