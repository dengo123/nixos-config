-- ~/.config/awesome/features/workspaces/tags/init.lua
local core = require("features.workspaces.tags.core")
local layout_policy = require("features.workspaces.tags.layout_policy")
local focus_policy = require("features.workspaces.tags.focus_policy")
local client_policy = require("features.workspaces.tags.client_policy")

-- Core mit Policies verdrahten (keine Zyklen, klare Abhängigkeiten)
core.set_hooks({
	kill_clients_in_tag = client_policy.kill_clients_in_tag,
	apply_layout_policy = layout_policy.apply_layout_policy,
})

local M = {}

-- Re-Exports (öffentliche API wie vorher)
M.ensure = core.ensure
M.renumber = core.renumber
M.add = core.add
M.delete_current = core.delete_current

M.apply_layout_policy = layout_policy.apply_layout_policy
M.apply_layout_policy_all = layout_policy.apply_layout_policy_all
M.on_screen_rotation = layout_policy.on_screen_rotation

M.focus_master_current = focus_policy.focus_master_current
M.attach_policy_signals = function()
	-- Fokus-Signale hängen intern die Layout-Policy gleich mit an
	focus_policy.attach_policy_signals(layout_policy.apply_layout_policy)
end

return M
