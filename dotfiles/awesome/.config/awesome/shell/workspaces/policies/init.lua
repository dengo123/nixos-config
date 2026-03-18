-- ~/.config/awesome/shell/workspaces/policies/init.lua
local M = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function safe_require(path)
	local ok, mod = pcall(require, path)
	if ok then
		return mod
	end

	return nil
end

-- =========================================================================
-- Policies
-- =========================================================================

M.layout = require("shell.workspaces.policies.layout_policy")
M.spacing = require("shell.workspaces.policies.spacing_policy")
M.focus = safe_require("shell.workspaces.policies.focus_policy")
M.client = safe_require("shell.workspaces.policies.client_policy")
M.autorandr = safe_require("shell.workspaces.policies.autorandr_policy")

-- =========================================================================
-- Public API
-- =========================================================================

function M.attach_policy_signals()
	if M.focus and type(M.focus.attach_policy_signals) == "function" then
		M.focus.attach_policy_signals(M.layout.apply_layout_policy)
	end

	if M.spacing and type(M.spacing.init) == "function" then
		M.spacing.init()
	end

	if M.autorandr and type(M.autorandr.attach_policy_signals) == "function" then
		M.autorandr.attach_policy_signals()
	end
end

return M
