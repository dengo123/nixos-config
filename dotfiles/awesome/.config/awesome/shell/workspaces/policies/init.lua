-- ~/.config/awesome/shell/workspaces/policies/init.lua
local P = {}

P.layout = require("shell.workspaces.policies.layout_policy")

local okF, focus = pcall(require, "shell.workspaces.policies.focus_policy")
P.focus = okF and focus or nil

local okC, clientp = pcall(require, "shell.workspaces.policies.client_policy")
P.client = okC and clientp or nil

local okA, autorandrp = pcall(require, "shell.workspaces.policies.autorandr_policy")
P.autorandr = okA and autorandrp or nil

function P.attach_policy_signals()
	if P.focus and type(P.focus.attach_policy_signals) == "function" then
		P.focus.attach_policy_signals(P.layout.apply_layout_policy)
	end
	if P.autorandr and type(P.autorandr.attach_policy_signals) == "function" then
		P.autorandr.attach_policy_signals()
	end
end

return P
