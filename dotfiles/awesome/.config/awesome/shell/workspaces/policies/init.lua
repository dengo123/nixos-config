-- ~/.config/awesome/shell/workspaces/policies/init.lua
local P = {}

P.layout = require("shell.workspaces.policies.layout_policy")

-- Optional: eigene Focus/Client Policies hier anschließen
local okF, focus = pcall(require, "shell.workspaces.policies.focus_policy")
P.focus = okF and focus or nil

local okC, clientp = pcall(require, "shell.workspaces.policies.client_policy")
P.client = okC and clientp or nil

-- Bequeme „attach“-Funktion für Fokussignale etc.
function P.attach_policy_signals()
	if P.focus and type(P.focus.attach_policy_signals) == "function" then
		P.focus.attach_policy_signals(P.layout.apply_layout_policy)
	end
end

return P
