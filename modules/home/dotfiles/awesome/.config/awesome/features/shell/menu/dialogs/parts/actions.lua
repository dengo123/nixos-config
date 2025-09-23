-- features/shell/menu/dialogs/parts/actions.lua
local awful = require("awful")
local Popup = require("features.shell.menu.dialogs.parts.popup")

local A = {}

local function run_with_policy(policy, fn_close, fn_action)
	policy = policy or { close = "before" } -- "before" | "after" | "none"
	if policy.close == "before" then
		Popup.close_all()
		if fn_close then
			fn_close()
		end
	end
	if fn_action then
		fn_action()
	end
	if policy.close == "after" then
		Popup.close_all()
		if fn_close then
			fn_close()
		end
	end
end

function A.cmd(cmd, policy)
	return function(close)
		run_with_policy(policy, close, function()
			if cmd and #cmd > 0 then
				awful.spawn.with_shell(cmd)
			end
		end)
	end
end

function A.lua(fn, policy)
	return function(close)
		run_with_policy(policy, close, function()
			if fn then
				pcall(fn)
			end
		end)
	end
end

function A.signal(sig, payload, policy)
	return function(close)
		run_with_policy(policy, close, function()
			if sig then
				awesome.emit_signal(sig, payload)
			end
		end)
	end
end

return A
