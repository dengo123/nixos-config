-- ~/.config/awesome/features/shell/menu/lib/actions.lua
local awful = require("awful")

local Actions = {}
local _api = nil

function Actions.init(api)
	_api = api
end

local function get_api()
	return _api or rawget(_G, "__menu_api")
end
local function tbl(x)
	return type(x) == "table" and x or {}
end
local function merge(a, b)
	local o = {}
	if type(a) == "table" then
		for k, v in pairs(a) do
			o[k] = v
		end
	end
	if type(b) == "table" then
		for k, v in pairs(b) do
			o[k] = v
		end
	end
	return o
end

-- --- Dialog öffnen (wie gehabt)
local function open_dialog_by_name(name, args)
	local api = get_api()
	if not api then
		return
	end
	local Dialogs = require("features.shell.menu.dialogs")
	local fn = Dialogs and Dialogs[name]
	if type(fn) ~= "function" then
		return
	end
	local theme = (api.get_theme and api:get_theme()) or {}
	local opts = merge({ theme = theme, embed = true }, tbl(args))
	local ok, widget = pcall(fn, opts)
	if not ok then
		return
	end
	if api.show then
		pcall(api.show, api)
	end
	if api.show_dialog then
		pcall(api.show_dialog, api, widget)
	end
	return true
end

-- --- Policy-Ausführung (ersetzt früheres dialogs/parts/actions.lua)
-- policy = { close="before"|"after"|"none" }
-- policy = { close="before"|"after"|"none" }
local function run_with_policy(policy, fn_close, fn_action)
	policy = policy or { close = "before" }

	local function close_dialogs()
		-- 1) wie früher: alle Dialog-Popups schließen (best effort)
		local ok, Popup = pcall(require, "features.shell.menu.dialogs.parts.popup")
		if ok and Popup and type(Popup.close_all) == "function" then
			pcall(Popup.close_all)
		end
		-- 2) zusätzlich: über Menü-API das Dialog-Overlay schließen (falls vorhanden)
		local api = get_api()
		if api and api.hide_dialog then
			pcall(api.hide_dialog, api)
		end
		-- 3) spezifische close()-Funktion, die der Caller (z. B. actions_row) übergibt
		if fn_close then
			pcall(fn_close)
		end
		-- WICHTIG: Menü selbst *nicht* verstecken; das alte Verhalten schloss nur Dialoge.
	end

	if policy.close == "before" then
		close_dialogs()
	end

	if fn_action then
		pcall(fn_action)
	end

	if policy.close == "after" then
		close_dialogs()
	end
end

-- --- Fabriken (Back-Compat)
function Actions.cmd(cmd, policy)
	return function(close)
		run_with_policy(policy, close, function()
			if cmd and #cmd > 0 then
				awful.spawn.with_shell(cmd)
			end
		end)
	end
end

function Actions.lua(fn, policy)
	return function(close)
		run_with_policy(policy, close, function()
			if type(fn) == "function" then
				pcall(fn)
			end
		end)
	end
end

function Actions.signal(sig, payload, policy)
	return function(close)
		run_with_policy(policy, close, function()
			if sig then
				awesome.emit_signal(sig, payload)
			end
		end)
	end
end

-- --- Primitive Runner
function Actions.run_shell(cmd)
	if cmd and #cmd > 0 then
		awful.spawn.with_shell(cmd)
	end
end

function Actions.run_bin(bin, argv)
	if bin and #bin > 0 then
		awful.spawn({ bin, unpack(tbl(argv)) })
	end
end

-- --- Haupt-Dispatcher (datengetriebene Items)
--  Felder: dialog, dialog_args, on_press, cmd, bin, argv, policy={close=...}
function Actions.run(item)
	if not item then
		return
	end
	-- Dialog
	if item.dialog then
		return open_dialog_by_name(item.dialog, item.dialog_args)
	end
	-- expliziter Callback mit optionaler Policy
	if type(item.on_press) == "function" then
		return run_with_policy(item.policy, nil, item.on_press)
	end
	-- shell
	if item.cmd then
		return run_with_policy(item.policy, nil, function()
			Actions.run_shell(item.cmd)
		end)
	end
	-- bin
	if item.bin then
		return run_with_policy(item.policy, nil, function()
			Actions.run_bin(item.bin, item.argv)
		end)
	end
end

function Actions.click(item)
	return function()
		Actions.run(item)
	end
end

return Actions
