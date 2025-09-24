-- ~/.config/awesome/features/shell/menu/lib/actions.lua
local awful = require("awful")

local Actions, _api = {}, nil
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

-- Dialog öffnen (Menü sichtbar lassen)
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

-- ---------------------------------------------
-- Zentrale Policy-Ausführung
-- policy = {
--   close = "before" | "after" | "none",  -- wann Dialog(e) schließen
--   menu_close = true|false,              -- Menü schließen? (default: true)
-- }
-- ---------------------------------------------
local function run_with_policy(policy, fn_close, fn_action)
	policy = policy or { close = "before" }

	local function close_dialogs_only()
		-- 1) Standalone-Dialoge (ältere APIs)
		local ok, Popup = pcall(require, "features.shell.menu.dialogs.parts.popup")
		if ok and Popup and type(Popup.close_all) == "function" then
			pcall(Popup.close_all)
		end
		-- 2) Menü-Overlay-Dialog
		local api = get_api()
		if api and api.hide_dialog then
			pcall(api.hide_dialog, api)
		end
		-- 3) explizites close() vom Dialog-Builder
		if fn_close then
			pcall(fn_close)
		end
	end

	local function maybe_close_menu()
		local api = get_api()
		if (policy.menu_close ~= false) and api and api.hide then
			pcall(api.hide, api)
		end
	end

	if policy.close == "before" then
		close_dialogs_only()
	end

	if fn_action then
		pcall(fn_action)
	end

	if policy.close == "after" then
		close_dialogs_only()
		maybe_close_menu()
		return
	end

	if policy.close == "before" then
		-- Dialoge waren vorher zu; jetzt noch Menü schließen (default: an)
		maybe_close_menu()
		return
	end

	-- close == "none" oder nicht gesetzt:
	-- Standard: Menü nach der Aktion schließen (falls nicht explizit verboten)
	maybe_close_menu()
end

-- Fabriken
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

-- Primitive Runner
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

-- Dispatcher
-- item: { dialog, dialog_args, on_press, cmd, bin, argv, policy }
function Actions.run(item)
	if not item then
		return
	end
	if item.dialog then
		return open_dialog_by_name(item.dialog, item.dialog_args)
	end
	if type(item.on_press) == "function" then
		return run_with_policy(item.policy, nil, item.on_press)
	end
	if item.cmd then
		return run_with_policy(item.policy, nil, function()
			Actions.run_shell(item.cmd)
		end)
	end
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
