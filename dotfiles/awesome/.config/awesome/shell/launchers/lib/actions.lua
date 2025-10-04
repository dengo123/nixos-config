-- ~/.config/awesome/shell/launchers/lib/actions.lua
local awful = require("awful")
local gears = require("gears")
local Actions, _api = {}, nil

-- API-Context (optional, wird z. B. via Lib.attach(api) gesetzt)
function Actions.init(api)
	_api = api
end

local function get_api()
	return _api or rawget(_G, "__menu_api")
end

-- -------------------- util helpers --------------------
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

-- stabiler Schlüssel je Item für throttle
local function item_key(it)
	if type(it) ~= "table" then
		return tostring(it)
	end
	if it.id then
		return "id:" .. tostring(it.id)
	end
	if it.dialog then
		return "dlg:" .. tostring(it.dialog)
	end
	if it.cmd then
		return "cmd:" .. tostring(it.cmd)
	end
	if it.bin then
		local argv = table.concat(tbl(it.argv), " ")
		return "bin:" .. tostring(it.bin) .. "|" .. argv
	end
	if type(it.on_press) == "function" then
		return "cb:" .. tostring(it.on_press)
	end
	return "item:" .. tostring(it)
end

-- -------------------- throttle/debounce --------------------
local _cooldown = {} -- key -> true während Cooldown
local _reentry = false

local function throttle(key, window_s, fn)
	local win = tonumber(window_s) or 0.22
	if _cooldown[key] then
		return
	end
	_cooldown[key] = true
	gears.timer.start_new(win, function()
		_cooldown[key] = nil
		return false
	end)
	return fn()
end

-- -------------------- dialog resolver --------------------
local function try_open_module(modname, opts)
	local ok, mod = pcall(require, modname)
	if not ok or not mod then
		return nil
	end

	if type(mod.open) == "function" then
		return mod.open(opts)
	end
	if type(mod.show) == "function" then
		return mod.show(opts)
	end
	if type(mod) == "function" then
		return mod(opts)
	end
	return nil
end

local function open_dialog_by_name(name, args)
	local api = get_api()

	local theme = (api and api.get_theme and api:get_theme()) or {}
	local opts = merge({ theme = theme, embed = true }, tbl(args))

	-- >>> Wichtig: Menü vor Dialog-Open weg (inkl. dessen Keygrabber)
	if api and api.hide then
		pcall(api.hide, api)
	elseif api and api.pause then
		pcall(api.pause, api)
	end

	local handle = nil
	if name == "power" then
		handle = try_open_module("shell.menu.power", opts)
	elseif name == "search" then
		handle = try_open_module("shell.menu.search", opts)
	else
		return
	end

	-- Optional: wenn dein Overlay einen Dialog „einrahmt“, aber ohne eigenen Grabber
	if api and api.show_dialog and handle and (handle.widget or handle.popup) then
		pcall(api.show_dialog, api, handle.widget or handle.popup)
	end
	return true
end

-- -------------------- policy wrapper --------------------
-- policy = { close="before"|"after"|"none", menu_close=true|false }
local function run_with_policy(policy, fn_close, fn_action)
	policy = policy or { close = "before" }

	if _reentry then
		return
	end
	_reentry = true
	gears.timer.delayed_call(function()
		_reentry = false
	end)

	local function close_dialogs_only()
		local api = get_api()
		if api and api.hide_dialog then
			pcall(api.hide_dialog, api)
		end
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
		maybe_close_menu()
		return
	end

	-- default: Menü nach der Aktion schließen
	maybe_close_menu()
end

-- -------------------- factories --------------------
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

-- primitive runner (direkt, ohne policy)
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

-- -------------------- dispatcher --------------------
-- item:
--   { dialog="power"|"search", dialog_args=?,
--     on_press=function(close) ... end,
--     cmd=string | bin=string, argv={...},
--     policy={ close="before"|"after"|"none", menu_close=true/false },
--     id=? (für throttle-key) }
function Actions.run(item, close)
	if not item then
		return
	end
	local key = item_key(item)

	return throttle(key, 0.22, function()
		-- 1) Dialog öffnen (Menü wird vorher versteckt)
		if item.dialog then
			return open_dialog_by_name(item.dialog, item.dialog_args)
		end

		-- 2) Callback-Variante
		if type(item.on_press) == "function" then
			return item.on_press(close)
		end

		-- 3) Shell / Bin (mit Policy)
		if item.cmd then
			return run_with_policy(item.policy, close, function()
				Actions.run_shell(item.cmd)
			end)
		end
		if item.bin then
			return run_with_policy(item.policy, close, function()
				Actions.run_bin(item.bin, item.argv)
			end)
		end
	end)
end

function Actions.click(item)
	return function(close)
		Actions.run(item, close)
	end
end

return Actions
