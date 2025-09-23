-- ~/.config/awesome/features/shell/menu/lib/actions.lua
local awful = require("awful")

local Actions = {}

-- Menü-API Handle (vom Menü während des Setups gesetzt)
-- Falls init noch nicht lief, verwenden wir __menu_api als Fallback.
local _api = nil

--- Initialisiert den Actions-Dispatcher mit der Menü-API.
-- Wird normalerweise aus parts/init.lua (Lib.attach) aufgerufen.
function Actions.init(api)
	_api = api
end

-- interner Helper: Dialog aus dialogs/init.lua bauen & im Overlay anzeigen
local function open_dialog_by_name(name, args)
	-- robust: API-Fallback auf globales Menü-API
	local api = _api or rawget(_G, "__menu_api")
	if not api then
		return
	end

	local Dialogs = require("features.shell.menu.dialogs")
	local theme = (api.get_theme and api:get_theme()) or {}
	local fn = Dialogs and Dialogs[name]
	if type(fn) ~= "function" then
		return
	end

	-- Optionen zusammenführen (Theme + Embed, plus Caller-Args)
	local opts = { theme = theme, embed = true }
	for k, v in pairs(args or {}) do
		opts[k] = v
	end

	-- Widget erzeugen
	local widget = fn(opts)

	-- Menü sicher nach vorne holen
	if api.show then
		api:show()
	end

	-- Dialog im Menü anzeigen
	if api.show_dialog then
		api:show_dialog(widget)
	end
end

-- Öffentliche Einzelfall-Runner (optional)
function Actions.run_dialog(name, args)
	open_dialog_by_name(name, args)
end

function Actions.run_shell(cmd)
	awful.spawn.with_shell(cmd)
end

function Actions.run_bin(bin, argv)
	awful.spawn({ bin, unpack(argv or {}) })
end

--- Universeller Click-Runner für Menü-Items.
-- Priorität: dialog > on_press > cmd > bin
-- item Felder:
--   dialog="power"|"logout"|...
--   dialog_args={...}
--   on_press=function() ... end
--   cmd="systemctl poweroff"
--   bin="firefox", argv={"--new-window","..."}
function Actions.run(item)
	if not item then
		return
	end

	if item.dialog then
		return open_dialog_by_name(item.dialog, item.dialog_args)
	end

	if type(item.on_press) == "function" then
		return item.on_press()
	end

	if item.cmd then
		return Actions.run_shell(item.cmd)
	end

	if item.bin then
		return Actions.run_bin(item.bin, item.argv)
	end

	-- nichts zu tun
end

--- Erzeugt eine klickbare Callback-Funktion (für :buttons)
function Actions.click(item)
	return function()
		Actions.run(item)
	end
end

return Actions
