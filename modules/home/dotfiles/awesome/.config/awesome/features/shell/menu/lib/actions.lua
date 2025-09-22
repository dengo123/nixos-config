-- ~/.config/awesome/features/shell/menu/lib/actions.lua
local awful = require("awful")

local Actions = {}

-- Menü-API (Overlay etc.) wird beim Setup aus parts/init.lua injiziert:
-- Actions.init(api)
local _api = nil

function Actions.init(api)
	_api = api
end

-- interner Helper: Dialog aus dialogs/init.lua bauen & im Overlay anzeigen
local function open_dialog_by_name(name, args)
	if not _api then
		return
	end
	local Dialogs = require("features.shell.menu.dialogs")
	local theme = (_api.get_theme and _api:get_theme()) or {}
	local fn = Dialogs and Dialogs[name]
	if type(fn) ~= "function" then
		return
	end

	local opts = { theme = theme, embed = true }
	for k, v in pairs(args or {}) do
		opts[k] = v
	end

	local widget = fn(opts)

	-- >>> wichtige Zeile: Menü aufklappen, falls gerade verdeckt
	if _api.show then
		_api:show()
	end

	if _api.show_dialog then
		_api:show_dialog(widget)
	end
end

-- öffentliche Einzelfall-Runner (optional)
function Actions.run_dialog(name, args)
	open_dialog_by_name(name, args)
end

function Actions.run_shell(cmd)
	awful.spawn.with_shell(cmd)
end

function Actions.run_bin(bin, argv)
	awful.spawn({ bin, unpack(argv or {}) })
end

-- Der **eine** universelle Click-Handler:
-- item kann enthalten (Priorität: dialog > on_press > cmd/bin):
--   dialog="power" | "logout" | ...
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

-- Komfort: liefert eine klickbare Callback-Funktion (für :buttons)
function Actions.click(item)
	return function()
		Actions.run(item)
	end
end

return Actions
