-- ~/.config/awesome/shell/menu/power/init.lua
local Parts = require("shell.menu.parts")
local Layout = require("shell.menu.power.layout")
local Lib = require("shell.menu.lib")

local M = {}
local policy = { close = "before" }

local function resolve_power_theme(overrides)
	local ok, ui_theme = pcall(require, "ui.theme")
	assert(ok and type(ui_theme) == "table" and ui_theme.power and ui_theme.power.get, "ui.theme.power.get fehlt")
	return ui_theme.power.get(overrides)
end

local function actions_power()
	return {
		{ emoji = "🚪", label = "Hibernate", on_press = Lib.cmd("systemctl hibernate", policy) },
		{ emoji = "🛌", label = "Stand By", on_press = Lib.cmd("systemctl suspend", policy) },
		{ emoji = "⏻", label = "Turn Off", on_press = Lib.cmd("systemctl poweroff", policy) },
		{ emoji = "🔄", label = "Restart", on_press = Lib.cmd("systemctl reboot", policy) },
		{
			emoji = "👤",
			label = "Log off",
			on_press = Lib.cmd(
				[[if command -v dm-tool >/dev/null 2>&1; then dm-tool switch-to-greeter
  elif command -v gdmflexiserver >/dev/null 2>&1; then gdmflexiserver
  else command -v notify-send >/dev/null 2>&1 && notify-send "Switch user" "Kein passender DM-Befehl gefunden."; fi]],
				policy
			),
		},
	}
end

local function make_dims(th)
	local W = tonumber(th.dialog_w) or 0 -- darf 0 sein → Breite kommt vom Layout
	local H = assert(tonumber(th.dialog_h), "power theme: dialog_h ungültig")
	local header_h = assert(tonumber(th.header_h), "power theme: header_h ungültig")
	local footer_h = assert(tonumber(th.footer_h), "power theme: footer_h ungültig")
	local pad_h = assert(tonumber(th.pad_h), "power theme: pad_h ungültig")
	local pad_v = assert(tonumber(th.pad_v), "power theme: pad_v ungültig")
	local body_h = H - header_h - footer_h
	assert(body_h >= 0, "power dims: body_h negativ – prüfe dialog_h/header_h/footer_h")

	return { w = W, h = H, header_h = header_h, footer_h = footer_h, body_h = body_h, pad_h = pad_h, pad_v = pad_v }
end

function M.open(opts)
	opts = opts or {}
	local th = resolve_power_theme(opts.theme)
	local dims = make_dims(th)

	return Parts.open({
		title = opts.title or "Turn off Computer",
		theme = th,
		container = "power",
		dims = dims, -- Größen/Höhen liefert Theme
		popup = {
			-- Breite wird von Parts.open anhand required_w gesetzt (oder auto, wenn nil)
			height = dims.h,
			show_root = "with_bars",
		},
		focus = { mouse_follow = true },
		build_body = function(th2, d, parts_api)
			local row, items, required_w = Layout.build_row(actions_power(), th2, d, function()
				return parts_api.close
			end)
			-- Parts.open erkennt required_w automatisch (3. Rückgabewert) und nutzt ihn als Popup-Breite.
			return row, items, required_w
		end,
	})
end

M.power = M.open
return M
