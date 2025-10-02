-- ~/.config/awesome/shell/launchers/power/init.lua
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")

local Container = require("shell.launchers.power.container")
local Layout = require("shell.launchers.power.layout")
local Icons = require("shell.launchers.power.icons")

local M = {}

local function resolve_theme(overrides)
	local ok, mod = pcall(require, "ui.theme.power")
	local base = {}
	if ok and mod and type(mod.get) == "function" then
		base = mod.get({})
	end
	-- merge base + overrides (overrides gewinnen)
	local out = {}
	for k, v in pairs(base or {}) do
		out[k] = v
	end
	for k, v in pairs(overrides or {}) do
		out[k] = v
	end
	return out
end

local function dims(th)
	local H = assert(tonumber(th.dialog_h), "power: dialog_h fehlt")
	local header_h = tonumber(th.header_h)
	local footer_h = tonumber(th.footer_h)

	if (not header_h or not footer_h) and th.header_ratio and th.footer_ratio then
		header_h = header_h or math.floor(H * tonumber(th.header_ratio))
		footer_h = footer_h or math.floor(H * tonumber(th.footer_ratio))
	end

	header_h = assert(tonumber(header_h), "power: header_h fehlt/ungültig")
	footer_h = assert(tonumber(footer_h), "power: footer_h fehlt/ungültig")

	local pad_h = assert(tonumber(th.pad_h), "power: pad_h fehlt")
	local pad_v = assert(tonumber(th.pad_v), "power: pad_v fehlt")
	local body_h = H - header_h - footer_h
	assert(body_h >= 0, "power: body_h negativ")

	return {
		w = tonumber(th.dialog_w) or 0,
		h = H,
		header_h = header_h,
		footer_h = footer_h,
		body_h = body_h,
		pad_h = pad_h,
		pad_v = pad_v,
	}
end

local function make_actions(Lib)
	local A = Lib and Lib.actions
	local function run(cmd)
		if A and A.cmd then
			return A.cmd(cmd, { close = "before" })
		end
		return function()
			awful.spawn.with_shell(cmd)
		end
	end
	return {
		{ emoji = "🚪", label = "Hibernate", on_press = run("systemctl hibernate") },
		{ emoji = "🛌", label = "Stand By", on_press = run("systemctl suspend") },
		{ emoji = "⏻", label = "Turn Off", on_press = run("systemctl poweroff") },
		{ emoji = "🔄", label = "Restart", on_press = run("systemctl reboot") },
		{
			emoji = "👤",
			label = "Log off",
			on_press = run([[
        if command -v dm-tool >/dev/null 2>&1; then dm-tool switch-to-greeter
        elif command -v gdmflexiserver >/dev/null 2>&1; then gdmflexiserver
        else command -v notify-send >/dev/null 2>&1 && notify-send "Switch user" "Kein passender DM-Befehl gefunden."; fi
      ]]),
		},
	}
end

function M.open(opts, injected_lib)
	opts = opts or {}
	local Lib = injected_lib or require("shell.launchers.lib")

	local th = resolve_theme(opts.theme)
	local d = dims(th)

	local handle

	-- Body (Icon-Reihe) – Button-Factory injiziert
	local row, _items, required_w = Layout.build_row(
		make_actions(Lib),
		th,
		d,
		{ mk_icon_button = assert(Icons and Icons.mk_icon_button, "power.icons.mk_icon_button fehlt") },
		function()
			return handle and handle.close
		end
	)

	-- Cancel (aus launchers/lib/cancel.lua)
	local Cancel = (Lib and Lib.cancel) or require("shell.launchers.lib.cancel")
	local cancel_btn = Cancel.mk_cancel_button(th.cancel_label or "Cancel", function()
		if handle and handle.close then
			handle.close()
		end
	end)

	-- Container (Header/Body/Footer/Border)
	local stack = Container.build(th, d, {
		title = th.header_title or opts.title or "Turn off Computer",
		body = row,
		cancel_btn = cancel_btn,
	})

	-- Popup
	local Popup = (Lib and Lib.popup) or require("shell.launchers.lib.popup")
	handle = Popup.show(stack, th, {
		width = (required_w and required_w > 0) and required_w or (th.dialog_w ~= 0 and th.dialog_w or nil),
		height = d.h,
		placement = awful.placement.centered,
		use_backdrop = true,
		group = "launchers",
		show_root = "with_bars",
	})

	return handle
end

return M
