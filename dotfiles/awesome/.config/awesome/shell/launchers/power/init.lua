-- ~/.config/awesome/shell/launchers/power/init.lua
local awful = require("awful")
local Container = require("shell.launchers.power.container")
local Layout = require("shell.launchers.power.layout")
local IconsMod = require("shell.launchers.power.icons")

local M = {}

-- Theme zusammenführen (ui/theme/power.lua -> Theme.get + optionale overrides)
local function resolve_theme(overrides)
	local base = {}
	local ok, mod = pcall(require, "ui.theme.power")
	if ok and mod and type(mod.get) == "function" then
		base = mod.get({})
	end
	local out = {}
	for k, v in pairs(base or {}) do
		out[k] = v
	end
	for k, v in pairs(overrides or {}) do
		out[k] = v
	end
	return out
end

-- Höhen ableiten: feste header/footer_h oder alternativ Ratios
local function dims(th)
	local H = assert(tonumber(th.dialog_h), "power: dialog_h fehlt/ungültig")
	local header_h = tonumber(th.header_h)
	local footer_h = tonumber(th.footer_h)

	if (not header_h or not footer_h) and th.header_ratio and th.footer_ratio then
		header_h = header_h or math.floor(H * tonumber(th.header_ratio))
		footer_h = footer_h or math.floor(H * tonumber(th.footer_ratio))
	end

	header_h = assert(tonumber(header_h), "power: header_h fehlt/ungültig")
	footer_h = assert(tonumber(footer_h), "power: footer_h fehlt/ungültig")

	local pad_h = assert(tonumber(th.pad_h), "power: pad_h fehlt/ungültig")
	local pad_v = assert(tonumber(th.pad_v), "power: pad_v fehlt/ungültig")
	local body_h = H - header_h - footer_h
	assert(body_h >= 0, "power: body_h negativ – prüfe dialog_h/header_h/footer_h")

	return {
		w = tonumber(th.dialog_w) or 0, -- 0 => Layout bestimmt Breite
		h = H,
		header_h = header_h,
		footer_h = footer_h,
		body_h = body_h,
		pad_h = pad_h,
		pad_v = pad_v,
	}
end

-- Hilfsfunktion: Theme-Iconpfad (relativ -> absolut), sonst nil
local function icon_from_theme(th, key)
	local tbl = th.icons or {}
	local p = tbl[key]
	if type(p) ~= "string" or #p == 0 then
		return nil
	end
	if p:match("^/") then
		return p
	end
	-- relativ zum Awesome config dir
	return require("gears").filesystem.get_configuration_dir() .. p
end

-- Aktionen mit Icon-Fallbacks (Emoji, falls Icon fehlt)
local function make_actions(Lib, th)
	local A = Lib and Lib.actions
	local function run(cmd)
		if A and A.cmd then
			return A.cmd(cmd, { close = "before" })
		end
		return function()
			awful.spawn.with_shell(cmd)
		end
	end

	local labels = (th.labels or {}) -- optional: Labels aus Theme überschreiben
	return {
		{
			icon = icon_from_theme(th, "hibernate"),
			emoji = "🚪",
			label = labels.hibernate or "Hibernate",
			on_press = run("systemctl hibernate"),
		},
		{
			icon = icon_from_theme(th, "poweroff"),
			emoji = "⏻",
			label = labels.poweroff or "Turn Off",
			on_press = run("systemctl poweroff"),
		},
		{
			icon = icon_from_theme(th, "reboot"),
			emoji = "🔄",
			label = labels.reboot or "Restart",
			on_press = run("systemctl reboot"),
		},
	}
end

-- Öffentliche API
function M.open(opts, injected_lib)
	opts = opts or {}
	local Lib = injected_lib or require("shell.launchers.lib")

	local th = resolve_theme(opts.theme)
	local d = dims(th)

	local handle

	-- Body (Iconreihe) – Icon-Buttons via IconsMod.mk_icon_button
	local row, _items, required_w = Layout.build_row(
		make_actions(Lib, th),
		th,
		d,
		{ mk_icon_button = assert(IconsMod and IconsMod.mk_icon_button, "power.icons.mk_icon_button fehlt") },
		function()
			return handle and handle.close
		end
	)

	-- Cancel-Button (einheitlich aus launchers/lib/cancel.lua)
	local Cancel = (Lib and Lib.cancel) or require("shell.launchers.lib.cancel")
	local cancel_btn = Cancel.mk_cancel_button(th.cancel_label or "Cancel", function()
		if handle and handle.close then
			handle.close()
		end
	end)

	-- Container (Header/Body/Footer + Rahmen)
	local stack = Container.build(th, d, {
		title = th.header_title or opts.title or "Turn off Computer",
		body = row,
		cancel_btn = cancel_btn,
	})

	-- Popup zeigen
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
