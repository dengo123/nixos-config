-- ~/.config/awesome/shell/launchers/power/init.lua
local awful = require("awful")
local Container = require("shell.launchers.power.container")
local Layout = require("shell.launchers.power.layout")
local IconsMod = require("shell.launchers.power.icons")

local M = {}

-- ============================================================================
-- Theme
-- ============================================================================

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
		w = tonumber(th.dialog_w) or 0,
		h = H,
		header_h = header_h,
		footer_h = footer_h,
		body_h = body_h,
		pad_h = pad_h,
		pad_v = pad_v,
	}
end

-- ============================================================================
-- Helpers
-- ============================================================================

local function icon_from_theme(th, key)
	local tbl = th.icons or {}
	local p = tbl[key]

	if type(p) ~= "string" or #p == 0 then
		return nil
	end
	if p:match("^/") then
		return p
	end

	return require("gears").filesystem.get_configuration_dir() .. p
end

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

	local labels = th.labels or {}

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

-- ============================================================================
-- Public API
-- ============================================================================

function M.open(opts, injected_lib)
	opts = opts or {}
	local Lib = injected_lib or require("shell.launchers.lib")

	local th = resolve_theme(opts.theme)
	local d = dims(th)

	local handle

	local row, _items, required_w = Layout.build_row(
		make_actions(Lib, th),
		th,
		d,
		{ mk_icon_button = assert(IconsMod and IconsMod.mk_icon_button, "power.icons.mk_icon_button fehlt") },
		function()
			return handle and handle.close
		end
	)

	local Button = (Lib and Lib.button) or require("shell.launchers.lib.button")
	local cancel_btn = Button.mk_button(th.cancel_label or "Cancel", function()
		if handle and handle.close then
			handle.close()
		end
	end)

	local stack = Container.build(th, d, {
		title = th.header_title or opts.title or "Turn off Computer",
		body = row,
		cancel_btn = cancel_btn,
	})

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
