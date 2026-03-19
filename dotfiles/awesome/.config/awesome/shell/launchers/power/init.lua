-- ~/.config/awesome/shell/launchers/power/init.lua
local awful = require("awful")

local Actions = require("shell.launchers.power.actions")
local Container = require("shell.launchers.power.container")
local Layout = require("shell.launchers.power.layout")
local Icons = require("shell.launchers.power.icons")
local Theme = require("shell.launchers.power.theme")

local M = {
	_handle = nil,
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function resolve_theme(cfg, overrides)
	Theme.init(cfg or {})

	local theme = Theme.get()
	assert(type(theme) == "table", "power: theme.get() lieferte kein table")

	if type(overrides) ~= "table" or next(overrides) == nil then
		return theme
	end

	local merged = {}
	for key, value in pairs(theme) do
		merged[key] = value
	end
	for key, value in pairs(overrides) do
		merged[key] = value
	end

	return merged
end

local function resolve_dims(th)
	local h = assert(tonumber(th.dialog_h), "power: dialog_h fehlt/ungültig")

	local header_h = tonumber(th.header_h)
	local footer_h = tonumber(th.footer_h)

	if (not header_h or not footer_h) and th.header_ratio and th.footer_ratio then
		header_h = header_h or math.floor(h * tonumber(th.header_ratio))
		footer_h = footer_h or math.floor(h * tonumber(th.footer_ratio))
	end

	header_h = assert(tonumber(header_h), "power: header_h fehlt/ungültig")
	footer_h = assert(tonumber(footer_h), "power: footer_h fehlt/ungültig")

	local pad_h = assert(tonumber(th.pad_h), "power: pad_h fehlt/ungültig")
	local pad_v = assert(tonumber(th.pad_v), "power: pad_v fehlt/ungültig")
	local body_h = h - header_h - footer_h

	assert(body_h >= 0, "power: body_h negativ – prüfe dialog_h/header_h/footer_h")

	return {
		w = tonumber(th.dialog_w) or 0,
		h = h,
		header_h = header_h,
		footer_h = footer_h,
		body_h = body_h,
		pad_h = pad_h,
		pad_v = pad_v,
	}
end

local function resolve_popup_width(th, required_w)
	if required_w and required_w > 0 then
		return required_w
	end

	if th.dialog_w ~= 0 then
		return th.dialog_w
	end

	return nil
end

local function clear_handle_if_closed()
	if M._handle and M._handle.is_open and not M._handle.is_open() then
		M._handle = nil
	end
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.is_open()
	clear_handle_if_closed()
	return M._handle and M._handle.is_open and M._handle.is_open() or false
end

function M.close()
	clear_handle_if_closed()

	if M._handle and M._handle.close then
		M._handle.close()
	end

	M._handle = nil
end

function M.open(opts, Lib)
	opts = opts or {}
	Lib = Lib or require("shell.launchers")

	-- ---------------------------------------------------------------------
	-- Toggle existing
	-- ---------------------------------------------------------------------

	if M.is_open() then
		M.close()
		return nil
	end

	-- ---------------------------------------------------------------------
	-- Config
	-- ---------------------------------------------------------------------

	assert(Lib and Lib.popup, "power: Lib.popup fehlt")
	assert(Lib and Lib.button, "power: Lib.button fehlt")
	assert(Lib and Lib.actions, "power: Lib.actions fehlt")

	local cfg = opts.cfg or {}
	local th = resolve_theme(cfg, opts.theme)
	local d = resolve_dims(th)

	-- ---------------------------------------------------------------------
	-- Layout
	-- ---------------------------------------------------------------------

	local handle = nil
	local actions = Actions.build(th, cfg)

	local row, _, required_w = Layout.build_row(actions, th, d, {
		mk_icon_button = assert(Icons and Icons.mk_icon_button, "power.icons.mk_icon_button fehlt"),
	}, function()
		return handle and handle.close
	end)

	local act_cancel = function() end

	local cancel_btn = Lib.button.mk_button(th.cancel_label, function()
		act_cancel()
	end)

	local stack = Container.build(th, d, {
		title = opts.title or th.header_title,
		body = row,
		cancel_btn = cancel_btn,
	})

	-- ---------------------------------------------------------------------
	-- Open
	-- ---------------------------------------------------------------------

	local launchers_cfg = cfg.launchers or {}
	local power_cfg = launchers_cfg.power or {}

	handle = Lib.popup.show(stack, th, {
		width = resolve_popup_width(th, required_w),
		height = d.h,
		placement = awful.placement.centered,
		use_backdrop = (power_cfg.backdrop ~= false),
		group = "launchers",
		show_root = "with_bars",
	})

	M._handle = handle

	-- ---------------------------------------------------------------------
	-- Bind Actions
	-- ---------------------------------------------------------------------

	local bound = Lib.actions.bind({
		handle = handle,
		actions = actions,
	})

	act_cancel = bound[th.cancel_label] or act_cancel

	return handle
end

return M
