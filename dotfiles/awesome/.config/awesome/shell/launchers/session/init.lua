-- ~/.config/awesome/shell/launchers/session/init.lua
local awful = require("awful")

local function safe_require(path)
	local ok, mod = pcall(require, path)
	if ok then
		return mod
	end

	return nil
end

local M = {
	container = nil,
	layout = nil,
	icons = nil,
	theme = nil,
	variants = {},
	_handle = {},
}

local runtime = {
	cfg = {},
	ui = {},
	launchers = nil,
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function cfg()
	return runtime.cfg or {}
end

local function ui()
	return runtime.ui or {}
end

local function launchers()
	return runtime.launchers
end

local function button_label(spec, fallback)
	if type(spec) == "table" then
		return spec.label or fallback
	end

	if type(spec) == "string" and spec ~= "" then
		return spec
	end

	return fallback
end

local function button_id(spec, fallback)
	if type(spec) == "table" then
		return spec.id or fallback
	end

	return fallback
end

local function resolve_theme(conf, overrides)
	local Theme = M.theme

	if Theme and type(Theme.init) == "function" then
		Theme.init({
			cfg = conf or {},
			ui = ui(),
		})
	end

	local theme = (Theme and type(Theme.get) == "function" and Theme.get()) or {}

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
	local h = tonumber(th.dialog_h)

	local header_h = tonumber(th.header_h)
	local footer_h = tonumber(th.footer_h)

	if (not header_h or not footer_h) and th.header_ratio and th.footer_ratio then
		header_h = header_h or math.floor(h * tonumber(th.header_ratio))
		footer_h = footer_h or math.floor(h * tonumber(th.footer_ratio))
	end

	header_h = tonumber(header_h)
	footer_h = tonumber(footer_h)

	local pad_h = tonumber(th.pad_h)
	local pad_v = tonumber(th.pad_v)

	local body_h = h - header_h - footer_h

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
	local handle = M._handle

	if type(handle.is_open) == "function" and not handle.is_open() then
		M._handle = {}
	end
end

local function resolve_variant(opts)
	local variant = tostring((opts and opts.variant) or "power"):lower()

	if variant == "logoff" or variant == "lock" or variant == "session" then
		return "logoff"
	end

	return "power"
end

local function resolve_module(variant)
	if variant == "logoff" then
		return M.variants.logoff
	end

	return M.variants.power
end

local function resolve_lib()
	local L = launchers() or {}
	return L.lib or {}
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(opts)
	opts = opts or {}

	runtime.cfg = opts.cfg or runtime.cfg
	runtime.ui = opts.ui or runtime.ui
	runtime.launchers = opts.launchers or runtime.launchers

	M.container = safe_require("shell.launchers.session.container")
	M.layout = safe_require("shell.launchers.session.layout")
	M.icons = safe_require("shell.launchers.session.icons")
	M.theme = safe_require("shell.launchers.session.theme")
	M.variants = {
		logoff = safe_require("shell.launchers.session.logoff"),
		power = safe_require("shell.launchers.session.power"),
	}

	return M
end

function M.is_open()
	clear_handle_if_closed()

	local handle = M._handle
	return type(handle.is_open) == "function" and handle.is_open() or false
end

function M.close()
	clear_handle_if_closed()

	local handle = M._handle
	if type(handle.close) == "function" then
		handle.close()
	end

	M._handle = {}
end

function M.open(opts)
	opts = opts or {}

	if M.is_open() then
		M.close()
		return nil
	end

	local Container = M.container
	local Layout = M.layout
	local Icons = M.icons

	assert(Container and type(Container.build) == "function", "launchers.session.init: container.build fehlt")
	assert(Layout and type(Layout.build_row) == "function", "launchers.session.init: layout.build_row fehlt")

	local icon_button_fn = Icons and Icons.mk_icon_button or nil

	local lib = resolve_lib()
	local Popup = lib.popup
	local Button = lib.button
	local Actions = lib.actions

	assert(Popup and type(Popup.show) == "function", "launchers.session.init: popup.show fehlt")
	assert(Button and type(Button.mk_button) == "function", "launchers.session.init: button.mk_button fehlt")
	assert(Actions and type(Actions.bind) == "function", "launchers.session.init: actions.bind fehlt")

	local conf = opts.cfg or cfg()
	local th = resolve_theme(conf, opts.theme)
	local d = resolve_dims(th)

	local variant = resolve_variant(opts)
	local Variant = resolve_module(variant)

	local handle = nil
	local spec = Variant and type(Variant.build) == "function" and Variant.build(th, conf) or nil
	local actions = spec and spec.actions or nil
	local header_title = spec and spec.header_title or nil
	local cancel_spec = (spec and spec.cancel_label) or th.cancel_label

	local cancel_label = button_label(cancel_spec, "Cancel")
	local cancel_id = button_id(cancel_spec, "cancel")

	local row, _, required_w = Layout.build_row(actions, th, d, {
		mk_icon_button = icon_button_fn,
	}, function()
		local current = handle or M._handle
		if type(current.close) == "function" then
			return current.close
		end

		return nil
	end)

	local act_cancel = function() end

	local cancel_btn = Button.mk_button(cancel_label, function()
		act_cancel()
	end)

	local stack = Container.build(th, d, {
		title = opts.title or header_title,
		body = row,
		cancel_btn = cancel_btn,
	})

	handle = Popup.show(stack, th, {
		width = resolve_popup_width(th, required_w),
		height = d.h,
		placement = awful.placement.centered,
		use_backdrop = (((conf.launchers or {}).session or {})[variant] or {}).backdrop ~= false,
		group = "launchers",
		show_root = false,
	})

	M._handle = handle or {}

	local bound = Actions.bind({
		handle = handle,
		actions = actions,
	}) or {}

	act_cancel = bound[cancel_id] or act_cancel

	return handle
end

return M
