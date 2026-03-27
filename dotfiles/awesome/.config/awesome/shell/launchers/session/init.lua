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
	api = {},
	_handle = nil,
}

local runtime = {
	ctx = {},
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function ctx()
	return runtime.ctx or {}
end

local function cfg()
	return ctx().cfg or {}
end

local function ui()
	return ctx().ui or {}
end

local function api()
	return M.api or {}
end

local function mod(name)
	return api()[name]
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
	local Theme = mod("theme")

	if Theme and type(Theme.init) == "function" then
		Theme.init({
			ctx = ctx(),
			cfg = conf or {},
			ui = api().ui or ui(),
		})
	end

	local theme = (Theme and Theme.get and Theme.get()) or {}

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
	if M._handle and M._handle.is_open and not M._handle.is_open() then
		M._handle = nil
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
	local variants = api().variants or {}

	if variant == "logoff" then
		return variants.logoff
	end

	return variants.power
end

local function resolve_lib(Lib)
	return (Lib and Lib.api and Lib.api.lib) or {}
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	runtime.ctx = (args and (args.ctx or args)) or {}

	M.api = {
		ui = args and args.ui or ui(),
		container = safe_require("shell.launchers.session.container"),
		layout = safe_require("shell.launchers.session.layout"),
		icons = safe_require("shell.launchers.session.icons"),
		theme = safe_require("shell.launchers.session.theme"),
		variants = {
			logoff = safe_require("shell.launchers.session.logoff"),
			power = safe_require("shell.launchers.session.power"),
		},
	}

	return M
end

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
	Lib = Lib or {}

	if M.is_open() then
		M.close()
		return nil
	end

	local Container = mod("container")
	local Layout = mod("layout")
	local Icons = mod("icons")

	local lib = resolve_lib(Lib)
	local Popup = lib.popup
	local Button = lib.button
	local Actions = lib.actions

	local conf = opts.cfg or cfg()
	local th = resolve_theme(conf, opts.theme)
	local d = resolve_dims(th)

	local variant = resolve_variant(opts)
	local Variant = resolve_module(variant)

	local handle = nil
	local spec = Variant and Variant.build and Variant.build(th, conf) or nil
	local actions = spec and spec.actions or nil
	local header_title = spec and spec.header_title or nil
	local cancel_spec = (spec and spec.cancel_label) or th.cancel_label

	local cancel_label = button_label(cancel_spec, "Cancel")
	local cancel_id = button_id(cancel_spec, "cancel")

	local row, _, required_w = Layout.build_row(actions, th, d, {
		mk_icon_button = Icons and Icons.mk_icon_button,
	}, function()
		return handle and handle.close
	end)

	local act_cancel = function() end

	local cancel_btn = Button and Button.mk_button and Button.mk_button(cancel_label, function()
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
		show_root = "with_bars",
	})

	M._handle = handle

	local bound = Actions.bind({
		handle = handle,
		actions = actions,
	})

	act_cancel = bound[cancel_id] or act_cancel

	return handle
end

return M
