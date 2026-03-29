-- ~/.config/awesome/shell/launchers/session/controller.lua
local awful = require("awful")
local gears = require("gears")

local M = {}

function M.new(opts)
	opts = opts or {}

	local Container = opts.container
	local Layout = opts.layout
	local Icons = opts.icons
	local Theme = opts.theme
	local Variants = opts.variants or {}

	local resolve_lib = opts.resolve_lib
	local cfg_fn = opts.cfg
	local ui_fn = opts.ui

	local state = {
		handle = {},
		modal = {
			previous_root_keys = nil,
			active_handle = nil,
		},
	}

	-- =========================================================================
	-- Helpers
	-- =========================================================================

	local function cfg()
		if type(cfg_fn) == "function" then
			return cfg_fn() or {}
		end

		return cfg_fn or {}
	end

	local function ui()
		if type(ui_fn) == "function" then
			return ui_fn() or {}
		end

		return ui_fn or {}
	end

	local function modal_state()
		return state.modal or {}
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
		local handle = state.handle

		if type(handle.is_open) == "function" and not handle.is_open() then
			state.handle = {}
		end
	end

	local function resolve_variant(open_opts)
		local variant = tostring((open_opts and open_opts.variant) or "power"):lower()

		if variant == "logoff" or variant == "lock" or variant == "session" then
			return "logoff"
		end

		return "power"
	end

	local function resolve_module(variant)
		if variant == "logoff" then
			return Variants.logoff
		end

		return Variants.power
	end

	local function restore_modal_keys(handle)
		local modal = modal_state()

		if modal.active_handle ~= handle then
			return
		end

		if modal.previous_root_keys then
			root.keys(modal.previous_root_keys)
		else
			root.keys(nil)
		end

		modal.previous_root_keys = nil
		modal.active_handle = nil
		state.modal = modal
	end

	local function install_modal_keys(modkey, handle, variant)
		if not (handle and type(handle.close) == "function") then
			return
		end

		local modal = modal_state()

		if modal.active_handle == handle then
			return
		end

		if modal.active_handle and type(modal.active_handle.close) == "function" then
			pcall(function()
				modal.active_handle.close()
			end)
		end

		local previous_root_keys = root.keys()
		local modal_keys = {}

		local modal_mod_sets = {
			{},
			{ modkey },
		}

		local function close_handle()
			if handle and type(handle.close) == "function" then
				handle.close()
			end
		end

		local function add_modal_key(key, fn, desc)
			for _, mods in ipairs(modal_mod_sets) do
				table.insert(
					modal_keys,
					awful.key(mods, key, fn, {
						description = desc or key,
						group = "Launchers",
					})
				)
			end
		end

		if variant == "logoff" then
			add_modal_key("l", function()
				close_handle()
				awful.spawn({ "dm-tool", "lock" }, false)
			end, "Lock Session")

			add_modal_key("o", function()
				close_handle()
				awful.spawn.with_shell("pkill -KILL -u " .. (os.getenv("USER") or ""))
			end, "Log Off")

			add_modal_key("u", function()
				close_handle()
				awful.spawn({ "dm-tool", "switch-to-greeter" }, false)
			end, "Switch User")
		else
			add_modal_key("u", function()
				close_handle()
				awful.spawn.with_shell("systemctl suspend")
			end, "Stand By")

			add_modal_key("h", function()
				close_handle()
				awful.spawn.with_shell("systemctl hibernate")
			end, "Sleep")

			add_modal_key("r", function()
				close_handle()
				awful.spawn.with_shell("systemctl reboot")
			end, "Reboot")

			add_modal_key("p", function()
				close_handle()
				awful.spawn.with_shell("systemctl poweroff")
			end, "Power Off")
		end

		add_modal_key("Escape", function()
			close_handle()
		end, "Cancel")

		local joined_keys
		if previous_root_keys then
			joined_keys = gears.table.join(previous_root_keys, table.unpack(modal_keys))
		else
			joined_keys = gears.table.join(table.unpack(modal_keys))
		end

		root.keys(joined_keys)

		modal.previous_root_keys = previous_root_keys
		modal.active_handle = handle
		state.modal = modal

		if type(handle.on_close) == "function" then
			handle.on_close(function()
				restore_modal_keys(handle)
			end)
		end
	end

	-- =========================================================================
	-- Public API
	-- =========================================================================

	local api = {}

	function api.is_open()
		clear_handle_if_closed()

		local handle = state.handle
		return type(handle.is_open) == "function" and handle.is_open() or false
	end

	function api.close()
		clear_handle_if_closed()

		local handle = state.handle
		if type(handle.close) == "function" then
			handle.close()
		end

		state.handle = {}
	end

	function api.open(open_opts)
		open_opts = open_opts or {}

		if api.is_open() then
			api.close()
			return nil
		end

		assert(Container and type(Container.build) == "function", "launchers.session.controller: container.build fehlt")
		assert(Layout and type(Layout.build_row) == "function", "launchers.session.controller: layout.build_row fehlt")

		local icon_button_fn = Icons and Icons.mk_icon_button or nil

		local lib = type(resolve_lib) == "function" and (resolve_lib() or {}) or {}
		local Popup = lib.popup
		local Button = lib.button
		local Actions = lib.actions

		assert(Popup and type(Popup.show) == "function", "launchers.session.controller: popup.show fehlt")
		assert(Button and type(Button.mk_button) == "function", "launchers.session.controller: button.mk_button fehlt")
		assert(Actions and type(Actions.bind) == "function", "launchers.session.controller: actions.bind fehlt")

		local conf = open_opts.cfg or cfg()
		local th = resolve_theme(conf, open_opts.theme)
		local d = resolve_dims(th)

		local variant = resolve_variant(open_opts)
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
			local current = handle or state.handle
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
			title = open_opts.title or header_title,
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

		state.handle = handle or {}

		local modkey = ((conf or {}).input or {}).modkey or "Mod4"
		if handle then
			install_modal_keys(modkey, handle, variant)
		end

		local bound = Actions.bind({
			handle = handle,
			actions = actions,
		}) or {}

		act_cancel = bound[cancel_id] or act_cancel

		return handle
	end

	return api
end

return M
