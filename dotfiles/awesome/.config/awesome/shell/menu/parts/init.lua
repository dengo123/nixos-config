-- ~/.config/awesome/shell/menu/parts/init.lua
local awful = require("awful")
local wibox = require("wibox")
local gears = require("gears")

local Parts = {}
Parts.widgets = require("shell.menu.parts.widgets")
Parts.containers = require("shell.menu.parts.containers")

-- >>> ganze Lib ziehen (focus, actions, placement, term, items …)
local Lib = require("shell.menu.lib")

local function pick(...)
	for i = 1, select("#", ...) do
		local v = select(i, ...)
		if v ~= nil then
			return v
		end
	end
end

-- opts:
--   title, theme, container="power"
--   dims = { w, h, header_h, footer_h, body_h, pad_h, pad_v }   <-- Pflicht
--   build_body = function(th, dims, parts) -> body_widget, focus_items, required_w?
--   popup = { width?, height?, placement?, use_backdrop? }
--   focus = { mouse_follow=true/false }

function Parts.open(opts)
	opts = opts or {}
	local th = opts.theme or {}
	local dims = assert(opts.dims, "parts.open: opts.dims erforderlich (w,h,header_h,footer_h,body_h)")
	assert(dims.w and dims.h and dims.header_h and dims.footer_h and dims.body_h, "parts.open: dims unvollständig")

	-- Body aus Dialog bauen
	assert(type(opts.build_body) == "function", "parts.open: build_body(th,dims,parts) erforderlich")
	local body_core, focus_items, required_w = opts.build_body(th, dims, Parts)
	focus_items = focus_items or {}

	-- >>> Fokusfähige Widgets aus der (flachen) Liste filtern
	local function collect_focus(list)
		local out = {}
		for _, w in ipairs(list or {}) do
			if w and (type(w.set_focus) == "function" or type(w.activate) == "function") then
				table.insert(out, w)
			end
		end
		return out
	end
	focus_items = collect_focus(focus_items)

	-- Cancel
	local cancel_inner = Parts.widgets.mk_cancel_button(th.cancel_label or "Cancel", nil, th)
	local cancel_btn = wibox.widget({ cancel_inner, bg = "#00000000", widget = wibox.container.background })

	-- Header
	local header = wibox.widget({
		{
			{ markup = string.format("<b>%s</b>", opts.title or ""), widget = wibox.widget.textbox },
			left = dims.pad_h,
			right = dims.pad_h,
			widget = wibox.container.margin,
		},
		bg = pick(th.header_bg, "#235CDB"),
		fg = pick(th.header_fg, "#FFFFFF"),
		widget = wibox.container.background,
	})

	-- Body
	local body = wibox.widget({
		{
			{ body_core, halign = "center", valign = "center", widget = wibox.container.place },
			left = dims.pad_h,
			right = dims.pad_h,
			top = dims.pad_v,
			bottom = dims.pad_v,
			widget = wibox.container.margin,
		},
		bg = pick(th.body_bg, "#00000000"),
		fg = pick(th.body_fg, "#FFFFFF"),
		widget = wibox.container.background,
	})

	-- Footer mit Cancel rechts
	local footer_right = wibox.widget({
		{ cancel_btn, halign = "right", valign = "center", widget = wibox.container.place },
		right = dims.pad_h,
		widget = wibox.container.margin,
	})
	local footer = wibox.widget({
		{ footer_right, top = dims.pad_v, bottom = dims.pad_v, widget = wibox.container.margin },
		bg = pick(th.footer_bg, "#235CDB"),
		fg = pick(th.footer_fg, "#FFFFFF"),
		widget = wibox.container.background,
	})

	local stack = Parts.containers.build(opts.container or "power", th, dims, {
		header = wibox.widget({
			header,
			strategy = "exact",
			height = dims.header_h,
			widget = wibox.container.constraint,
		}),
		body = wibox.widget({
			body,
			strategy = "exact",
			height = dims.body_h,
			widget = wibox.container.constraint,
		}),
		footer = wibox.widget({
			footer,
			strategy = "exact",
			height = dims.footer_h,
			widget = wibox.container.constraint,
		}),
	})

	-- Popup-Args (keine Keygrabber-Optionen – ESC kommt aus focus.lua)
	local p = opts.popup or {}
	local popup_args = {
		width = p.width or (required_w and required_w > 0 and required_w or nil),
		height = p.height or dims.h,
		placement = p.placement or awful.placement.centered,
		use_backdrop = (p.use_backdrop ~= false), -- default: true
		group = "dialogs",
	}

	-- >>> Popup anzeigen (aus Parts.containers.popup)
	local handle = Parts.containers.popup.show(stack, th, popup_args)

	-- Cancel Verhalten/Fokus
	if not cancel_btn.set_focus then
		function cancel_btn:set_focus(on)
			local on_bg = pick(th.cancel_bg_hover, th.row_bg_hover, th.bg_focus, "#FFFFFF22")
			local off_bg = pick(th.cancel_bg, "#00000000")
			self.bg = on and on_bg or off_bg
		end
	end
	cancel_btn:buttons(gears.table.join(awful.button({}, 1, function()
		handle.close()
	end)))
	if not cancel_btn.activate then
		function cancel_btn:activate()
			handle.close()
		end
	end
	cancel_btn.mouse_enter_target = cancel_btn

	-- Fokus (linear) über die Lib
	table.insert(focus_items, cancel_btn)
	if Lib and Lib.focus and type(Lib.focus.attach) == "function" then
		local stop = Lib.focus.attach(
			focus_items,
			th,
			{ handle = handle, mouse_follow = (not opts.focus) or (opts.focus.mouse_follow ~= false) }
		)
		if type(stop) == "function" then
			local oc = handle.close
			handle.close = function(...)
				pcall(stop)
				return oc(...)
			end
		end
	end

	return handle
end

return Parts
