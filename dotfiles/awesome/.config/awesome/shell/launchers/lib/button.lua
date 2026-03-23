-- ~/.config/awesome/shell/launchers/lib/button.lua
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")

local B = {}

local runtime_api = {}

local STYLE = {
	height = 24,
	width = 96,
	radius = 4,
	pad_h = 14,
	font = "Sans 11",

	fg = "black",
	base_bg = "white",
	transparent = "#00000000",

	outer_bw = 1,
	outer_col = "black",

	hover_col = "primary",
	hover_bw = 1,
	hover_inset = 2,
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function ui_api()
	return runtime_api.ui or {}
end

local function ui_helpers()
	return ui_api().helpers or {}
end

local function ui_colors()
	local colors = ui_api().colors or {}

	if type(colors.get) == "function" then
		local ok, palette = pcall(colors.get)
		if ok and type(palette) == "table" then
			return colors, palette
		end
	end

	return colors, colors
end

local function resolve_color(value)
	local colors_mod, palette = ui_colors()

	if type(colors_mod.resolve) == "function" then
		local ok, resolved = pcall(colors_mod.resolve, value)
		if ok and resolved ~= nil then
			return resolved
		end
	end

	if type(value) == "string" and palette[value] then
		return palette[value]
	end

	return value
end

local function hex_to_rgb01(hex)
	local colors_mod, _ = ui_colors()
	local Helpers = ui_helpers()

	if type(colors_mod.hex_to_rgb01) == "function" then
		local ok, r, g, b = pcall(colors_mod.hex_to_rgb01, hex)
		if ok then
			return r, g, b
		end
	end

	if type(Helpers.hex_to_rgb01) == "function" then
		return Helpers.hex_to_rgb01(hex)
	end

	hex = tostring(resolve_color(hex) or "#000000"):gsub("^#", "")

	if #hex == 3 then
		hex = hex:gsub(".", "%1%1")
	end

	if #hex == 8 then
		hex = hex:sub(1, 6)
	end

	if #hex ~= 6 then
		return 0, 0, 0
	end

	local r = tonumber(hex:sub(1, 2), 16) or 0
	local g = tonumber(hex:sub(3, 4), 16) or 0
	local b = tonumber(hex:sub(5, 6), 16) or 0

	return r / 255, g / 255, b / 255
end

local function resolved_style(style_override)
	local S = setmetatable(style_override or {}, { __index = STYLE })

	return {
		height = S.height,
		width = S.width,
		radius = S.radius,
		pad_h = S.pad_h,
		font = S.font,

		fg = resolve_color(S.fg),
		base_bg = resolve_color(S.base_bg),
		transparent = resolve_color(S.transparent),

		outer_bw = S.outer_bw,
		outer_col = resolve_color(S.outer_col),

		hover_col = resolve_color(S.hover_col),
		hover_bw = S.hover_bw,
		hover_inset = S.hover_inset,
	}
end

-- =========================================================================
-- Public API
-- =========================================================================

function B.init(args)
	args = args or {}
	runtime_api = args.api or args or {}
	return B
end

function B.mk_button(label, on_click, style_override)
	local S = resolved_style(style_override)

	-- ---------------------------------------------------------------------
	-- Content
	-- ---------------------------------------------------------------------

	local txt = wibox.widget({
		text = label or "Button",
		font = S.font,
		align = "center",
		valign = "center",
		widget = wibox.widget.textbox,
	})

	local inner_r = math.max(0, (S.radius or 0) - (S.outer_bw or 0))

	local base = wibox.widget({
		{
			{
				txt,
				halign = "center",
				valign = "center",
				widget = wibox.container.place,
			},
			left = S.pad_h,
			right = S.pad_h,
			widget = wibox.container.margin,
		},
		bg = S.base_bg,
		fg = S.fg,
		shape = function(cr, w, h)
			gears.shape.rounded_rect(cr, w, h, inner_r)
		end,
		shape_clip = true,
		widget = wibox.container.background,
	})

	-- ---------------------------------------------------------------------
	-- Hover Overlay
	-- ---------------------------------------------------------------------

	local overlay = wibox.widget.base.make_widget()
	overlay._hover_on = false

	function overlay:fit(_, w, h)
		return w, h
	end

	function overlay:draw(_, cr, w, h)
		if not self._hover_on then
			return
		end

		local inset = (S.outer_bw or 0) + (S.hover_inset or 0)
		local ww = w - inset * 2
		local hh = h - inset * 2

		if ww <= 0 or hh <= 0 then
			return
		end

		cr:save()
		cr:translate(inset, inset)

		local r = math.max(0, inner_r - (S.hover_inset or 0))
		gears.shape.rounded_rect(cr, ww, hh, r)

		local rr, gg, bb = hex_to_rgb01(S.hover_col)
		cr:set_source_rgb(rr, gg, bb)
		cr:set_line_width(S.hover_bw or 1)
		cr:stroke()
		cr:restore()
	end

	-- ---------------------------------------------------------------------
	-- Stack
	-- ---------------------------------------------------------------------

	local inner_stack = wibox.widget({
		base,
		overlay,
		layout = wibox.layout.stack,
	})

	local bordered = wibox.widget({
		{
			inner_stack,
			left = S.outer_bw or 0,
			right = S.outer_bw or 0,
			top = S.outer_bw or 0,
			bottom = S.outer_bw or 0,
			widget = wibox.container.margin,
		},
		shape = function(cr, w, h)
			gears.shape.rounded_rect(cr, w, h, S.radius or 0)
		end,
		shape_clip = true,
		shape_border_width = S.outer_bw or 1,
		shape_border_color = S.outer_col,
		bg = S.transparent,
		widget = wibox.container.background,
	})

	local fixed_h = wibox.widget({
		bordered,
		strategy = "exact",
		height = S.height,
		widget = wibox.container.constraint,
	})

	local fixed_size = wibox.widget({
		fixed_h,
		strategy = "exact",
		width = S.width,
		widget = wibox.container.constraint,
	})

	local root = wibox.widget({
		fixed_size,
		bg = S.transparent,
		widget = wibox.container.background,
	})

	-- ---------------------------------------------------------------------
	-- Interaction
	-- ---------------------------------------------------------------------

	local function set_hover(on)
		overlay._hover_on = not not on
		overlay:emit_signal("widget::redraw_needed")
	end

	root:connect_signal("mouse::enter", function()
		set_hover(true)
	end)

	root:connect_signal("mouse::leave", function()
		set_hover(false)
	end)

	root:buttons(gears.table.join(awful.button({}, 1, function()
		if type(on_click) == "function" then
			on_click()
		end
	end)))

	function root:set_focus(on)
		set_hover(on)
	end

	function root:activate()
		if type(on_click) == "function" then
			on_click()
		end
	end

	set_hover(false)
	return root
end

return B
