-- ~/.config/awesome/features/shell/menu/parts/footer.lua
local gears = require("gears")
local awful = require("awful")
local wibox = require("wibox")
local P = require("features.shell.menu.shared.primitives")

local Footer = {}

-- Kompatibel:
--   Footer.build(power_items, t)
--   Footer.build({ power_items = ..., on_search = ..., t = ... })
function Footer.build(arg1, arg2)
	-- ---- Compat Layer -----------------------------------------------------
	local opts
	if type(arg1) == "table" and (arg1.power_items or arg1.on_search or arg1.t) then
		opts = arg1 -- neue API
	else
		opts = { power_items = arg1, t = arg2 } -- alte API
	end
	opts.t = opts.t or {}
	local t = opts.t

	-- -----------------------------------------------------------------------
	-- Config
	-- -----------------------------------------------------------------------
	local FOOTER_H = t.footer_h or 40 -- Gesamthöhe Footer
	local PAD_T, PAD_B = (t.footer_pad_t or 6), (t.footer_pad_b or 6)
	local inner_h = math.max(FOOTER_H - PAD_T - PAD_B, 1)
	local SEARCH_H = math.floor(inner_h / 2) -- Suche = halbe Footerhöhe

	local HIT_W = t.search_hit_w or 200 -- unsichtbare, klickbare Breite (collapsed)
	local EXPAND_W = t.search_w or 200 -- sichtbare Breite im Fokus (expanded)

	local ROW_SP = t.power_group_spacing or 8

	-- -----------------------------------------------------------------------
	-- Search Prompt (ohne Label/Placeholder)
	-- -----------------------------------------------------------------------
	local on_search = opts.on_search
	local prompt = awful.widget.prompt({
		bg = t.search_bg or t.footer_bg or t.bg or "#455A64",
		fg = t.search_fg or t.footer_fg or t.fg or "#FFFFFF",
		cursor_color = t.search_cursor or "#FFFFFF",
	})

	local stack = wibox.widget({
		{ prompt, visible = false, id = "prompt", widget = wibox.container.place },
		layout = wibox.layout.stack,
	})

	-- Hintergrund-Box für Styles (bekommt nur im expanded State eine Farbe)
	local bg_box = wibox.widget({
		{ stack, left = 10, right = 10, top = 2, bottom = 2, widget = wibox.container.margin },
		shape = (t.shape or gears.shape.rounded_rect),
		shape_clip = true,
		bg = "#00000000", -- transparent wenn collapsed
		fg = t.search_fg or t.footer_fg or t.fg or "#FFFFFF",
		widget = wibox.container.background,
	})

	-- Höhe hart auf halbe Footerhöhe klemmen + vertikal zentrieren
	local height_ctl = wibox.widget({
		bg_box,
		strategy = "exact",
		height = SEARCH_H,
		widget = wibox.container.constraint,
	})
	local vcenter = wibox.widget({
		height_ctl,
		valign = "center",
		widget = wibox.container.place,
	})

	-- Breite per Constraint steuern (collapsed/expanded)
	local width_ctl = wibox.widget({
		vcenter,
		strategy = "exact",
		width = HIT_W, -- initial: nur Hitbox
		widget = wibox.container.constraint,
	})

	local collapsed = true
	local search_active = false

	local function apply_collapsed_style()
		collapsed = true
		bg_box.bg = "#00000000"
		width_ctl.width = HIT_W
		bg_box.forced_width = HIT_W
	end

	local function apply_expanded_style()
		collapsed = false
		bg_box.bg = t.search_bg or t.footer_bg or t.bg or "#455A64"
		width_ctl.width = EXPAND_W
		bg_box.forced_width = EXPAND_W
	end

	local function cancel_search()
		local prompt_node = stack:get_children_by_id("prompt")[1]
		prompt_node.visible = false
		prompt.widget:set_text("")
		search_active = false
		-- Keygrabber sicher stoppen, damit kein Hidden-Input weiterläuft
		pcall(awful.keygrabber.stop)
		apply_collapsed_style()
	end

	local function focus_search()
		local prompt_node = stack:get_children_by_id("prompt")[1]
		apply_expanded_style()
		prompt_node.visible = true
		prompt.widget:set_text("")
		search_active = true

		awful.prompt.run({
			prompt = "", -- <<< kein Label, nur Cursor
			textbox = prompt.widget,
			history_path = t.history_path or (gears.filesystem.get_cache_dir() .. "/menu_search_history"),
			completion_callback = awful.completion.shell,
			exe_callback = function(q)
				if q and #q > 0 and on_search then
					on_search(q)
				end
			end,
			done_callback = function()
				cancel_search()
			end,
		})
	end

	-- Klickbare (unsichtbare) Fläche links im Footer
	local search_box = wibox.widget({
		width_ctl, -- enthält vcenter -> height_ctl -> bg_box -> stack -> prompt
		buttons = gears.table.join(awful.button({}, 1, function()
			if collapsed then
				focus_search()
			end
		end)),
		layout = wibox.layout.fixed.horizontal,
	})

	apply_collapsed_style()

	-- -----------------------------------------------------------------------
	-- Power Buttons rechts
	-- -----------------------------------------------------------------------
	local powers = { layout = wibox.layout.fixed.horizontal, spacing = ROW_SP }
	for _, p in ipairs(opts.power_items or {}) do
		table.insert(powers, P.power_button(p, t))
	end
	local powers_right = wibox.widget({ powers, halign = "right", widget = wibox.container.place })

	-- -----------------------------------------------------------------------
	-- Footer Row & Container
	-- -----------------------------------------------------------------------
	local row = wibox.widget({
		search_box, -- links: Such-Hitbox (unsichtbar bis Fokus)
		nil, -- Mitte: Spacer
		powers_right, -- rechts: Power-Buttons
		expand = "inside",
		layout = wibox.layout.align.horizontal,
	})

	local footer = wibox.widget({
		{
			row,
			left = t.footer_pad_l or 10,
			right = t.footer_pad_r or 10,
			top = PAD_T,
			bottom = PAD_B,
			widget = wibox.container.margin,
		},
		forced_height = FOOTER_H, -- Footer behält seine volle Höhe
		bg = t.footer_bg or t.bg or "#263238",
		fg = t.footer_fg or t.fg or "#FFFFFF",
		widget = wibox.container.background,
	})

	return footer,
		{
			focus_search = focus_search,
			cancel_search = cancel_search,
			is_search_active = function()
				return search_active
			end,
		}
end

return Footer
