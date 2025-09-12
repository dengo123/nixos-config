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

	-- Konfig: unsichtbare Hitbox-Breite (collapsed) & sichtbare Prompt-Breite (expanded)
	local HIT_W = t.search_hit_w or 200 -- unsichtbare, aber klickbare Breite
	local EXPAND_W = t.search_w or 200 -- sichtbare Breite im Fokus
	local ROW_SP = t.power_group_spacing or 8

	-- ---- Search prompt -----------------------------------------------------
	local on_search = opts.on_search
	local prompt = awful.widget.prompt({
		bg = t.search_bg or t.footer_bg or t.bg or "#455A64",
		fg = t.search_fg or t.footer_fg or t.fg or "#FFFFFF",
		cursor_color = t.search_cursor or "#FFFFFF",
	})

	-- Nur der Prompt (kein Placeholder)
	local stack = wibox.widget({
		{ prompt, visible = false, id = "prompt", widget = wibox.container.place },
		layout = wibox.layout.stack,
	})

	local collapsed = true

	-- Hintergrundbox (bekommt Farbe nur im expanded state)
	local bg_box = wibox.widget({
		{ stack, left = 10, right = 10, top = 2, bottom = 2, widget = wibox.container.margin },
		shape = (t.shape or gears.shape.rounded_rect),
		shape_clip = true,
		bg = "#00000000", -- transparent collapsed
		fg = t.search_fg or t.footer_fg or t.fg or "#FFFFFF",
		forced_height = t.search_h or 32,
		widget = wibox.container.background,
	})

	-- Constraint um die Breite in collapsed/expanded zu steuern
	local width_ctl = wibox.widget({
		bg_box,
		strategy = "exact", -- wir setzen die Breite explizit
		width = HIT_W, -- initial: nur Hitbox
		widget = wibox.container.constraint,
	})

	local function apply_collapsed_style()
		collapsed = true
		bg_box.bg = "#00000000" -- transparent
		width_ctl.width = HIT_W -- nur klickbare Fläche
	end

	local function apply_expanded_style()
		collapsed = false
		bg_box.bg = t.search_bg or t.footer_bg or t.bg or "#455A64"
		width_ctl.width = EXPAND_W -- volle sichtbare Breite
	end

	local function focus_search()
		local prompt_node = stack:get_children_by_id("prompt")[1]

		apply_expanded_style()
		prompt_node.visible = true

		awful.prompt.run({
			prompt = t.search_prompt or "Search: ",
			textbox = prompt.widget,
			history_path = t.history_path or (gears.filesystem.get_cache_dir() .. "/menu_search_history"),
			completion_callback = awful.completion.shell,
			exe_callback = function(q)
				if q and #q > 0 and on_search then
					on_search(q)
				end
			end,
			done_callback = function()
				prompt_node.visible = false
				apply_collapsed_style()
			end,
		})
	end

	local search_box = wibox.widget({
		width_ctl, -- enthält bg_box -> stack -> prompt
		buttons = gears.table.join(awful.button({}, 1, function()
			if collapsed then
				focus_search()
			end
		end)),
		layout = wibox.layout.fixed.horizontal,
	})

	-- initial collapsed (unsichtbar, aber klickbar über HIT_W)
	apply_collapsed_style()

	-- ---- Power buttons ----------------------------------------------------
	local powers = { layout = wibox.layout.fixed.horizontal, spacing = ROW_SP }
	for _, p in ipairs(opts.power_items or {}) do
		table.insert(powers, P.power_button(p, t))
	end
	local powers_right = wibox.widget({ powers, halign = "right", widget = wibox.container.place })

	-- ---- Row + container --------------------------------------------------
	local row = wibox.widget({
		search_box, -- links: klickbare (unsichtbare) Hitbox -> öffnet Prompt
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
			top = t.footer_pad_t or 6,
			bottom = t.footer_pad_b or 6,
			widget = wibox.container.margin,
		},
		bg = t.footer_bg or t.bg or "#263238",
		fg = t.footer_fg or t.fg or "#FFFFFF",
		widget = wibox.container.background,
	})

	return footer, { focus_search = focus_search }
end

return Footer
