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

	-- ---- Search prompt -----------------------------------------------------
	local on_search = opts.on_search
	local prompt = awful.widget.prompt({
		bg = t.search_bg or t.footer_bg or t.bg or "#455A64",
		fg = t.search_fg or t.footer_fg or t.fg or "#FFFFFF",
		cursor_color = t.search_cursor or "#FFFFFF",
	})

	local placeholder = wibox.widget({
		markup = "<span foreground='"
			.. (t.search_fg or t.footer_fg or t.fg or "#FFFFFF")
			.. "99'>"
			.. (t.search_placeholder or "Search…")
			.. "</span>",
		valign = "center",
		widget = wibox.widget.textbox,
	})

	-- prompt MUSS als Widget rein (nicht prompt.widget)
	local stack = wibox.widget({
		{ prompt, visible = false, id = "prompt", widget = wibox.container.place },
		{ placeholder, visible = true, id = "hint", widget = wibox.container.place },
		layout = wibox.layout.stack,
	})

	local function focus_search()
		local prompt_node = stack:get_children_by_id("prompt")[1]
		local hint_node = stack:get_children_by_id("hint")[1]

		prompt_node.visible = true
		hint_node.visible = false

		-- funktionale API + textbox = prompt.widget  (wichtig für Fokus)
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
				hint_node.visible = true
			end,
		})
	end

	-- Optisch sichtbare Box + Klick-Hotspot
	local search_box = wibox.widget({
		{
			{ stack, left = 10, right = 10, top = 2, bottom = 2, widget = wibox.container.margin },
			shape = (t.shape or gears.shape.rounded_rect),
			shape_clip = true,
			bg = t.search_bg or t.footer_bg or t.bg or "#455A64",
			fg = t.search_fg or t.footer_fg or t.fg or "#FFFFFF",
			forced_height = t.search_h or 32,
			forced_width = t.search_w or 420,
			widget = wibox.container.background,
		},
		buttons = gears.table.join(awful.button({}, 1, focus_search)),
		layout = wibox.layout.fixed.horizontal,
	})

	-- ---- Power buttons ----------------------------------------------------
	local powers = { layout = wibox.layout.fixed.horizontal, spacing = t.power_group_spacing or 8 }
	for _, p in ipairs(opts.power_items or {}) do
		table.insert(powers, P.power_button(p, t))
	end
	local powers_right = wibox.widget({ powers, halign = "right", widget = wibox.container.place })

	-- ---- Row + container --------------------------------------------------
	local row = wibox.widget({
		search_box, -- links: Suche
		nil, -- Mitte: Spacer
		powers_right, -- rechts: Power-Buttons
		expand = "outside",
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
