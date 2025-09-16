-- features/shell/menu/components/footer.lua
local gears = require("gears")
local awful = require("awful")
local wibox = require("wibox")
local P = require("features.shell.menu.parts.widgets")
local Dialogs = require("features.shell.menu.dialogs")
local Search = require("features.shell.menu.search")

local Footer = {}

-- Kompatibel:
--   Footer.build(power_items, t)
--   Footer.build({ power_items = ..., on_search = ..., t = ... })
function Footer.build(arg1, arg2)
	-- ---- Compat Layer -------------------------------------------------------
	local opts
	if type(arg1) == "table" and (arg1.power_items or arg1.on_search or arg1.t) then
		opts = arg1
	else
		opts = { power_items = arg1, t = arg2 }
	end
	opts.t = opts.t or {}
	local t = opts.t

	-- -------------------------------------------------------------------------
	-- Container-Layout (nur Footer-Rahmen; KEIN Search-Theming hier!)
	-- -------------------------------------------------------------------------
	local FOOTER_H = t.footer_h or 48
	local PAD_T = t.footer_pad_t or 6
	local PAD_B = t.footer_pad_b or 6
	local inner_h = math.max(FOOTER_H - PAD_T - PAD_B, 1)
	local SEARCH_H = math.floor(inner_h / 2)

	local FOOTER_BG = t.footer_bg or t.bg or "#235CDB"
	local FOOTER_FG = t.footer_fg or t.fg or "#FFFFFF"

	-- -------------------------------------------------------------------------
	-- Search Host-Widgets (neutral; Styling/Größen macht Search.init)
	-- -------------------------------------------------------------------------
	local prompt = awful.widget.prompt({}) -- kein bg/fg/cursor hier!

	local stack = wibox.widget({
		{
			prompt,
			visible = false,
			id = "prompt",
			widget = wibox.container.place,
			halign = "left",
			valign = "center",
		},
		layout = wibox.layout.stack,
	})

	local bg_box = wibox.widget({
		{
			stack,
			left = 10,
			right = 10,
			top = 2,
			bottom = 2,
			widget = wibox.container.margin,
		},
		shape = gears.shape.rectangle,
		shape_clip = true,
		-- bewusst neutral: Search.init setzt bg/fg/border/width
		bg = "#00000000",
		widget = wibox.container.background,
	})

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

	local width_ctl = wibox.widget({
		vcenter,
		strategy = "exact",
		-- keine feste Breite hier; Search.init setzt width/forced_width
		width = 1,
		widget = wibox.container.constraint,
	})

	-- Alle Search-Details (Theme, collapsed/expanded, prompt.run, cursor, widths) in /search/init.lua
	local search_ctl = Search.build({
		prompt_widget = prompt.widget,
		bg_box = bg_box,
		width_ctl = width_ctl,
		stack = stack,
		history_path = t.history_path,
		on_search = opts.on_search,
		-- start_collapsed = true  -- falls dein Search.init diese Option unterstützt
	})

	-- Klick auf die Searchbox delegiert nur noch an Search
	local search_box = wibox.widget({
		width_ctl,
		buttons = gears.table.join(awful.button({}, 1, function()
			if search_ctl.is_collapsed and search_ctl:is_collapsed() then
				search_ctl:focus()
			end
		end)),
		layout = wibox.layout.fixed.horizontal,
	})

	-- -------------------------------------------------------------------------
	-- Power-Buttons (unverändert)
	-- -------------------------------------------------------------------------
	local powers = { layout = wibox.layout.fixed.horizontal, spacing = 0 }

	for _, p in ipairs(opts.power_items or {}) do
		local t_btn = {}
		for k, v in pairs(t or {}) do
			t_btn[k] = v
		end
		t_btn.defer_power_clicks = true

		local btn = P.power_button(p, t_btn)

		local fitted = wibox.widget({
			btn,
			strategy = "exact",
			height = inner_h,
			widget = wibox.container.constraint,
		})

		local inner = btn._click_target or btn
		inner:buttons({})
		btn:buttons({})

		local raw_text = (p.text or p.label or ""):lower()
		local raw_nospace = raw_text:gsub("%s+", "")
		local key = (p.id or raw_text):lower():gsub("%s+", "")

		local function bind(handler)
			local b = gears.table.join(awful.button({}, 1, handler))
			inner:buttons(b)
			btn:buttons(b)
		end

		local matched = false
		if key == "power" or raw_nospace:find("turnoff") or raw_text:find("shutdown") then
			matched = true
			bind(function()
				Dialogs.power({
					bg = FOOTER_BG,
					fg = FOOTER_FG,
					btn_bg = "#ECECEC",
					btn_fg = "#000000",
					backdrop = "#00000088",
					radius = 6,
				})
			end)
		elseif
			key == "logout"
			or key == "logoff"
			or raw_text:find("logout")
			or raw_text:find("log off")
			or raw_text:find("exit")
		then
			matched = true
			bind(function()
				Dialogs.logout_confirm({
					bg = FOOTER_BG,
					fg = FOOTER_FG,
					btn_bg = "#ECECEC",
					btn_fg = "#000000",
					backdrop = "#00000088",
					radius = 6,
				})
			end)
		end

		if not matched and p.on_press then
			bind(function()
				p.on_press()
			end)
		end

		table.insert(powers, fitted)
	end

	local powers_right = wibox.widget({
		powers,
		halign = "right",
		widget = wibox.container.place,
	})

	-- -------------------------------------------------------------------------
	-- Footer Row & Container
	-- -------------------------------------------------------------------------
	local row = wibox.widget({
		search_box,
		nil,
		powers_right,
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
		forced_height = FOOTER_H,
		bg = FOOTER_BG,
		fg = FOOTER_FG,
		widget = wibox.container.background,
	})

	return footer,
		{
			focus_search = function()
				if search_ctl.focus then
					search_ctl:focus()
				end
			end,
			cancel_search = function()
				if search_ctl.cancel then
					search_ctl:cancel()
				end
			end,
			is_search_active = function()
				return search_ctl.is_active and search_ctl:is_active() or false
			end,
		}
end

return Footer
