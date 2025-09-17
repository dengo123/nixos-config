-- ~/.config/awesome/features/shell/menu/components/footer.lua
local gears = require("gears")
local awful = require("awful")
local wibox = require("wibox")

local P = require("features.shell.menu.widgets") -- Widgets (list, buttons, power_bar)
local Dialogs = require("features.shell.menu.dialogs") -- Power/Logout Dialoge
local Search = require("features.shell.menu.search") -- Search-Orchestrator

local Footer = {}

-- Kompatibel:
--   Footer.build(power_items, t)
--   Footer.build({ power_items = ..., on_search = ..., t = ... })
function Footer.build(arg1, arg2)
	-- ---- Compat Layer ---------------------------------------------------------
	local opts
	if type(arg1) == "table" and (arg1.power_items or arg1.on_search or arg1.t) then
		opts = arg1
	else
		opts = { power_items = arg1, t = arg2 }
	end
	opts.t = opts.t or {}
	local t = opts.t

	-- ---------------------------------------------------------------------------
	-- Footer-Grundlayout (nur Rahmen/Container – kein Search-Theming hier!)
	-- ---------------------------------------------------------------------------
	local FOOTER_H = t.footer_h or 48
	local PAD_T = t.footer_pad_t or 6
	local PAD_B = t.footer_pad_b or 6
	local inner_h = math.max(FOOTER_H - PAD_T - PAD_B, 1)

	local FOOTER_BG = t.footer_bg or t.bg or "#235CDB"
	local FOOTER_FG = t.footer_fg or t.fg or "#FFFFFF"

	-- ---------------------------------------------------------------------------
	-- Search: neutrale Host-Widgets (Styling übernimmt Search.init / theme.lua)
	-- ---------------------------------------------------------------------------
	local prompt = awful.widget.prompt({}) -- neutral; Farben/Cursor setzt die Search

	-- Margin direkt um die Textbox – wird von Search thematisiert
	local inner_margin = wibox.widget({
		prompt,
		id = "inner_margin",
		widget = wibox.container.margin,
	})

	-- Stack: Platzhalter mit id "prompt" (Search blendet ein/aus)
	local stack = wibox.widget({
		{
			inner_margin,
			id = "prompt",
			visible = false,
			halign = "left",
			valign = "center",
			widget = wibox.container.place,
		},
		layout = wibox.layout.stack,
	})

	-- Hintergrundbox (Farbe/Breite setzt Search)
	local bg_box = wibox.widget({
		{ stack, widget = wibox.container.margin }, -- neutral; Search setzt Margins/Farben
		shape = gears.shape.rectangle,
		shape_clip = true,
		bg = "#00000000",
		widget = wibox.container.background,
	})

	-- Höhe der Suchleiste (wird von Search gesetzt)
	local height_ctl = wibox.widget({
		bg_box,
		strategy = "exact",
		widget = wibox.container.constraint,
	})

	-- Vertikal mittig im Footer
	local vcenter = wibox.widget({
		height_ctl,
		valign = "center",
		widget = wibox.container.place,
	})

	-- Kollaps-/Expand-Breite (setzt Search)
	local width_ctl = wibox.widget({
		vcenter,
		strategy = "exact",
		widget = wibox.container.constraint,
	})

	-- Search-Controller aufbauen: alles Theming/Verhalten kommt von dort
	local search_ctl = Search.build({
		-- Host-Refs
		prompt_widget = prompt.widget, -- die eigentliche Textbox
		prompt_node = stack, -- Container mit Kind-ID "prompt"
		inner_margin = inner_margin, -- für pad_l/r/t/b aus Search-Theme
		bg_box = bg_box,
		width_ctl = width_ctl,
		height_ctl = height_ctl,

		-- Theme-Ableitung: gib dein globales Menü-Theme durch
		shared_theme = t, -- Search.theme.from_shared(t, ...) nutzt search_* Keys

		-- Verhalten/History
		history_path = t.history_path,
		on_search = opts.on_search,
	})

	-- Klick auf die Searchbox → Fokus (Search entscheidet expand/collapse)
	local search_box = wibox.widget({
		width_ctl,
		buttons = gears.table.join(awful.button({}, 1, function()
			search_ctl:focus()
		end)),
		layout = wibox.layout.fixed.horizontal,
	})

	-- ---------------------------------------------------------------------------
	-- Power-Buttons rechts (komplett in widgets.lua gelöst)
	-- ---------------------------------------------------------------------------
	local powers_right = P.power_bar(opts.power_items or {}, t, {
		inner_h = inner_h,
		dialogs = Dialogs,
	})

	-- ---------------------------------------------------------------------------
	-- Footer-Row & Container
	-- ---------------------------------------------------------------------------
	local row = wibox.widget({
		search_box,
		nil,
		powers_right, -- rechts
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

	-- ---------------------------------------------------------------------------
	-- API (für Keybinds/Signals) – alle Calls mit ':' (Colon!)
	-- ---------------------------------------------------------------------------
	return footer,
		{
			focus_search = function()
				search_ctl:focus()
			end,
			cancel_search = function()
				search_ctl:cancel()
			end,
			is_search_active = function()
				return search_ctl:is_active()
			end,
		}
end

return Footer
