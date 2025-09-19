-- ~/.config/awesome/features/shell/menu/parts/footer.lua
local gears = require("gears")
local awful = require("awful")
local wibox = require("wibox")

local P = require("features.shell.menu.widgets") -- Widgets (list, buttons, power_bar)
local Dialogs = require("features.shell.menu.dialogs") -- Power/Logout Dialoge

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
	-- ---- Minimal: Inline Search Bar (collapsed/expanded + outside click) -----
	local function urlencode(str)
		if not str then
			return ""
		end
		str = str:gsub("\n", " ")
		str = str:gsub("([^%w%-%_%.%~ ])", function(c)
			return string.format("%%%02X", string.byte(c))
		end)
		return str:gsub(" ", "+")
	end

	-- Größen
	local FIX_W = 180 -- expandierte Breite
	local HIT_W = 180 -- kollabierte Klickfläche
	local FOOT_H = t.footer_h or 48
	local H = math.max(16, math.floor(FOOT_H / 3 + 0.5))

	-- Prompt + sichtbare Textbox
	local prompt = awful.widget.prompt({})
	local textbox = prompt.widget
	if textbox.set_align then
		textbox:set_align("left")
	end
	if textbox.set_valign then
		textbox:set_valign("center")
	end
	if textbox.set_text then
		textbox:set_text("")
	end
	textbox.bg = "#FFFFFF"
	textbox.fg = "#000000"
	textbox.bg_cursor = "#00000000" -- kein Blockcursor
	textbox.fg_cursor = "#000000"

	-- Innenabstand
	local inner_margin = wibox.widget({
		textbox,
		left = 10,
		right = 10,
		top = 4,
		bottom = 4,
		widget = wibox.container.margin,
	})

	-- Weißes Rechteck (sichtbare Fläche)
	local bg_box = wibox.widget({
		inner_margin,
		bg = "#FFFFFF",
		fg = "#000000",
		widget = wibox.container.background,
	})

	-- Höhe fixieren
	local height_ctl = wibox.widget({
		bg_box,
		strategy = "exact",
		height = H,
		widget = wibox.container.constraint,
	})

	-- Vertikal mittig
	local vcenter = wibox.widget({
		height_ctl,
		valign = "center",
		widget = wibox.container.place,
	})

	-- Breite per constraint
	local width_ctl = wibox.widget({
		vcenter,
		strategy = "exact",
		width = FIX_W, -- wird dynamisch gesetzt
		widget = wibox.container.constraint,
	})

	-- Links im Row-Slot
	local hleft = wibox.widget({
		width_ctl,
		halign = "left",
		widget = wibox.container.place,
	})

	-- Äußeres Widget der Bar (KLICKFLÄCHE = GANZE BAR)
	local search_box = wibox.widget({
		hleft,
		layout = wibox.layout.fixed.horizontal,
	})

	-- Zustand
	local prompt_running = false
	local collapsed = true
	local saved_root_buttons = nil
	local ignore_next_root_click = false

	local function detach_root_click_watcher()
		if saved_root_buttons then
			root.buttons(saved_root_buttons)
			saved_root_buttons = nil
		end
	end

	local function attach_root_click_watcher()
		if not saved_root_buttons then
			saved_root_buttons = root.buttons()
		end
		root.buttons(gears.table.join(awful.button({}, 1, function()
			-- den unmittelbar vorigen Klick aus der Bar ignorieren
			if ignore_next_root_click then
				return
			end
			if prompt_running then
				pcall(function()
					awful.keygrabber.stop()
				end)
			end
			prompt_running = false
			-- kollabieren
			bg_box.bg = "#00000000"
			inner_margin.left, inner_margin.right = 0, 0
			inner_margin.top, inner_margin.bottom = 0, 0
			bg_box.forced_width = HIT_W
			width_ctl.width = HIT_W
			width_ctl:emit_signal("widget::layout_changed")
			collapsed = true
			detach_root_click_watcher()
		end)))
	end

	local function apply_collapsed_style()
		collapsed = true
		bg_box.bg = "#00000000" -- transparent im collapsed
		inner_margin.left, inner_margin.right = 0, 0
		inner_margin.top, inner_margin.bottom = 0, 0
		bg_box.forced_width = HIT_W
		width_ctl.width = HIT_W
		width_ctl:emit_signal("widget::layout_changed")
	end

	local function apply_expanded_style()
		collapsed = false
		bg_box.bg = "#FFFFFF"
		inner_margin.left, inner_margin.right = 10, 10
		inner_margin.top, inner_margin.bottom = 4, 4
		bg_box.forced_width = FIX_W
		width_ctl.width = FIX_W
		width_ctl:emit_signal("widget::layout_changed")
	end

	local function stop_prompt()
		if prompt_running then
			prompt_running = false
		end
		pcall(function()
			awful.keygrabber.stop()
		end)
		gears.timer.delayed_call(function()
			pcall(function()
				awful.keygrabber.stop()
			end)
		end)
		pcall(function()
			if textbox.set_text then
				textbox:set_text("")
			end
		end)
		detach_root_click_watcher()
	end

	local function collapse()
		stop_prompt()
		apply_collapsed_style()
	end

	local function expand_and_start()
		stop_prompt()
		apply_expanded_style()
		attach_root_click_watcher()

		-- einen Tick verzögert starten → verhindert Grabber-Kollisionen
		gears.timer.delayed_call(function()
			prompt_running = true
			pcall(function()
				prompt.bg = "#FFFFFF"
			end)
			pcall(function()
				prompt.fg = "#000000"
			end)
			pcall(function()
				prompt.bg_cursor = "#00000000"
			end)
			pcall(function()
				prompt.fg_cursor = "#000000"
			end)

			awful.prompt.run({
				prompt = "",
				textbox = textbox,
				history_path = nil,
				completion_callback = nil,

				exe_callback = function(q)
					q = (q or ""):match("^%s*(.-)%s*$")
					if q ~= "" then
						awful.spawn({ "firefox", "https://duckduckgo.com/?q=" .. urlencode(q) }, false)
					end
					collapse()
				end,
				done_callback = function()
					collapse()
				end,
				keypressed_callback = function(mod, key)
					if (not mod or #mod == 0) and key == "Escape" then
						collapse()
						return true
					end
					return false
				end,
			})
		end)
	end

	-- Klickfläche = GANZE BAR (search_box)
	local function mark_inner_click_and_expand()
		ignore_next_root_click = true
		gears.timer.delayed_call(function()
			ignore_next_root_click = false
		end)
		expand_and_start()
	end
	search_box:buttons(gears.table.join(awful.button({}, 1, mark_inner_click_and_expand)))

	-- Startzustand: kollabiert
	apply_collapsed_style()

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
				expand_and_start()
			end,
			cancel_search = function()
				collapse()
			end, -- von base.lua:on_hide() aufgerufen
			is_search_active = function()
				return prompt_running
			end,
			is_collapsed = function()
				return collapsed
			end,
		}
end

return Footer
