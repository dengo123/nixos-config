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
	-- ---- Minimal: Inline Search Bar (mit awful.widget.prompt) -----------------
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

	-- Prompt + sichtbare Textbox des Prompts
	local prompt = awful.widget.prompt({})
	local textbox = prompt.widget

	-- Optik: weißes Feld, schwarze Schrift, transparenter Cursor
	textbox.bg = "#FFFFFF"
	textbox.fg = "#000000"
	textbox.bg_cursor = "#00000000"
	textbox.fg_cursor = "#000000"
	if textbox.set_align then
		textbox:set_align("left")
	end
	if textbox.set_valign then
		textbox:set_valign("center")
	end
	if textbox.set_text then
		textbox:set_text("")
	end

	-- Innenabstände + weißer Hintergrund
	local inner_margin = wibox.widget({
		textbox,
		left = 10,
		right = 10,
		top = 4,
		bottom = 4,
		widget = wibox.container.margin,
	})
	local bg_box = wibox.widget({
		inner_margin,
		bg = "#FFFFFF",
		fg = "#000000",
		widget = wibox.container.background,
	})

	-- Geometrie
	local FIX_W = 180
	bg_box.forced_width = FIX_W

	local derived_h = math.max(16, math.floor((t.footer_h or 48) / 3 + 0.5))
	local height_ctl = wibox.widget({
		bg_box,
		strategy = "exact",
		height = derived_h,
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
		width = FIX_W,
		widget = wibox.container.constraint,
	})
	local hleft = wibox.widget({
		width_ctl,
		halign = "left",
		widget = wibox.container.place,
	})

	-- Zustand + robuste Stop-Funktion (Keyboard immer freigeben)
	local prompt_running = false
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
	end

	-- Prompt starten (einen Tick verzögert → keine Grabber-Kollision)
	local function start_prompt()
		stop_prompt()
		gears.timer.delayed_call(function()
			prompt_running = true

			-- redundante Farbsicherheit am Prompt selbst
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
					stop_prompt()
				end,

				done_callback = function()
					stop_prompt()
				end,

				keypressed_callback = function(mod, key)
					if (not mod or #mod == 0) and key == "Escape" then
						stop_prompt()
						return true
					end
					return false
				end,
			})
		end)
	end

	-- Klick → Prompt starten
	local search_box = wibox.widget({
		hleft,
		buttons = gears.table.join(awful.button({}, 1, function()
			start_prompt()
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
				start_prompt()
			end,
			cancel_search = function()
				stop_prompt()
			end, -- <— NEU: erzwingt Freigabe
			is_search_active = function()
				return prompt_running
			end,
		}
end

return Footer
