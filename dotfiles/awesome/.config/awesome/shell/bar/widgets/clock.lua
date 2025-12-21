-- ~/.config/awesome/shell/widgets/clock.lua
local wibox = require("wibox")
local awful = require("awful")
local beautiful = require("beautiful")

local M = {}

function M.build(s, fmt)
	local bar_h = tonumber(beautiful.wibar_height) or 28
	local pad_h = tonumber(beautiful.clock_pad_h) or 6
	local pad_v = tonumber(beautiful.clock_pad_v) or 0
	local bg = beautiful.clock_bg or beautiful.wibar_bg
	local fg = beautiful.clock_fg or beautiful.wibar_fg

	-- Format/Refresh aus Theme (fmt kann überschreiben)
	local format = fmt or beautiful.clock_format or "%H:%M"
	local refresh = tonumber(beautiful.clock_refresh) or 1

	-- Textclock (explizit mit table-ctor, damit refresh greift)
	local clock = wibox.widget({
		format = format,
		refresh = refresh,
		widget = wibox.widget.textclock,
	})

	-- ---------- Kalender nur wenn aktiviert ----------
	if beautiful.clock_calendar_enable ~= false then
		local use_menu = (beautiful.clock_calendar_use_menu_theme ~= false)

		-- Farben: entweder Menü-Theme bevorzugen oder direkt die clock_calendar_* Werte
		local bg_norm = use_menu and (beautiful.menu_bg_normal or beautiful.clock_calendar_bg)
			or beautiful.clock_calendar_bg
			or "#FFF7E6"
		local fg_norm = use_menu and (beautiful.menu_fg_normal or beautiful.clock_calendar_fg)
			or beautiful.clock_calendar_fg
			or "#000000"
		local bg_focus = use_menu and (beautiful.menu_bg_focus or beautiful.clock_calendar_focus)
			or beautiful.clock_calendar_focus
			or "#F2E7CF"
		local fg_focus = use_menu and (beautiful.menu_fg_focus or beautiful.clock_calendar_fg)
			or beautiful.clock_calendar_fg
			or "#000000"
		local bcol = use_menu and (beautiful.menu_border_color or beautiful.clock_calendar_border_color)
			or beautiful.clock_calendar_border_color
			or "#000000"
		local bw = tonumber(beautiful.clock_calendar_border_width) or 0

		-- Platzierung aus Theme mappen
		local placement = beautiful.clock_calendar_placement or "bottom_right"
		local pos_map = { top_left = "tl", top_right = "tr", bottom_left = "bl", bottom_right = "br" }
		local attach_pos = pos_map[placement] or "br"

		local cal = awful.widget.calendar_popup.month({
			screen = s,
			spacing = 4,
			start_sunday = false,
			long_weekdays = true,
			week_numbers = false,

			-- Monat/Container
			style_month = {
				bg_color = bg_norm,
				fg_color = fg_norm,
				padding = 8,
				border_width = bw, -- aus Theme (Default 0)
				border_color = bcol,
			},
			-- Kopfzeile (Monatsname/Navigation)
			style_header = {
				bg_color = bg_norm,
				fg_color = fg_norm,
				border_width = 0,
			},
			-- Wochentags-Köpfe (Mo, Di, …)
			style_weekday = {
				bg_color = bg_norm,
				fg_color = fg_norm,
				border_width = 0,
			},
			-- Normale Zellen (Tage)
			style_normal = {
				bg_color = bg_norm,
				fg_color = fg_norm,
				border_width = 0,
				shape_border_width = 0,
			},
			-- Fokus-Tag (heute/selektiert)
			style_focus = {
				bg_color = bg_focus,
				fg_color = fg_focus,
				border_width = 0,
				shape_border_width = 0,
				markup = function(t)
					return "<b>" .. t .. "</b>"
				end,
			},
			-- Kalenderwochen-Spalte (falls aktiviert)
			style_weeknumbers = {
				bg_color = bg_norm,
				fg_color = fg_norm,
				border_width = 0,
			},
		})

		-- An die Clock attachen (du kannst auch den ganzen Block attachen; beides ok)
		cal:attach(clock, attach_pos)
	end

	-- ---------- Verpacken / Farbebene ----------
	local placed = wibox.widget({
		clock,
		widget = wibox.container.place,
		halign = "center",
		valign = "center",
	})

	local with_margin = wibox.widget({
		placed,
		left = pad_h,
		right = pad_h,
		top = pad_v,
		bottom = pad_v,
		widget = wibox.container.margin,
	})

	local bg_block = wibox.widget({
		with_margin,
		fg = fg,
		bg = bg,
		widget = wibox.container.background,
	})

	return wibox.container.constraint(bg_block, "exact", nil, bar_h)
end

return M
