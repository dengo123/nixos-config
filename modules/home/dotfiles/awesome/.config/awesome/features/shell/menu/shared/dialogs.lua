-- ~/.config/awesome/features/shell/menu/shared/dialogs.lua
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")

local Dialogs = {}

-- =============================== THEME PRESETS ===============================
local XP = {
	header_bg = "#3A6EA5", -- Luna Blue
	header_fg = "#FFFFFF",
	body_bg = "#F0F8FF", -- sehr helles blau/weiß
	body_fg = "#000000",
	footer_bg = "#2B5B88", -- dunkleres Blau für Fußleiste
	footer_fg = "#FFFFFF",
	backdrop = "#00000088",
	radius = 8,

	dialog_min_w = 440,
	dialog_max_w = 520,

	-- große Icon-Kacheln
	tile_w = 110,
	tile_h = 120,
	tile_bg = "#EAF2FB",
	tile_fg = "#000000",
	tile_hover = "#D5E6FA",

	-- Cancel Button
	cancel_bg = "#ECECEC",
	cancel_fg = "#000000",
	cancel_hover_bg = "#DADADA",

	-- Paddings
	pad_h = 16,
	pad_v = 14,
}

-- =============================== HELPERS ====================================
local function clamp(x, a, b)
	return math.max(a, math.min(b, x))
end

local function mk_backdrop(screen, color)
	local backdrop = wibox({
		screen = screen,
		visible = true,
		ontop = true,
		type = "splash",
		bg = color or "#00000088",
	})
	backdrop:geometry(screen.geometry)
	return backdrop
end

local function mk_header(title, opts)
	local box = wibox.widget({
		{
			{
				{
					-- optional: kleines Logo/Emoji links
					{
						text = (opts.logo_text or ""), -- z.B. "🪟"
						align = "left",
						valign = "center",
						widget = wibox.widget.textbox,
					},
					{
						markup = "<b>" .. (title or "") .. "</b>",
						align = "left",
						valign = "center",
						widget = wibox.widget.textbox,
					},
					spacing = 8,
					layout = wibox.layout.fixed.horizontal,
				},
				left = opts.pad_h or XP.pad_h,
				right = opts.pad_h or XP.pad_h,
				widget = wibox.container.margin,
			},
			bg = opts.header_bg or XP.header_bg,
			fg = opts.header_fg or XP.header_fg,
			widget = wibox.container.background,
		},
		widget = wibox.container.background,
	})
	return box
end

local function mk_footer(cancel_label, on_cancel, opts)
	local cancel = wibox.widget({
		{
			{
				text = cancel_label or "Cancel",
				align = "center",
				valign = "center",
				widget = wibox.widget.textbox,
			},
			left = 18,
			right = 18,
			top = 8,
			bottom = 8,
			widget = wibox.container.margin,
		},
		bg = opts.cancel_bg or XP.cancel_bg,
		fg = opts.cancel_fg or XP.cancel_fg,
		shape = gears.shape.rounded_rect,
		widget = wibox.container.background,
	})
	local normal = opts.cancel_bg or XP.cancel_bg
	local hover = opts.cancel_hover_bg or XP.cancel_hover_bg
	cancel:connect_signal("mouse::enter", function()
		cancel.bg = hover
	end)
	cancel:connect_signal("mouse::leave", function()
		cancel.bg = normal
	end)
	cancel:buttons(gears.table.join(awful.button({}, 1, function()
		if on_cancel then
			on_cancel()
		end
	end)))

	local row = wibox.widget({
		nil,
		nil,
		cancel,
		expand = "outside",
		layout = wibox.layout.align.horizontal,
	})

	return wibox.widget({
		{
			row,
			left = XP.pad_h,
			right = XP.pad_h,
			top = 8,
			bottom = 8,
			widget = wibox.container.margin,
		},
		bg = opts.footer_bg or XP.footer_bg,
		fg = opts.footer_fg or XP.footer_fg,
		widget = wibox.container.background,
	})
end

-- Eine große Icon-Kachel (Icon oben, Text unten), XP-like
local function mk_icon_tile(args)
	local icon_widget
	if args.icon and type(args.icon) == "string" and args.icon:match("^/") then
		icon_widget = wibox.widget({
			image = args.icon,
			resize = true,
			forced_width = 48,
			forced_height = 48,
			widget = wibox.widget.imagebox,
		})
	else
		-- Emoji / Text als Fallback
		icon_widget = wibox.widget({
			markup = string.format("<span font='%s'>%s</span>", args.emoji_font or "sans 28", args.emoji or "⏻"),
			align = "center",
			valign = "center",
			widget = wibox.widget.textbox,
		})
	end

	local label = wibox.widget({
		text = args.label or "",
		align = "center",
		valign = "center",
		widget = wibox.widget.textbox,
	})

	local inner = wibox.widget({
		nil,
		icon_widget,
		label,
		expand = "none",
		layout = wibox.layout.align.vertical,
	})

	local tile = wibox.widget({
		{
			inner,
			left = 8,
			right = 8,
			top = 10,
			bottom = 8,
			widget = wibox.container.margin,
		},
		forced_width = args.w or XP.tile_w,
		forced_height = args.h or XP.tile_h,
		bg = args.bg or XP.tile_bg,
		fg = args.fg or XP.tile_fg,
		shape = function(cr, w, h)
			gears.shape.rounded_rect(cr, w, h, 10)
		end,
		widget = wibox.container.background,
	})

	local normal = args.bg or XP.tile_bg
	local hover = args.hover or XP.tile_hover
	tile:connect_signal("mouse::enter", function()
		tile.bg = hover
	end)
	tile:connect_signal("mouse::leave", function()
		tile.bg = normal
	end)
	if args.on_press then
		tile:buttons(gears.table.join(awful.button({}, 1, function()
			args.on_press()
		end)))
	end

	return tile
end

-- ============================ GENERIC CHOICE DIALOG =========================
-- opts = {
--   title = "Turn off computer",
--   actions = { {icon="/path", emoji="🛌", label="Stand By", on_press=function() end}, ... },
--   theme   = table (überschreibt XP),
-- }
function Dialogs.choice(opts)
	local theme = {}
	for k, v in pairs(XP) do
		theme[k] = v
	end
	for k, v in pairs(opts.theme or {}) do
		theme[k] = v
	end

	local s = awful.screen.focused()
	local backdrop = mk_backdrop(s, theme.backdrop)

	local tiles_row = wibox.widget({
		spacing = 16,
		layout = wibox.layout.fixed.horizontal,
	})

	for _, a in ipairs(opts.actions or {}) do
		tiles_row:add(mk_icon_tile({
			icon = a.icon,
			emoji = a.emoji,
			emoji_font = a.emoji_font,
			label = a.label,
			on_press = function()
				if a.on_press then
					a.on_press()
				end
			end,
			w = theme.tile_w,
			h = theme.tile_h,
			bg = theme.tile_bg,
			fg = theme.tile_fg,
			hover = theme.tile_hover,
		}))
	end

	local body = wibox.widget({
		{
			tiles_row,
			left = theme.pad_h,
			right = theme.pad_h,
			top = theme.pad_v + 2,
			bottom = theme.pad_v,
			widget = wibox.container.margin,
		},
		bg = theme.body_bg,
		fg = theme.body_fg,
		widget = wibox.container.background,
	})

	local popup -- forward-declared für close()
	local function close()
		if popup and popup.visible then
			popup.visible = false
		end
		if backdrop and backdrop.visible then
			backdrop.visible = false
		end
	end

	local footer = mk_footer("Cancel", close, theme)
	-- Backdrop: Klick schließt
	backdrop:buttons(gears.table.join(awful.button({}, 1, function()
		close()
	end)))
	-- ESC schließt
	awful
		.keygrabber({
			mask_modkeys = true,
			stop_key = "Escape",
			stop_event = "release",
			keybindings = {
				{
					{},
					"Escape",
					function()
						close()
					end,
				},
			},
		})
		:start()

	-- Header
	local header = mk_header(opts.title or "Turn off computer", theme)

	-- Popup (Awesome 4.3: widget im Konstruktor)
	popup = awful.popup({
		screen = s,
		ontop = true,
		visible = false,
		type = "dialog",
		bg = "#00000000", -- transparenter Wrapper, wir bringen die 3 Segmente selbst mit BGs
		placement = awful.placement.centered,
		minimum_width = theme.dialog_min_w,
		maximum_width = theme.dialog_max_w,
		widget = {
			header,
			body,
			footer,
			layout = wibox.layout.fixed.vertical,
		},
	})

	popup.visible = true
	awful.placement.centered(popup, { honor_workarea = true })

	return { close = close, popup = popup, backdrop = backdrop }
end

-- ============================ CONCRETE DIALOGS ==============================
-- XP-Style Power: Standby/Suspend, Turn Off, Restart, plus Cancel
function Dialogs.power(opts)
	opts = opts or {}
	local function close_and(cmd)
		return function()
			-- schließe das Dialogfenster und führe danach den Befehl aus
			if opts._close then
				opts._close()
			end
			if cmd and #cmd > 0 then
				awful.spawn.with_shell(cmd)
			end
		end
	end

	-- wir bauen zuerst die Actions, dann rufen wir choice() und fügen die close-Funktion ein
	local actions = {
		{ emoji = "🛌", label = "Stand By", on_press = close_and("systemctl suspend") },
		{ emoji = "⏻", label = "Turn Off", on_press = close_and("systemctl poweroff") },
		{ emoji = "🔄", label = "Restart", on_press = close_and("systemctl reboot") },
		-- Wenn du echte PNGs nutzen willst, gib 'icon = "/pfad/zum/icon.png"' anstelle von emoji
	}

	-- baue den Dialog
	local dlg = Dialogs.choice({
		title = "Turn off computer",
		actions = actions,
		theme = opts, -- erlaubt Farbüberschreibung vom Footer aus
	})
	-- injiziere close, damit Aktionen erst schließen, dann ausführen
	opts._close = dlg.close
	return dlg
end

-- Soft-Logout (nur Awesome beenden) -----------------------------------------
function Dialogs.logout_confirm(opts)
	opts = opts or {}
	local actions = {
		{
			emoji = "🚪",
			label = "Exit WM",
			on_press = function()
				if opts._close then
					opts._close()
				end
				awesome.quit()
			end,
		},
	}
	local dlg = Dialogs.choice({
		title = "Log off",
		actions = actions,
		theme = opts,
	})
	opts._close = dlg.close
	return dlg
end

return Dialogs
