-- ~/.config/awesome/shell/launchers/lib/popup.lua
-- Reiner Popup-Lifecycle (ohne Border/Shape/Radius-Zeichnung!)
-- Borders, Shapes, Radius etc. werden ausschließlich in den Containern der
-- einzelnen Launcher (run/container.lua, power/container.lua) gezeichnet.
--
-- API:
--   local Popup = require("shell.launchers.lib.popup")
--   local h = Popup.show(form_widget, theme, {
--     screen=..., width=..., height=...,
--     placement=awful.placement.centered,
--     use_backdrop=true, close_on_backdrop=false,
--     show_root=false|"with_bars"|"full",
--     group="launchers",
--   })
--   h.close()

local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")

local Popup, _open = {}, {}

-- ================== Utils ==================
local function nz(x, fb)
	return x == nil and fb or x
end

local function set_clients_visible(scr, on)
	local ok, clients = pcall(function()
		return client.get(scr)
	end)
	if ok and clients then
		for _, c in ipairs(clients) do
			c.hidden = not on
		end
	end
end

local function set_bars_visible(scr, on)
	for _, bar in pairs(scr.bars or {}) do
		if bar and bar.visible ~= nil then
			bar.visible = on
		end
	end
end

function Popup.close_all()
	for i = #_open, 1, -1 do
		local h = _open[i]
		if h and h.close then
			h.close(true)
		end
		_open[i] = nil
	end
end

-- ================== Main ==================
-- opts:
--   screen, width (req), height (req)
--   placement? (default awful.placement.centered)
--   use_backdrop? (default true)
--   close_on_backdrop? (default false)
--   show_root? = false|"with_bars"|"full" (default "with_bars")
--   group? (nur Tagging/Debug)
function Popup.show(form_widget, th, opts)
	opts, th = (opts or {}), (th or {})

	local s = opts.screen or awful.screen.focused()
	local W = assert(opts.width, "popup.show: width required")
	local H = assert(opts.height, "popup.show: height required")

	local placement_fn = opts.placement or awful.placement.centered
	local use_backdrop = nz(opts.use_backdrop, true)
	local close_on_backdrop = nz(opts.close_on_backdrop, false)

	local show_root_mode = opts.show_root
	if show_root_mode == nil then
		show_root_mode = "with_bars"
	end
	if show_root_mode == true then
		show_root_mode = "full"
	end

	local hide_clients = (show_root_mode == "with_bars") or (show_root_mode == "full")
	local hide_bars = (show_root_mode == "full")

	-- ---------- Backdrop (optional) ----------
	local backdrop = nil
	if use_backdrop then
		backdrop = wibox({
			screen = s,
			visible = true,
			ontop = true,
			type = "splash",
			bg = nz(th.backdrop, "#00000066"), -- nur Backdrop-Farbe
		})
		backdrop.input_passthrough = false
		if show_root_mode == "with_bars" then
			local wa = s.workarea
			backdrop:geometry({ x = wa.x, y = wa.y, width = wa.width, height = wa.height })
		else
			backdrop:geometry(s.geometry)
		end
	end

	-- ---------- Popup (reiner Träger; kein Border/Shape hier!) ----------
	-- form_widget enthält bereits alles (inkl. Border/Radius, falls gewünscht).
	local popup_widget = wibox.widget({
		{ form_widget, strategy = "exact", width = W, height = H, widget = wibox.container.constraint },
		widget = wibox.container.margin,
	})

	local popup = awful.popup({
		screen = s,
		ontop = true,
		visible = false,
		type = "dialog",
		bg = "#00000000", -- komplett transparent; kein eigener Rand/Shape
		placement = placement_fn,
		widget = popup_widget,
	})

	-- ---------- Lifecycle ----------
	local function remove_from_open()
		for i = #_open, 1, -1 do
			if _open[i] and _open[i].popup == popup then
				table.remove(_open, i)
				break
			end
		end
	end

	local function really_close()
		if popup then
			popup.visible = false
		end
		if backdrop then
			backdrop.visible = false
		end

		if hide_clients then
			set_clients_visible(s, true)
		end
		if hide_bars then
			set_bars_visible(s, true)
		end

		if popup then
			popup.widget = nil
		end
		remove_from_open()
	end

	local function close()
		really_close()
	end

	if hide_clients then
		set_clients_visible(s, false)
	end
	if hide_bars then
		set_bars_visible(s, false)
	end

	popup.visible = true
	placement_fn(popup, { honor_workarea = true })

	-- Backdrop-Interaktion
	if backdrop then
		if close_on_backdrop then
			backdrop:buttons(gears.table.join(awful.button({}, 1, function()
				close()
			end)))
		else
			backdrop:buttons(
				gears.table.join(
					awful.button({}, 1, function() end),
					awful.button({}, 2, function() end),
					awful.button({}, 3, function() end)
				)
			)
		end
	end

	-- Auto-Close bei Tag/Screen-Änderungen
	local function on_tag_selected()
		close()
	end
	local function on_screen_removed()
		close()
	end
	local function on_screen_added()
		close()
	end
	local function on_primary_changed()
		close()
	end

	tag.connect_signal("property::selected", on_tag_selected)
	screen.connect_signal("removed", on_screen_removed)
	screen.connect_signal("added", on_screen_added)
	screen.connect_signal("primary_changed", on_primary_changed)

	local orig_close = close
	close = function(...)
		pcall(function()
			tag.disconnect_signal("property::selected", on_tag_selected)
		end)
		pcall(function()
			screen.disconnect_signal("removed", on_screen_removed)
		end)
		pcall(function()
			screen.disconnect_signal("added", on_screen_added)
		end)
		pcall(function()
			screen.disconnect_signal("primary_changed", on_primary_changed)
		end)
		return orig_close(...)
	end

	local handle = { close = close, popup = popup, backdrop = backdrop, screen = s, group = opts.group or "launchers" }
	table.insert(_open, handle)
	return handle
end

return Popup
