local awful = require("awful")
local beautiful = require("beautiful")
local MRU = require("features.shell.menu.lib.mru")
local AllPrograms = require("features.shell.menu.lib.all_programs")

local M = {}

M.user = {
	name = os.getenv("USER") or "user",
	avatar = beautiful.awesome_icon,
}

-- Pins (oben in der linken Spalte)
MRU:set_pinned({
	{ text = "Internet", icon = beautiful.awesome_icon, cmd = "firefox" },
	{ text = "E-mail", icon = beautiful.awesome_icon, cmd = "thunderbird" },
})

-- Steuerung
M.left_mode = "mixed" -- "pinned" | "recent" | "mixed"
MRU:set_max(6)

-- Dropdown „Alle Programme“
local all_menu = AllPrograms:new_menu()
M.all_programs_button = {
	text = "All Programs",
	icon = beautiful.awesome_icon,
	on_press = function(_, anchor_widget)
		AllPrograms:toggle_at_widget(all_menu, anchor_widget or mouse.current_widget_geometry)
	end,
}

-- Linke Spalte zur Laufzeit erzeugen
function M.left_items()
	local items = {}
	-- Stelle (oben oder unten) abhängig von deinem Layout
	table.insert(items, M.all_programs_button)

	for _, it in ipairs(MRU:get(M.left_mode)) do
		table.insert(items, {
			text = it.text,
			icon = it.icon,
			on_press = function()
				MRU:spawn(it)
			end,
		})
	end
	return items
end

-- Rechte Spalte bleibt „Orte/System“
function M.right_items()
	return {
		{
			text = "My Documents",
			icon = beautiful.awesome_icon,
			on_press = function()
				awful.spawn.with_shell('xdg-open "$HOME"')
			end,
		},
		{
			text = "Control Panel",
			icon = beautiful.awesome_icon,
			on_press = function()
				awful.spawn("gnome-control-center")
			end,
		},
		{
			text = "Help and Support",
			icon = beautiful.awesome_icon,
			on_press = function()
				awful.spawn("yelp")
			end,
		},
	}
end

function M.power_items()
	return {
		{
			text = "Log Off",
			icon = beautiful.awesome_icon,
			on_press = function()
				awesome.quit()
			end,
		},
		{
			text = "Turn Off",
			icon = beautiful.awesome_icon,
			on_press = function()
				awful.spawn.with_shell("systemctl poweroff")
			end,
		},
	}
end

-- Falls du extern (z.B. aus der Suche) starten willst und MRU füttern:
function M.record_launch(entry)
	-- entry = {text, cmd, icon}
	MRU:add(entry)
end

return M
