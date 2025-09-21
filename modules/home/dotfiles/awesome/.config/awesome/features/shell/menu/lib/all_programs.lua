local awful = require("awful")
local menu_gen = require("menubar.menu_gen")

local M = {}

local function build_items(cb)
	menu_gen.generate(function(entries)
		local items = {}
		table.sort(entries, function(a, b)
			return (a.name or ""):lower() < (b.name or ""):lower()
		end)
		for _, e in ipairs(entries) do
			table.insert(items, { e.name, e.cmdline, e.icon })
		end
		cb(items)
	end)
end

function M:new_menu(args)
	args = args or {}
	local menu = awful.menu({
		items = { { "Loading…", function() end } },
		theme = { width = args.width or 360 },
	})
	function menu:_rebuild()
		build_items(function(items)
			if #items == 0 then
				items = { { "No applications found", function() end } }
			end
			self.items = items
			self:refresh()
		end)
	end

	menu:_rebuild()
	return menu
end

function M:toggle_at_widget(menu, widget)
	local g = widget and widget:geometry()
	local x, y = mouse.coords().x, mouse.coords().y
	if g and menu.wibox then
		x = g.x
		y = g.y - menu.wibox.height
	end
	menu:toggle({ coords = { x = x, y = y } })
end

return M
