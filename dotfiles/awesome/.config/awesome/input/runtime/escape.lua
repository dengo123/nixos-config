-- ~/.config/awesome/input/keys/runtime/escape.lua
local awful = require("awful")
local gears = require("gears")

local M = {}

local overlay_escape_key = nil
local base_globalkeys = nil
local active = false

-- =========================================================================
-- Internal
-- =========================================================================

local function build_escape_key(overlays)
	return awful.key({}, "Escape", function()
		local ordered = (overlays and overlays.ordered) or {}

		for _, overlay in ipairs(ordered) do
			local is_open = overlay.is_open
			local close = overlay.close

			if type(is_open) == "function" and type(close) == "function" and is_open() then
				close()
				return
			end
		end
	end, {
		description = "close active overlay",
		group = "ui",
	})
end

local function any_overlay_open(overlays)
	local ordered = (overlays and overlays.ordered) or {}

	for _, overlay in ipairs(ordered) do
		local is_open = overlay.is_open

		if type(is_open) == "function" and is_open() then
			return true
		end
	end

	return false
end

local function install(root_keys)
	if active then
		return
	end

	active = true
	root.keys(gears.table.join(root_keys, overlay_escape_key))
end

local function uninstall(root_keys)
	if not active then
		return
	end

	active = false
	root.keys(root_keys)
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	base_globalkeys = args.globalkeys
	overlay_escape_key = build_escape_key(args.overlays)

	awesome.connect_signal("ui::overlays_changed", function()
		if any_overlay_open(args.overlays) then
			install(base_globalkeys)
		else
			uninstall(base_globalkeys)
		end
	end)

	if any_overlay_open(args.overlays) then
		install(base_globalkeys)
	else
		uninstall(base_globalkeys)
	end
end

return M
