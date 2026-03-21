-- ~/.config/awesome/input/global/power.lua
local awful = require("awful")
local gears = require("gears")

local M = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function bind(mods, key, fn, desc)
	return awful.key(mods, key, fn, {
		description = desc,
		group = "Launchers",
	})
end

local function spawn_shell(cmd)
	awful.spawn.with_shell(cmd)
end

local function can_open_power(launchers_api)
	return launchers_api and launchers_api.open and type(launchers_api.open.power) == "function"
end

local function install_modal_keys(modkey, handle)
	local previous_root_keys = root.keys()
	local modal_keys = {}

	local modal_mod_sets = {
		{},
		{ modkey },
	}

	local function close_handle()
		if handle and type(handle.close) == "function" then
			handle.close()
		end
	end

	local function add_modal_key(key, fn, desc)
		for _, mods in ipairs(modal_mod_sets) do
			table.insert(
				modal_keys,
				awful.key(mods, key, fn, {
					description = desc or key,
					group = "Launchers",
				})
			)
		end
	end

	add_modal_key("u", function()
		close_handle()
		spawn_shell("systemctl suspend")
	end, "Stand By")

	add_modal_key("h", function()
		close_handle()
		spawn_shell("systemctl hibernate")
	end, "Sleep")

	add_modal_key("r", function()
		close_handle()
		spawn_shell("systemctl reboot")
	end, "Reboot")

	add_modal_key("p", function()
		close_handle()
		spawn_shell("systemctl poweroff")
	end, "Power Off")

	add_modal_key("Escape", function()
		close_handle()
	end, "Cancel")

	local joined_keys
	if previous_root_keys then
		joined_keys = gears.table.join(previous_root_keys, table.unpack(modal_keys))
	else
		joined_keys = gears.table.join(table.unpack(modal_keys))
	end

	root.keys(joined_keys)

	local original_close = handle.close
	handle.close = function(...)
		if previous_root_keys then
			root.keys(previous_root_keys)
		else
			root.keys(nil)
		end

		if type(original_close) == "function" then
			return original_close(...)
		end
	end
end

local function open_power_dialog(modkey, launchers_api)
	if not can_open_power(launchers_api) then
		return
	end

	local handle = launchers_api.open.power({})

	if handle then
		install_modal_keys(modkey, handle)
	end
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.build(modkey, launchers_api)
	return gears.table.join(bind({ modkey }, "End", function()
		open_power_dialog(modkey, launchers_api)
	end, "Open Power Dialog"))
end

return function(modkey, launchers_api)
	return M.build(modkey, launchers_api)
end
