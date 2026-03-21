-- ~/.config/awesome/input/global/apps.lua
local awful = require("awful")

local M = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function spawn_cmd(cmd)
	if type(cmd) == "table" then
		awful.spawn(cmd)
		return
	end

	if type(cmd) == "string" and cmd ~= "" then
		if cmd:find("||") or cmd:find("~") then
			awful.spawn.with_shell(cmd)
		else
			awful.spawn(cmd)
		end
	end
end

local function primary_cmd(apps, input_cfg)
	local return_app = input_cfg.return_app or "terminal"

	if return_app == "editor" then
		return apps.editor
	end

	return apps.terminal
end

local function secondary_cmd(apps, input_cfg)
	local return_app = input_cfg.return_app or "terminal"

	if return_app == "editor" then
		return apps.terminal
	end

	return apps.editor
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.build(modkey, cfg)
	cfg = cfg or {}

	local apps = cfg.apps or {}
	local input_cfg = cfg.input or {}

	return awful.util.table.join(
		awful.key({ modkey }, "Return", function()
			spawn_cmd(primary_cmd(apps, input_cfg))
		end, {
			description = "Open Primary App",
			group = "Apps",
		}),

		awful.key({ modkey, "Shift" }, "Return", function()
			spawn_cmd(secondary_cmd(apps, input_cfg))
		end, {
			description = "Open Secondary App",
			group = "Apps",
		}),

		awful.key({ modkey }, "b", function()
			spawn_cmd(apps.browser)
		end, {
			description = "Open Browser",
			group = "Apps",
		}),

		awful.key({ modkey }, "e", function()
			spawn_cmd(apps.files)
		end, {
			description = "Open File Manager",
			group = "Apps",
		})
	)
end

return function(modkey, cfg)
	return M.build(modkey, cfg)
end
