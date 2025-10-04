-- ~/.config/awesome/input/keys/global/run.lua
local awful = require("awful")
local gears = require("gears")

local function get_run_api()
	return rawget(_G, "__run_api")
end

return function(modkey, launchers)
	local function open_in_mode(mode)
		if launchers and launchers.open and launchers.open.run then
			launchers.open.run({ screen = mouse and mouse.screen or nil, mode = mode or "run" })
		end
	end
	local function ensure_and_switch(target_mode)
		if target_mode == "files" then
			target_mode = "local"
		end
		local api = get_run_api()
		if api and api.is_active and api.is_active() then
			if target_mode == "local" and api.focus_local then
				api.focus_local()
				return
			end
			if target_mode == "web" and api.focus_web then
				api.focus_web()
				return
			end
			if target_mode == "run" and api.focus_run then
				api.focus_run()
				return
			end
			return
		end
		open_in_mode(target_mode)
	end
	local function close_if_open()
		local api = get_run_api()
		if api and api.is_active and api.is_active() and api.cancel then
			api.cancel()
		end
	end

	return gears.table.join(
		awful.key({ modkey }, "space", function()
			ensure_and_switch("run")
		end, { description = "Run-Launcher: Apps (öffnen/umschalten)", group = "apps" }),
		awful.key({ modkey }, "/", function()
			ensure_and_switch("local")
		end, { description = "Run-Launcher: Files (öffnen/umschalten)", group = "apps" }),
		awful.key({ modkey, "Shift" }, "/", function()
			ensure_and_switch("web")
		end, { description = "Run-Launcher: Web (öffnen/umschalten)", group = "apps" })
	)
end
