-- ~/.config/awesome/input/keys/global/run.lua
local awful = require("awful")
local gears = require("gears")

-- Wird vom Run-Launcher beim Öffnen gesetzt (siehe run/init.lua)
local function get_run_api()
	return rawget(_G, "__run_api")
end

return function(modkey, launchers) -- launchers injiziert (wie bei power)
	local function open_or_toggle(mode)
		local api = get_run_api()
		if api and api.is_active and api.is_active() then
			if api.cancel then
				api.cancel()
			end
			return
		end
		if launchers and launchers.open and launchers.open.run then
			launchers.open.run({
				screen = mouse and mouse.screen or nil, -- auf aktuellem Screen öffnen
				mode = mode or "run", -- "run" (Apps) ist Default
			})
		end
	end

	local function ensure_and_switch(target_mode)
		local api = get_run_api()
		if api and api.is_active and api.is_active() then
			if target_mode == "files" and api.focus_local then
				api.focus_local()
			end
			if target_mode == "web" and api.focus_web then
				api.focus_web()
			end
			if target_mode == "run" and api.focus_run then
				api.focus_run()
			end
			return
		end
		-- noch nicht offen → direkt im gewünschten Modus öffnen
		open_or_toggle(target_mode)
	end

	local function close_if_open()
		local api = get_run_api()
		if api and api.is_active and api.is_active() and api.cancel then
			api.cancel()
		end
	end

	return gears.table.join(
		-- Mod4 + Space → Run-Launcher öffnen/toggeln (Apps)
		awful.key({ modkey }, "space", function()
			open_or_toggle("run")
		end, { description = "Run-Launcher öffnen/toggeln (Apps)", group = "apps" }),

		-- Ctrl + / → Files-Modus (wenn offen: umschalten, sonst in Files öffnen)
		awful.key({ "Control" }, "/", function()
			ensure_and_switch("files")
		end, { description = "Run-Launcher: Files-Modus", group = "apps" }),

		-- Ctrl + Shift + / → Web-Modus (wenn offen: umschalten, sonst in Web öffnen)
		awful.key({ "Control", "Shift" }, "/", function()
			ensure_and_switch("web")
		end, { description = "Run-Launcher: Web-Modus", group = "apps" }),

		-- Escape → schließen (nur wenn offen)
		awful.key({}, "Escape", function()
			close_if_open()
		end, { description = "Run-Launcher schließen", group = "apps" })
	)
end
