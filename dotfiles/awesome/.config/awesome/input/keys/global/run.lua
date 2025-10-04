-- ~/.config/awesome/input/keys/global/run.lua
local awful = require("awful")
local gears = require("gears")

-- Wird vom Run-Launcher beim Öffnen gesetzt (siehe run/init.lua: _G.__run_api)
local function get_run_api()
	return rawget(_G, "__run_api")
end

return function(modkey, launchers) -- launchers wird von shell.launchers injiziert
	local function open_in_mode(mode)
		if launchers and launchers.open and launchers.open.run then
			launchers.open.run({
				screen = mouse and mouse.screen or nil, -- auf aktuellem Screen öffnen
				mode = mode or "run",
			})
		end
	end

	local function ensure_and_switch(target_mode)
		-- Alias: "files" -> "local"
		if target_mode == "files" then
			target_mode = "local"
		end

		local api = get_run_api()
		if api and api.is_active and api.is_active() then
			-- bereits offen → Modus umschalten
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
		-- noch nicht offen → direkt im gewünschten Modus öffnen
		open_in_mode(target_mode)
	end

	local function close_if_open()
		local api = get_run_api()
		if api and api.is_active and api.is_active() and api.cancel then
			api.cancel()
		end
	end

	return gears.table.join(
		-- Mod4 + Space → Run (Apps) öffnen oder – falls offen – auf Run-Modus schalten
		awful.key({ modkey }, "space", function()
			ensure_and_switch("run")
		end, { description = "Run-Launcher: Apps (öffnen/umschalten)", group = "apps" }),

		-- Mod4 + / → Files (Local) öffnen/umschalten
		awful.key({ modkey }, "/", function()
			ensure_and_switch("files") -- alias → local
		end, { description = "Run-Launcher: Files (öffnen/umschalten)", group = "apps" }),

		-- Mod4 + Shift + / → Web öffnen/umschalten
		awful.key({ modkey, "Shift" }, "/", function()
			ensure_and_switch("web")
		end, { description = "Run-Launcher: Web (öffnen/umschalten)", group = "apps" }),

		-- Escape → nur schließen, wenn offen
		awful.key({}, "Escape", function()
			close_if_open()
		end, { description = "Run-Launcher: schließen", group = "apps" })
	)
end
