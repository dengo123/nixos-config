-- ~/.config/awesome/shell/launchers/lib/actions.lua
-- Zentrale Aktions-Registry. Labels -> Funktionen.
-- bind(ctx) liefert eine Lookup-Tabelle mit gebundenen Aktionen:
--   ctx.ctrl  : der Controller (cancel(), submit(), ...)
--   ctx.handle: Popup-Handle (close())
--   ctx.gears : gears (für delayed_call)
--   ctx.awful : (optional) falls gebraucht

local A = {}

local function safe_close(ctrl, handle, gears)
	if ctrl and ctrl.cancel then
		ctrl.cancel() -- stoppt prompt/keygrabber zuverlässig
	end
	if handle and handle.close then
		(gears and gears.timer or require("gears").timer).delayed_call(function()
			handle.close()
		end)
	end
end

function A.bind(ctx)
	ctx = ctx or {}
	local ctrl, handle, gears = ctx.ctrl, ctx.handle, ctx.gears

	local actions = {}

	-- Standard-Aktionen für Run/Launcher
	actions["Cancel"] = function()
		safe_close(ctrl, handle, gears)
	end

	actions["OK"] = function()
		if ctrl and ctrl.submit then
			ctrl.submit() -- wie Enter
		else
			safe_close(ctrl, handle, gears)
		end
	end

	-- Du kannst hier weitere Standardlabels definieren, z.B.:
	-- actions["Close"] = actions["Cancel"]
	-- actions["Apply"] = function() if ctrl and ctrl.apply then ctrl.apply() end end

	-- Power-spezifische Labels können optional hier auch registriert werden,
	-- wenn du Buttons in Power nutzt (anstatt Icon-Row):
	-- actions["Shutdown"] = function() awful.spawn.with_shell("systemctl poweroff") end
	-- actions["Restart"]  = function() awful.spawn.with_shell("systemctl reboot")  end

	return actions
end

return A
