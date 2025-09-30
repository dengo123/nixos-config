-- ~/.config/awesome/features/workspaces/tags/client_policy.lua

local M = {}

-- Standardverhalten für "soft" (Mod4 + c):
-- - true  = Clients beim Tag-Löschen behandeln
-- - "exclusive" = nur Clients killen, die exklusiv auf diesem Tag sind;
--                 Multi-Tag-Clients behalten andere Tags und verlieren nur diesen Tag.
-- - "all" = alle Clients auf diesem Tag killen (hart)
local KILL_ON_DELETE = true
local SOFT_MODE = "exclusive" -- "exclusive" | "all"

-- Optional konfigurierbar:
function M.set_kill_on_delete(val)
	KILL_ON_DELETE = not not val
end

function M.set_soft_mode(mode)
	if mode == "exclusive" or mode == "all" then
		SOFT_MODE = mode
	end
end

--- Kill-/Enttag-Policy für Clients auf einem Tag.
--  @param t     tag
--  @param force boolean|nil  -> wenn true, IMMER alle Clients killen (für Force-Delete)
function M.kill_clients_in_tag(t, force)
	if not (t and KILL_ON_DELETE) then
		return
	end

	local clients = t:clients()
	for _, c in ipairs(clients) do
		if force then
			-- Force: immer hart killen, egal wie viele Tags der Client hat
			c:kill()
		else
			if SOFT_MODE == "all" then
				c:kill()
			else
				-- SOFT_MODE == "exclusive": nur exklusive Clients killen
				local ctags = c:tags() or {}
				if #ctags <= 1 then
					c:kill()
				else
					-- Multi-Tag: nur diesen Tag entfernen
					local keep = {}
					for _, tagx in ipairs(ctags) do
						if tagx ~= t then
							table.insert(keep, tagx)
						end
					end
					c:tags(keep)
				end
			end
		end
	end
end

return M
