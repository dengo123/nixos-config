-- ~/.config/awesome/features/workspaces/tags/client_policy.lua

local M = {}

-- Einstellungen (falls du’s dynamisch machen willst, exportier Setters)
local KILL_ON_DELETE = true -- kill clients, wenn Tag gelöscht wird
local KILL_MODE = "all" -- "all" | "exclusive"

-- optional öffentlich machen:
function M.set_kill_on_delete(val)
	KILL_ON_DELETE = not not val
end

function M.set_kill_mode(mode)
	if mode == "all" or mode == "exclusive" then
		KILL_MODE = mode
	end
end

function M.kill_clients_in_tag(t)
	if not (KILL_ON_DELETE and t) then
		return
	end
	local clients = t:clients()
	for _, c in ipairs(clients) do
		if KILL_MODE == "all" then
			c:kill()
		else
			-- nur exklusive Clients killen; Multi-Tag-Clients behalten,
			-- verlieren aber diesen Tag
			local ctags = c:tags() or {}
			if #ctags <= 1 then
				c:kill()
			else
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

return M
