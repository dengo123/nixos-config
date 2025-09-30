-- ~/.config/awesome/ui/theme/init.lua
local M = {}

-- Liste der Teil-Themes, die (optional) existieren können
-- -> einfach neue Namen ergänzen, Datei unter ui/theme/<name>.lua anlegen
local PARTS = {
	"tabs",
	"titlebar",
	"wibar",
}

-- Teile laden (optional, ohne Absturz wenn eine Datei fehlt)
for _, name in ipairs(PARTS) do
	local ok, mod = pcall(require, "ui.theme." .. name)
	if ok and type(mod) == "table" then
		M[name] = mod
	end
end

-- Optionaler Init-Hook: ruft .init(cfg) aller registrierten Teile auf
function M.init(cfg)
	for _, mod in pairs(M) do
		if type(mod) == "table" and type(mod.init) == "function" then
			-- defensiv aufrufen, damit ein fehlerhaftes Teil-Theme nicht alles stoppt
			local ok, err = pcall(mod.init, cfg)
			if not ok then
				-- Wenn du Debug willst, ent-kommentieren:
				-- naughty.notify({ title = "Theme init error", text = tostring(err) })
			end
		end
	end
end

return M
