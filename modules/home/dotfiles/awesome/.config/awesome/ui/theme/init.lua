-- ui/theme/init.lua
-- Zentrale API für Theme-Settings. Heute minimal, später kann hier
-- ein echtes Theme-Modul geladen werden.

local M = {}

--- Theme.use(section, overrides)
--- @param section string  -- Name der Komponente (z.B. "footer")
--- @param overrides table -- optionale Werte (z.B. aus opts.ui)
--- @return table          -- gemergte Theme-Tabelle
function M.use(section, overrides)
	overrides = overrides or {}

	-- Hier später: aus Tokens/Theme-Dateien laden
	-- Heute: nur Overrides direkt zurückgeben
	return overrides
end

return M
