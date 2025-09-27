-- ~/.config/awesome/features/shell/menu/lib/term.lua
local awful = require("awful")

local M = {}

local function read_cfg()
	local ok, cfg = pcall(require, "system.config")
	if ok and type(cfg) == "table" then
		return cfg
	end
	return { terminal = "xterm" }
end

-- starte ein Terminal und führe *eine* Shell-Commandline darin aus
function M.run(cmdline)
	local term = (read_cfg().terminal or "xterm"):gsub("%s+$", "")
	local sh = string.format("bash -lc %q", cmdline)

	if term:find("wezterm") then
		awful.spawn(string.format("%s start -- %s", term, sh))
	elseif term:find("alacritty") then
		awful.spawn(string.format("%s -e %s", term, sh))
	elseif term:find("kitty") then
		awful.spawn(string.format("%s -- %s", term, sh))
	elseif term:find("ghostty") then
		-- Ghostty: gängig ist `--` um ein Kommando zu übergeben
		awful.spawn(string.format("%s -- %s", term, sh))
	elseif term:find("gnome%-terminal") then
		awful.spawn(string.format("%s -- %s", term, sh))
	else
		-- generischer Fallback
		awful.spawn(string.format("%s -e %s", term, sh))
	end
end

return M
