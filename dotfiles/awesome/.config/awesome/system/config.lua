-- ~/.config/awesome/system/config.lua
return {
	-- Basis
	terminal = "kitty",
	editor = os.getenv("EDITOR") or "nano",
	modkey = "Mod4",
	theme = "default/theme.lua", -- oder absoluter Pfad zu deinem Theme

	-- Ausgelagerte App-Commands
	launcher = nil, -- z.B. rofi, fuzzel, bemenu ...
	browser = "firefox", -- oder "librewolf", "chromium", ...
	files = "nemo || xdg-open ~", -- Dateimanager Fallback

	workspaces = "core", -- "core || sync"
	tabs = "single", -- "single || group"
}
