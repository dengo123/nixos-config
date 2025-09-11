-- Zentrale Defaults/Konfiguration
return {
	-- Basis
	terminal = "ghostty",
	editor = os.getenv("EDITOR") or "nano",
	modkey = "Mod4",
	theme = "default/theme.lua", -- oder absoluter Pfad zu deinem Theme

	-- Ausgelagerte App-Commands
	launcher = "launcher", -- z.B. rofi, fuzzel, bemenu ...
	browser = "firefox", -- oder "librewolf", "chromium", ...
	files = "thunar || xdg-open ~", -- Dateimanager Fallback
}
