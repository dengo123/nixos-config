-- ~/.config/awesome/system/config.lua
return {
	terminal = os.getenv("TERMINAL") or "kitty",
	editor = os.getenv("EDITOR") or "nano",
	modkey = "Mod4",
	theme = "default/theme.lua",

	launcher = nil,
	browser = os.getenv("BROWSER") or "firefox",
	files = "nemo || xdg-open ~",

	emacs = {
		client = { "emacsclient", "-c", "-a", "" }, -- -a "" startet Emacs, falls kein Server läuft
	},

	workspaces = "core",
}
