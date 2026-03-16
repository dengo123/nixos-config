-- ~/.config/awesome/system/config.lua
return {
	system = {
		terminal = os.getenv("TERMINAL") or "kitty",
		editor = os.getenv("EDITOR") or "nano",
		modkey = "Mod4",

		launcher = nil,
		browser = os.getenv("BROWSER") or "firefox",
		files = (os.getenv("FILE_MANAGER") or "xdg-open") .. " ~",
	},

	tags = {
		mode = "fixed", -- "fixed" | "dynamic"
		selection = "sync", -- "single" | "sync"
		fixed_count = 2,
	},

	focus = {
		sloppy = true,
		center_mouse = true,
		raise_on_mouse = false,
		block_ms = 150,
	},

	windowing = {

		titlebars = {
			enabled = true,
		},

		portrait = {
			fullscreen_tiled = true,
		},
	},
}
