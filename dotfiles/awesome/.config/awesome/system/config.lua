-- ~/.config/awesome/system/config.lua
return {
	system = {
		terminal = os.getenv("TERMINAL"),
		editor = os.getenv("EDITOR") or "nano",
		modkey = "Mod4",

		launcher = nil,
		browser = os.getenv("BROWSER") or "firefox",
		files = "nemo",
	},

	bar = {
		position = "bottom",
		start_on_primary_only = false,

		reveal_on_fullscreen_edge = true,
		reveal_trigger_px = 2,
		reveal_hide_delay = 0.20,

		clock = {
			show_seconds = false,
			app = "gnome-calendar",
			calendar_enable = true,
			calendar_use_menu_theme = false,
		},
	},

	tags = {
		selection = "sync", -- "single" | "sync"
		mode = "fixed", -- "fixed" | "dynamic"
		fixed_count = 2,
	},

	focus = {
		sloppy = true,
		center_mouse = true,
		raise_on_mouse = false,
		block_ms = 150,
	},

	windowing = {
		titlebars = true,
		floating = {
			files = true,
			terminals = true,
		},
		fullscreen = {
			dim = {
				enabled = true,
				never_dim_primary = false,
			},
		},
	},
}
