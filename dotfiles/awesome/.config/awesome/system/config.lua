-- ~/.config/awesome/system/config.lua
return {
	apps = {
		terminal = "xterm",
		editor = "emacsclient -c -a ''",
		browser = "firefox",
		files = "nemo",
		calendar = "gnome-calendar",

		launcher = nil,
	},

	input = {
		modkey = "Mod4",
	},

	bar = {
		position = "bottom",
		start_on_primary_only = false,

		reveal_on_fullscreen_edge = true,
		reveal_trigger_px = 2,
		reveal_hide_delay = 0.20,

		clock = {
			show_seconds = false,
			calendar_enable = true,
			calendar_use_menu_theme = false,
		},
		show_notify = "primary",
	},

	tags = {
		selection = "sync", -- "single" | "sync"
		mode = "fixed", -- "fixed" | "dynamic"
		fixed_count = 2,
	},

	focus = {
		sloppy = true,
		center_mouse = false,
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

	notify = {
		timeout = 5,
		speech = false,
		screen = "focused",

		history = {
			max_entries = 100,
		},

		center = {
			close_on_escape = true,
			close_on_click_outside = true,
			close_on_tag_switch = true,
			close_on_client_focus = false,
		},

		actions = {
			show = true,
			invoke_closes_center = true,
		},

		filter = {
			ignore_resident = false,
			ignore_silent = false,
			apps = {},
		},
	},
}
