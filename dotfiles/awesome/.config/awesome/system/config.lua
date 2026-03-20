-- ~/.config/awesome/system/config.lua
return {
	-- =========================================================================
	-- Apps
	-- =========================================================================

	apps = {
		terminal = "xterm",
		editor = "emacsclient -c -a ''",
		browser = "firefox",
		files = "nemo",
		calendar = "gnome-calendar",
	},

	-- =========================================================================
	-- Input
	-- =========================================================================

	input = {
		modkey = "Mod4",
		return_app = "editor",

		media = {
			volume_step = 3,
			osd_timeout = 1.2,
		},
		screenshot = {
			save_dir = (os.getenv("HOME") or "") .. "/Pictures/screenshots",
		},
	},

	-- =========================================================================
	-- System
	-- =========================================================================

	system = {
		autostart = {
			copyq = {
				enable = true,
				delay = 1.5,
			},
		},
	},

	-- =========================================================================
	-- Bar
	-- =========================================================================

	bar = {
		position = "bottom",
		start_on_primary_only = false,

		start_action = "menu",

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

	-- =========================================================================
	-- Tags
	-- =========================================================================

	tags = {
		selection = "sync",
		mode = "fixed",
		fixed_count = 2,

		layouts = {
			mode = "tiling",
			default = "max",
			include = {
				"max",
				"fair",
				"tile_main",
				"tile_cross",
			},

			use_max_fullscreen_for_portrait = false,
		},

		gap = 4,
		max_padding = true,

		delete = {
			kill_clients = true,
			soft_mode = "exclusive",
		},
	},

	-- =========================================================================
	-- Windowing
	-- =========================================================================

	windowing = {
		titlebars = {
			mode = "on",
			exclude = { "terminal" },
		},

		rounded_corners = true,

		focus = {
			raise_on_mouse = false,
			block_ms = 150,

			center_mouse = {
				enable = true,
				exclude_layouts = { "max" },
				exclude_states = { "fullscreen" },
			},
		},

		floating = {
			files = true,
			terminals = true,
		},

		fullscreen = {
			dim = {
				enabled = true,
				never_dim_primary = true,
			},
		},
	},

	-- =========================================================================
	-- Launchers
	-- =========================================================================

	launchers = {
		session = {
			power = {
				backdrop = true,
				first_action = "hibernate",
				show_both_sleep_actions = false,
			},

			logoff = {
				backdrop = true,
				show_switch_user = false,
			},
		},

		run = {
			web_engine = "https://duckduckgo.com/?q=%s",
		},
	},

	-- =========================================================================
	-- Menu
	-- =========================================================================

	menu = {
		dynamic_labels = true,
		tabs = true,
	},

	-- =========================================================================
	-- Notifications
	-- =========================================================================

	notify = {
		timeout = 5,
		speech = false,
		screen = "focused",

		history = {
			max_entries = 100,
		},

		center = {
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
