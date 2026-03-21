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
	-- UI / Wallpaper
	-- =========================================================================

	ui = {
		wallpaper = {
			-- source = "~/Pictures/wallpapers",
			-- span_across_screens = true,

			fit = {
				enabled = false,
				style = "solid",
			},

			rotation = {
				enabled = false,
				interval = 600,
				random = false,
			},

			screen = {
				[2] = {
					source = "~/Pictures/wallpapers/nixos_waves.png",
					fit = {
						enabled = true,
						style = "gradient",
					},
				},
			},
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

		screen = "primary_only", -- all | primary_only | landscape_only | off

		reveal_trigger_px = 2,
		reveal_hide_delay = 0.20,
		layout_peek_duration = 0.7,

		start = {
			show = "on", -- on | visible_bar_only | off
			action = "menu", -- menu | rofi | terminal | editor
		},

		notify = {
			show = "visible_bar_only", -- on | visible_bar_only | off
		},

		clock = {
			show_seconds = false,
			calendar_enable = true,
			calendar_use_menu_theme = false,
		},
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
				"tile_main", -- tile_cross as alternative tile layouts
			},
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
			show = "on", -- on | floating_only | off
			exclude = { "terminal", "editor" },
		},

		rounded_corners = true,

		focus = {
			raise_on_mouse = false,
			block_ms = 150,

			center_mouse = {
				enable = false,
				exclude_layouts = { "max" },
				exclude_states = { "fullscreen" },
			},
		},

		floating = {
			files = true,
			terminals = true,
		},

		fullscreen_dim = {
			enabled = true,
			never_dim_primary = true,
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
