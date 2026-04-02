-- ~/.config/awesome/system/config.lua
return {
	-- =========================================================================
	-- System
	-- =========================================================================

	system = {
		autostart = {
			copyq_enable = true,
			picom_enable = true,
		},

		session_state = {
			path = nil,
			restore = {
				-- on_start = false,
				-- state = false,
				-- layout = false,
				screen = true,
				tag = true,
			},
			preserve_rule_floating = {
				-- dialog = false,
				-- utility = false,
				portrait_autosize = false,
				centered_autosize = false,
			},
		},
	},

	-- =========================================================================
	-- Apps
	-- =========================================================================

	apps = {
		terminal = "kitty",
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
		return_app = "editor", -- or terminal

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
		-- theme = "luna",
		wallpaper = {
			-- source = "~/Pictures/wallpapers/TreasurePlanet_collab_done.png",

			display = {
				mode = "maximized", -- "maximized | fit_fill | center | strech"
				span_across_screens = true,
			},

			-- rules = {
			-- 	{
			-- 		match = { orientation = "portrait" }, -- index = num, or orientation = "landscape | portrait"
			-- 		source = "~/Pictures/wallpapers/nixos_waves.png",
			-- 		display = {
			-- 			mode = "fit_fill",
			-- 			fill = "solid",
			-- 			span_across_screens = false,
			-- 		},
			-- 	},
			-- },

			-- rotation = {
			-- 	enabled = true,
			-- 	interval = 600,
			-- 	random = false,
			-- },
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

		max = {
			padding = true,
			solo = true,
		},

		delete = {
			kill_clients = false,
			soft_mode = "exclusive",
		},
	},

	-- =========================================================================
	-- Windowing
	-- =========================================================================

	windowing = {
		titlebars = {
			show = "floating_only", -- on | floating_only | off
			exclude = { "terminal" },
		},

		rounded_corners = false, -- prefere picom

		focus = {
			raise_on_mouse = false,
			-- block_ms = 150,

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
	},

	-- =========================================================================
	-- Launchers
	-- =========================================================================

	launchers = {
		session = {
			power = {
				first_action = "hibernate",
				show_both_sleep_actions = false,
			},

			logoff = {
				show_switch_user = false,
			},
		},

		run = {
			web_engine = "google",
		},
	},

	-- =========================================================================
	-- Menu
	-- =========================================================================

	menu = {
		dynamic_labels = false,
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

			visible_cards = 5,
			visible_cards_portrait = 7,
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
