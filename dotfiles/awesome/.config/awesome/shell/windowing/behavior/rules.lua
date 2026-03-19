-- ~/.config/awesome/shell/windowing/behavior/rules.lua
local awful = require("awful")
local beautiful = require("beautiful")
local gears = require("gears")

local M = {}

local portrait_autosize_ready = false
local centered_autosize_ready = false
local tall_centered_reinforce_ready = false

-- =========================================================================
-- Helpers
-- =========================================================================

local function has_entries(t)
	return t and next(t) ~= nil
end

local function ucfirst(s)
	if not s or s == "" then
		return s
	end

	return s:sub(1, 1):upper() .. s:sub(2)
end

local function add_unique(t, v)
	if not v or v == "" then
		return
	end

	for _, existing in ipairs(t) do
		if existing == v then
			return
		end
	end

	table.insert(t, v)
end

local function extract_base_binary(cmd)
	if not cmd or cmd == "" then
		return nil
	end

	local bin = cmd:match("^%s*([^%s]+)")
	if not bin or bin == "" then
		return nil
	end

	local base = bin:match("([^/]+)$") or bin
	if not base or base == "" then
		return nil
	end

	return base
end

local function build_file_manager_match(file_manager)
	local base = extract_base_binary(file_manager)
	if not base then
		return nil
	end

	local classes = {}
	local instances = {}

	add_unique(instances, base)
	add_unique(classes, base)
	add_unique(classes, ucfirst(base))

	if base == "nautilus" then
		add_unique(classes, "org.gnome.Nautilus")
		add_unique(classes, "Org.gnome.Nautilus")
	elseif base == "pcmanfm-qt" then
		add_unique(classes, "Pcmanfm-qt")
	elseif base == "doublecmd" then
		add_unique(classes, "Double Commander")
	end

	return {
		class = classes,
		instance = instances,
	}
end

local function build_terminal_match(terminal)
	local base = extract_base_binary(terminal)
	if not base then
		return nil
	end

	local classes = {}
	local instances = {}

	add_unique(instances, base)
	add_unique(classes, base)
	add_unique(classes, ucfirst(base))

	if base == "gnome-terminal" then
		add_unique(classes, "Gnome-terminal")
		add_unique(classes, "org.gnome.Terminal")
		add_unique(classes, "Org.gnome.Terminal")
	elseif base == "wezterm" then
		add_unique(classes, "WezTerm")
	elseif base == "alacritty" then
		add_unique(classes, "Alacritty")
	elseif base == "kitty" then
		add_unique(classes, "Kitty")
	elseif base == "tilix" then
		add_unique(classes, "Tilix")
	elseif base == "konsole" then
		add_unique(classes, "Konsole")
	elseif base == "xterm" then
		add_unique(classes, "XTerm")
	end

	return {
		class = classes,
		instance = instances,
	}
end

local function client_class(c)
	return tostring(c and c.class or ""):lower()
end

local function client_instance(c)
	return tostring(c and c.instance or ""):lower()
end

local function is_terminal_client(c)
	local class = client_class(c)
	local instance = client_instance(c)

	return class == "xterm"
		or instance == "xterm"
		or class == "alacritty"
		or instance == "alacritty"
		or class == "kitty"
		or instance == "kitty"
		or class == "wezterm"
		or instance == "wezterm"
		or class == "gnome-terminal"
		or instance == "gnome-terminal"
		or class == "org.gnome.terminal"
		or instance == "org.gnome.terminal"
		or class == "konsole"
		or instance == "konsole"
end

local function is_copyq_client(c)
	local class = client_class(c)
	local instance = client_instance(c)

	return class == "copyq" or instance == "copyq"
end

local function is_tall_centered_client(c)
	return is_terminal_client(c) or is_copyq_client(c)
end

-- =========================================================================
-- Portrait Autosize
-- =========================================================================

local function portrait_autosize_apply(c)
	if not (c and c.valid) then
		return
	end

	if not c.portrait_autosize then
		return
	end

	if not c.floating or c.fullscreen or c.maximized then
		return
	end

	local s = c.screen
	if not s then
		return
	end

	local wa = s.workarea
	if wa.height <= wa.width then
		return
	end

	local w = math.floor(wa.width * 0.92)
	local h = math.floor(wa.height / 3)
	local x = wa.x + math.floor((wa.width - w) / 2)
	local y = wa.y + math.floor((wa.height - h) / 2)

	c:geometry({
		x = x,
		y = y,
		width = w,
		height = h,
	})
end

local function hook_portrait_autosize()
	if portrait_autosize_ready then
		return
	end

	client.connect_signal("manage", function(c)
		gears.timer.delayed_call(function()
			portrait_autosize_apply(c)
		end)
	end)

	client.connect_signal("property::floating", function(c)
		portrait_autosize_apply(c)
	end)

	screen.connect_signal("property::geometry", function(s)
		for _, c in ipairs(s.clients) do
			portrait_autosize_apply(c)
		end
	end)

	portrait_autosize_ready = true
end

-- =========================================================================
-- Centered Autosize
-- =========================================================================

local function centered_autosize_apply(c)
	if not (c and c.valid) then
		return
	end

	if not c.centered_autosize then
		return
	end

	if not c.floating or c.fullscreen then
		return
	end

	local s = c.screen
	if not s then
		return
	end

	local wa = s.workarea
	local width = math.floor(math.min(wa.width * 0.36, 620))

	width = math.max(width, 420)

	local height = math.floor(width * 1.60)
	local max_height = math.floor(wa.height * 0.90)

	if height > max_height then
		height = max_height
		width = math.floor(height / 1.60)
	end

	local x = wa.x + math.floor((wa.width - width) / 2)
	local y = wa.y + math.floor((wa.height - height) / 2)

	c.maximized = false
	c.maximized_horizontal = false
	c.maximized_vertical = false

	c:geometry({
		x = x,
		y = y,
		width = width,
		height = height,
	})
end

local function hook_centered_autosize()
	if centered_autosize_ready then
		return
	end

	client.connect_signal("manage", function(c)
		gears.timer.delayed_call(function()
			centered_autosize_apply(c)
		end)
	end)

	client.connect_signal("property::floating", function(c)
		centered_autosize_apply(c)
	end)

	screen.connect_signal("property::geometry", function(s)
		for _, c in ipairs(s.clients) do
			centered_autosize_apply(c)
		end
	end)

	centered_autosize_ready = true
end

-- =========================================================================
-- Tall Centered Reinforce
-- =========================================================================

local function normalize_tall_centered_client(c)
	if not (c and c.valid) then
		return
	end

	c.floating = true
	c.fullscreen = false
	c.maximized = false
	c.maximized_horizontal = false
	c.maximized_vertical = false
	c.centered_autosize = true
	c.portrait_autosize = false

	centered_autosize_apply(c)
end

local function reinforce_tall_centered_client(c)
	if not is_tall_centered_client(c) then
		return
	end

	normalize_tall_centered_client(c)

	gears.timer.delayed_call(function()
		normalize_tall_centered_client(c)
	end)

	gears.timer.start_new(0.15, function()
		normalize_tall_centered_client(c)
		return false
	end)

	gears.timer.start_new(0.50, function()
		normalize_tall_centered_client(c)
		return false
	end)
end

local function hook_tall_centered_reinforce()
	if tall_centered_reinforce_ready then
		return
	end

	client.connect_signal("property::maximized", function(c)
		if is_tall_centered_client(c) and c.floating then
			gears.timer.delayed_call(function()
				normalize_tall_centered_client(c)
			end)
		end
	end)

	client.connect_signal("property::fullscreen", function(c)
		if is_tall_centered_client(c) and c.floating then
			gears.timer.delayed_call(function()
				normalize_tall_centered_client(c)
			end)
		end
	end)

	tall_centered_reinforce_ready = true
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.apply(o)
	o = o or {}

	-- ---------------------------------------------------------------------
	-- Config
	-- ---------------------------------------------------------------------

	local modkey = o.modkey
	local mouse = o.mouse
	local cfg = o.cfg or {}

	local apps_cfg = cfg.apps or {}
	local windowing_cfg = cfg.windowing or {}
	local floating_cfg = windowing_cfg.floating or {}

	local file_manager = apps_cfg.files
	local terminal = apps_cfg.terminal

	local files_floating = (floating_cfg.files ~= false)
	local terminals_floating = (floating_cfg.terminals == true)
	local titlebars_enabled = (windowing_cfg.titlebars ~= false)

	local fm_match = build_file_manager_match(file_manager)
	local term_match = build_terminal_match(terminal)

	-- ---------------------------------------------------------------------
	-- Rules
	-- ---------------------------------------------------------------------

	local rules = {
		{
			rule = {},
			properties = {
				border_width = beautiful.border_width,
				border_color = beautiful.border_normal,
				focus = awful.client.focus.filter,
				raise = true,
				buttons = mouse and mouse.client_buttons and mouse.client_buttons(modkey) or nil,
				screen = awful.screen.preferred,
				placement = awful.placement.no_overlap + awful.placement.no_offscreen,
			},
		},
	}

	-- ---------------------------------------------------------------------
	-- File Manager
	-- ---------------------------------------------------------------------

	if files_floating and fm_match and (has_entries(fm_match.class) or has_entries(fm_match.instance)) then
		table.insert(rules, {
			rule_any = {
				class = has_entries(fm_match.class) and fm_match.class or nil,
				instance = has_entries(fm_match.instance) and fm_match.instance or nil,
			},
			properties = {
				floating = true,
				placement = awful.placement.centered,
				portrait_autosize = true,
				centered_autosize = false,
			},
		})
	end

	-- ---------------------------------------------------------------------
	-- Terminal
	-- ---------------------------------------------------------------------

	if terminals_floating and term_match and (has_entries(term_match.class) or has_entries(term_match.instance)) then
		table.insert(rules, {
			rule_any = {
				class = has_entries(term_match.class) and term_match.class or nil,
				instance = has_entries(term_match.instance) and term_match.instance or nil,
			},
			properties = {
				floating = true,
				placement = awful.placement.centered,
				portrait_autosize = false,
				centered_autosize = true,
			},
			callback = function(c)
				reinforce_tall_centered_client(c)
			end,
		})
	end

	-- ---------------------------------------------------------------------
	-- CopyQ
	-- ---------------------------------------------------------------------

	table.insert(rules, {
		rule_any = {
			class = { "copyq", "CopyQ" },
			instance = { "copyq" },
		},
		properties = {
			floating = true,
			placement = awful.placement.centered,
			portrait_autosize = false,
			centered_autosize = true,
		},
		callback = function(c)
			reinforce_tall_centered_client(c)
		end,
	})

	-- ---------------------------------------------------------------------
	-- Calendar
	-- ---------------------------------------------------------------------

	table.insert(rules, {
		rule_any = {
			class = {
				"gnome-calendar",
				"org.gnome.Calendar",
				"Org.gnome.Calendar",
				"Gnome-calendar",
				"Gnome-Calendar",
			},
			instance = { "gnome-calendar" },
		},
		properties = {
			floating = true,
			placement = awful.placement.centered,
			portrait_autosize = true,
			centered_autosize = false,
		},
		callback = function(c)
			portrait_autosize_apply(c)
		end,
	})

	-- ---------------------------------------------------------------------
	-- System Utilities
	-- ---------------------------------------------------------------------

	table.insert(rules, {
		rule_any = {
			class = {
				"Nm-connection-editor",
				"Blueman-manager",
				"Pavucontrol",
				"Gnome-disks",
				"Org.gnome.DiskUtility",
				"org.gnome.DiskUtility",
				".xscreensaver-demo-wrapped",
				"XScreenSaver",
			},
			instance = {
				"nm-connection-editor",
				"blueman-manager",
				"pavucontrol",
				"gnome-disks",
				".xscreensaver-demo-wrapped",
				"xscreensaver",
				"xscreensaver-demo",
			},
			name = {
				"Network Connections",
				"Bluetooth",
				"GNOME Disks",
				"Volume Control",
				"XScreenSaver",
				"XScreenSaver Preferences",
			},
		},
		properties = {
			floating = true,
			placement = awful.placement.centered,
			portrait_autosize = true,
			centered_autosize = false,
		},
	})

	-- ---------------------------------------------------------------------
	-- Generic Floaters
	-- ---------------------------------------------------------------------

	table.insert(rules, {
		rule_any = {
			type = { "dialog", "utility", "toolbar", "splash" },
			role = { "pop-up", "Preferences" },
		},
		properties = {
			floating = true,
			placement = awful.placement.centered,
			portrait_autosize = true,
			centered_autosize = false,
		},
	})

	-- ---------------------------------------------------------------------
	-- Titlebars
	-- ---------------------------------------------------------------------

	table.insert(rules, {
		rule_any = { type = { "normal", "dialog" } },
		properties = {
			titlebars_enabled = titlebars_enabled,
		},
	})

	-- ---------------------------------------------------------------------
	-- Callbacks
	-- ---------------------------------------------------------------------

	table.insert(rules, {
		rule = {},
		properties = {},
		callback = function(c)
			if c.name and c.name:match("^XScreenSaver:") then
				c.floating = true
				awful.placement.centered(c)
			end
		end,
	})

	awful.rules.rules = rules
	hook_portrait_autosize()
	hook_centered_autosize()
	hook_tall_centered_reinforce()
end

return M
