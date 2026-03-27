-- ~/.config/awesome/shell/windowing/policy/clients.lua
local M = {}

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

local function client_class(c)
	return tostring(c and c.class or ""):lower()
end

local function client_instance(c)
	return tostring(c and c.instance or ""):lower()
end

local function client_name(c)
	return tostring(c and c.name or ""):lower()
end

-- =========================================================================
-- Match Builders
-- =========================================================================

function M.build_file_manager_match(file_manager)
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

function M.build_terminal_match(terminal)
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

-- =========================================================================
-- Match Checks
-- =========================================================================

function M.has_entries(t)
	return has_entries(t)
end

function M.client_matches(c, spec)
	if not (c and c.valid and spec) then
		return false
	end

	local class = client_class(c)
	local instance = client_instance(c)
	local name = client_name(c)

	for _, v in ipairs(spec.class or {}) do
		if class == tostring(v):lower() then
			return true
		end
	end

	for _, v in ipairs(spec.instance or {}) do
		if instance == tostring(v):lower() then
			return true
		end
	end

	for _, v in ipairs(spec.name or {}) do
		if name == tostring(v):lower() then
			return true
		end
	end

	return false
end

-- =========================================================================
-- Client Kinds
-- =========================================================================

function M.is_terminal_client(c)
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

function M.is_copyq_client(c)
	local class = client_class(c)
	local instance = client_instance(c)

	return class == "copyq" or instance == "copyq"
end

function M.is_tall_centered_client(c)
	return M.is_terminal_client(c) or M.is_copyq_client(c)
end

-- =========================================================================
-- Accessors
-- =========================================================================

function M.client_class(c)
	return client_class(c)
end

function M.client_instance(c)
	return client_instance(c)
end

function M.client_name(c)
	return client_name(c)
end

return M
