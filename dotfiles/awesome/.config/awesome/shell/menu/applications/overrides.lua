-- ~/.config/awesome/shell/menu/applications/overrides.lua
local M = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function first_string(...)
	for i = 1, select("#", ...) do
		local v = select(i, ...)
		if type(v) == "string" and v ~= "" then
			return v
		end
	end

	return nil
end

local function lowercase(value)
	if type(value) ~= "string" then
		return ""
	end

	return value:lower()
end

local function entry_name(entry)
	return lowercase(
		first_string(
			entry and entry.Name,
			entry and entry.name,
			entry and entry.label,
			entry and entry.text,
			entry and entry[1]
		)
	)
end

local function entry_exec(entry)
	local second = entry and entry[2]
	local second_string = type(second) == "string" and second or nil

	return lowercase(
		first_string(
			entry and entry.cmdline,
			entry and entry.cmd,
			entry and entry.Exec,
			entry and entry.exec,
			entry and entry.command,
			second_string
		)
	)
end

local function contains(haystack, needle)
	return haystack ~= "" and needle ~= "" and haystack:find(needle, 1, true) ~= nil
end

local function any_contains(haystack, needles)
	for _, needle in ipairs(needles or {}) do
		if contains(haystack, needle) then
			return true
		end
	end

	return false
end

local function matches(entry, patterns)
	local name = entry_name(entry)
	local exec = entry_exec(entry)

	return any_contains(name, patterns) or any_contains(exec, patterns)
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.hide(entry)
	local name = entry_name(entry)
	local exec = entry_exec(entry)

	if name == "xscreensaver" or exec == "xscreensaver" then
		return true
	end

	if
		matches(entry, {
			"picom",
			"protontricks",
			"druckerverwaltung",
			"cups",
			"hp-uiscan",
			"org-protocol",
		})
	then
		return true
	end

	if matches(entry, {
		"emacsclient-mail",
		"mimeinfo.cache",
	}) then
		return true
	end

	return false
end

function M.category(entry)
	-- Games
	if
		matches(entry, {
			"steam",
			"supertuxkart",
			"prismlauncher",
			"prism launcher",
			"minecraft",
			"lutris",
		})
	then
		return "Games"
	end

	-- Development
	if matches(entry, {
		"emacs",
		"emacsclient",
	}) then
		return "Development"
	end

	-- Office
	if
		matches(entry, {
			"textmaker",
			"planmaker",
			"presentations",
			"freeoffice",
			"okular",
			"libreoffice",
			"writer",
			"calc",
			"impress",
		})
	then
		return "Office"
	end

	-- Multimedia / Viewer
	if
		matches(entry, {
			"bildbetrachter",
			"celluloid",
			"vlc",
			"mpv",
			"audacity",
			"spotify",
			"obs",
			"calibre",
			"e-book editor",
			"e-book viewer",
			"lrf viewer",
		})
	then
		return "Multimedia"
	end

	-- Internet
	if
		matches(entry, {
			"firefox",
			"chromium",
			"google-chrome",
			"brave",
			"tor browser",
			"thunderbird",
			"signal",
			"discord",
			"telegram",
			"newsflash",
			"localsend",
		})
	then
		return "Internet"
	end

	-- Graphics
	if matches(entry, {
		"gimp",
		"inkscape",
		"krita",
		"blender",
		"nomacs",
	}) then
		return "Graphics"
	end

	-- System
	if
		matches(entry, {
			"nvidia x-server",
			"nvidia-settings",
			"lact",
			"laufwerke",
			"lautstärkeregler",
			"bluetooth manager",
			"network manager",
			"pavucontrol",
			"archivverwaltung",
			"file roller",
			"file manager",
			"files",
			"dateien",
			"kalender",
			"calendar",
			"copyq",
			"xterm",
			"druckeinstellungen",
			"printer",
			"screensaver-demo",
			"settings",
			"remote-viewer",
			"virt-manager",
			"virtuelle maschinenverwaltung",
			"virtual machine manager",
		})
	then
		return "System"
	end

	-- Terminal
	if matches(entry, {
		"htop",
		"btop",
		"nvtop",
		"tmux",
		"yazi",
		"lazygit",
	}) then
		return "Terminal"
	end

	return nil
end

function M.dedupe_key(entry)
	-- Emacs-Launcher zusammenfassen
	if matches(entry, {
		"emacs",
		"emacsclient",
	}) then
		return "app:emacs"
	end

	-- FreeOffice pro Programm deduplizieren
	if matches(entry, {
		"textmaker",
	}) then
		return "app:freeoffice:textmaker"
	end

	if matches(entry, {
		"planmaker",
	}) then
		return "app:freeoffice:planmaker"
	end

	if matches(entry, {
		"presentations",
	}) then
		return "app:freeoffice:presentations"
	end

	-- Screensaver-Einträge gezielt behandeln
	if matches(entry, {
		"xscreensaver-demo",
		"screensaver settings",
	}) then
		return "app:screensaver:settings"
	end

	if matches(entry, {
		"xscreensaver-command",
	}) then
		return "app:screensaver:command"
	end

	return nil
end

return M
