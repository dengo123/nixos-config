-- ~/.config/awesome/shell/windowing/behavior/titlebars.lua
local M = {}

local runtime = {
	windowing = {},
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function windowing()
	return runtime.windowing or {}
end

local function cfg()
	return windowing().cfg or {}
end

local function clients_api()
	return windowing().clients
end

local function titlebars_cfg()
	local windowing_cfg = cfg().windowing or {}
	local value = windowing_cfg.titlebars

	if type(value) == "table" then
		return value
	end

	if value == false then
		return {
			show = "off",
			exclude = {},
		}
	end

	return {
		show = "on",
		exclude = {},
	}
end

local function titlebar_show()
	local show = tostring(titlebars_cfg().show or titlebars_cfg().mode or "on"):lower()

	if show == "off" then
		return "off"
	end

	if show == "floating_only" then
		return "floating_only"
	end

	return "on"
end

local function titlebar_exclude()
	local list = titlebars_cfg().exclude
	if type(list) ~= "table" then
		return {}
	end

	return list
end

local function client_name(c)
	return tostring(c and c.name or ""):lower()
end

local function token_matches_client(token, c)
	token = tostring(token or ""):lower()

	local Clients = clients_api()
	local apps_cfg = cfg().apps or {}

	local fm_match = Clients and Clients.build_file_manager_match and Clients.build_file_manager_match(apps_cfg.files)
		or nil
	local term_match = Clients and Clients.build_terminal_match and Clients.build_terminal_match(apps_cfg.terminal)
		or nil

	local class = (Clients and Clients.client_class and Clients.client_class(c)) or ""
	local instance = (Clients and Clients.client_instance and Clients.client_instance(c)) or ""
	local name = client_name(c)

	if token == "terminal" then
		return (Clients and Clients.client_matches and Clients.client_matches(c, term_match)) or false
	end

	if token == "files" then
		return (Clients and Clients.client_matches and Clients.client_matches(c, fm_match)) or false
	end

	if token == "copyq" then
		return (Clients and Clients.is_copyq_client and Clients.is_copyq_client(c)) or false
	end

	if token == "calendar" then
		return class == "gnome-calendar" or class == "org.gnome.calendar" or instance == "gnome-calendar"
	end

	if token == "browser" then
		local browser = tostring((apps_cfg.browser or "")):lower()
		local exe = browser:match("^%s*([^%s]+)")
		exe = exe and (exe:match("([^/]+)$") or exe):lower() or nil
		return exe ~= nil and (class == exe or instance == exe)
	end

	if token == "editor" then
		local editor = tostring((apps_cfg.editor or "")):lower()
		local exe = editor:match("^%s*([^%s]+)")
		exe = exe and (exe:match("([^/]+)$") or exe):lower() or nil
		return (exe ~= nil and (class == exe or instance == exe)) or class == "emacs" or instance == "emacs"
	end

	return token == class or token == instance or token == name
end

local function excluded(c)
	for _, token in ipairs(titlebar_exclude()) do
		if token_matches_client(token, c) then
			return true
		end
	end

	return false
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	args = args or {}
	runtime.windowing = args.windowing or runtime.windowing
	return M
end

function M.show()
	return titlebar_show()
end

function M.mode()
	return titlebar_show()
end

function M.enabled_for(c, _windowing)
	local show = titlebar_show()

	if show == "off" then
		return false
	end

	if excluded(c) then
		return false
	end

	if show == "floating_only" then
		return c.floating == true
	end

	return true
end

return M
