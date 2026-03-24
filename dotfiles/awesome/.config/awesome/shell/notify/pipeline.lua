-- ~/.config/awesome/shell/notify/pipeline.lua
local gears = require("gears")
local naughty = require("naughty")

local M = {}

local runtime = {
	callback_ready = false,
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function notify_cfg(cfg)
	return (cfg and cfg.notify) or {}
end

local function filter_cfg(cfg)
	return notify_cfg(cfg).filter or {}
end

local function sanitize_markup_text(value)
	if value == nil then
		return nil
	end

	return gears.string.xml_escape(tostring(value))
end

local function sanitize_notify_args(args)
	if type(args) ~= "table" then
		return args
	end

	args.title = sanitize_markup_text(args.title)
	args.text = sanitize_markup_text(args.text)
	args.message = sanitize_markup_text(args.message)
	args.app_name = sanitize_markup_text(args.app_name)

	return args
end

local function is_ignored_app(cfg, args)
	local apps = filter_cfg(cfg).apps or {}
	local app_name = tostring(args.app_name or args.appname or args.app or "")

	for _, name in ipairs(apps) do
		if app_name == tostring(name) then
			return true
		end
	end

	return false
end

local function should_store_notification(cfg, args)
	if args.skip_history == true then
		return false
	end

	local fcfg = filter_cfg(cfg)

	if fcfg.ignore_resident == true and args.resident == true then
		return false
	end

	if fcfg.ignore_silent == true and args.ignore == true then
		return false
	end

	if is_ignored_app(cfg, args) then
		return false
	end

	return true
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.register(args)
	args = args or {}

	if runtime.callback_ready then
		return
	end

	runtime.callback_ready = true

	local cfg = args.cfg or {}
	local History = args.history
	local prev_callback = naughty.config.notify_callback

	naughty.config.notify_callback = function(nargs)
		if type(prev_callback) == "function" then
			nargs = prev_callback(nargs) or nargs
		end

		nargs = sanitize_notify_args(nargs or {})

		if should_store_notification(cfg, nargs) and History and type(History.add) == "function" then
			History.add(nargs)
		end

		return nargs
	end
end

return M
