-- ~/.config/awesome/ui/wallpaper/init.lua
local function req(path)
	local ok, mod = pcall(require, path)
	if ok and mod ~= nil then
		return mod
	end

	error(("ui.wallpaper.init: require failed: %s\n%s"):format(path, tostring(mod)))
end

local M = {
	api = {},
}

local runtime_cfg = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function api()
	return M.api or {}
end

local function config_api()
	return api().config
end

local function source_api()
	return api().source
end

local function colors_api()
	return api().colors
end

local function scope_api()
	return api().scope
end

local function apply_api()
	return api().apply
end

local function refresh_api()
	return api().refresh
end

local function rotation_api()
	return api().rotation
end

local function set_runtime_cfg(cfg)
	runtime_cfg = cfg or {}

	local Config = config_api()
	local Source = source_api()
	local Refresh = refresh_api()
	local Rotation = rotation_api()

	if Config and type(Config.set_runtime_cfg) == "function" then
		Config.set_runtime_cfg(runtime_cfg)
	end

	if Source and type(Source.set_runtime_cfg) == "function" then
		Source.set_runtime_cfg(runtime_cfg)
	end

	if Refresh and type(Refresh.set_runtime_cfg) == "function" then
		Refresh.set_runtime_cfg(runtime_cfg)
	end

	if Rotation and type(Rotation.set_runtime_cfg) == "function" then
		Rotation.set_runtime_cfg(runtime_cfg)
	end
end

local function resolved_spec_for_screen(s)
	local Config = config_api()
	local Source = source_api()

	if not (Config and type(Config.screen_spec) == "function") then
		return nil
	end

	local spec = Config.screen_spec(s)
	if not spec then
		return nil
	end

	if Source and type(Source.resolve_for_screen) == "function" then
		local resolved = Source.resolve_for_screen(s, spec)

		if type(resolved) == "string" and resolved ~= "" then
			spec.source = resolved
		end
	end

	return spec
end

local function desktop_entries()
	local Config = config_api()
	local Source = source_api()
	local out = {}

	if not (Config and type(Config.screens_for_desktop_span) == "function") then
		return out
	end

	for _, entry in ipairs(Config.screens_for_desktop_span() or {}) do
		local s = entry.screen
		local spec = entry.spec

		if s and spec then
			if Source and type(Source.resolve_for_screen) == "function" then
				local resolved = Source.resolve_for_screen(s, spec)

				if type(resolved) == "string" and resolved ~= "" then
					spec.source = resolved
				end
			end

			table.insert(out, {
				screen = s,
				spec = spec,
			})
		end
	end

	return out
end

local function has_desktop_span()
	return #(desktop_entries()) > 0
end

local function sync_rotation()
	local Rotation = rotation_api()
	local Config = config_api()

	if not (Rotation and type(Rotation.sync_all) == "function") then
		return
	end

	if has_desktop_span() then
		if type(Rotation.stop_all) == "function" then
			Rotation.stop_all()
		end
		return
	end

	Rotation.sync_all(function(s)
		if Config and type(Config.screen_spec) == "function" then
			return Config.screen_spec(s)
		end

		return nil
	end)
end

local function apply_desktop_wallpapers()
	local Apply = apply_api()
	local Scope = scope_api()
	local Colors = colors_api()

	if not (Apply and type(Apply.apply_desktop) == "function") then
		return
	end

	for _, entry in ipairs(desktop_entries()) do
		Apply.apply_desktop(entry.screen, entry.spec, Scope, Colors)
	end
end

local function apply_screen_wallpapers()
	local Apply = apply_api()
	local Colors = colors_api()
	local Config = config_api()

	if not (Apply and type(Apply.apply_screen) == "function") then
		return
	end

	for s in screen do
		local spans_desktop = Config
			and type(Config.screen_spans_desktop) == "function"
			and Config.screen_spans_desktop(s)

		if not spans_desktop then
			local spec = resolved_spec_for_screen(s)

			if spec then
				Apply.apply_screen(s, spec, Colors)
			end
		end
	end
end

local function init_refresh()
	local Refresh = refresh_api()

	if Refresh and type(Refresh.init) == "function" then
		Refresh.init({
			cfg = runtime_cfg,
			refresh = M.refresh,
		})
	end
end

local function init_rotation()
	local Rotation = rotation_api()
	local Config = config_api()

	if not (Rotation and type(Rotation.init) == "function") then
		return
	end

	Rotation.init({
		cfg = runtime_cfg,
		refresh = M.refresh,
		spec_for_screen = function(s)
			if Config and type(Config.screen_spec) == "function" then
				return Config.screen_spec(s)
			end

			return nil
		end,
		source_api = source_api(),
	})
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	args = args or {}

	M.api = {
		ui = args.ui or {},
		config = req("ui.wallpaper.config"),
		source = req("ui.wallpaper.source"),
		colors = req("ui.wallpaper.colors"),
		scope = req("ui.wallpaper.scope"),
		apply = req("ui.wallpaper.apply"),
		refresh = req("ui.wallpaper.refresh"),
		rotation = req("ui.wallpaper.rotation"),
	}

	set_runtime_cfg(args.cfg or args or {})

	local Source = source_api()
	if Source and type(Source.reset_all) == "function" then
		Source.reset_all()
	end

	init_refresh()
	init_rotation()

	M.refresh()

	return M
end

function M.refresh()
	apply_desktop_wallpapers()
	apply_screen_wallpapers()
	sync_rotation()
end

function M.set(source)
	runtime_cfg.ui = runtime_cfg.ui or {}
	runtime_cfg.ui.wallpaper = runtime_cfg.ui.wallpaper or {}
	runtime_cfg.ui.wallpaper.source = source

	local Source = source_api()
	if Source and type(Source.reset_all) == "function" then
		Source.reset_all()
	end

	set_runtime_cfg(runtime_cfg)
	M.refresh()
end

return M
