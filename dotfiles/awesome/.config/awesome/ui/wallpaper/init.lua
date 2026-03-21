-- ~/.config/awesome/ui/wallpaper/init.lua
local function safe_require(path)
	local ok, mod = pcall(require, path)
	if ok then
		return mod
	end

	return nil
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
		spec.source = Source.resolve_for_screen(s, spec)
	end

	return spec
end

local function resolved_global_spec()
	local Config = config_api()
	local Source = source_api()

	if not (Config and type(Config.global_spec) == "function") then
		return nil
	end

	local spec = Config.global_spec()
	if not spec then
		return nil
	end

	if Source and type(Source.resolve_global) == "function" then
		spec.source = Source.resolve_global(spec)
	elseif Source and type(Source.resolve_for_screen) == "function" then
		spec.source = Source.resolve_for_screen(nil, spec)
	end

	return spec
end

local function sync_rotation()
	local Rotation = rotation_api()
	local Config = config_api()

	if not (Rotation and Config and type(Rotation.sync_all) == "function") then
		return
	end

	if type(Config.span_across_screens) == "function" and Config.span_across_screens() == true then
		Rotation.stop_all()
		return
	end

	Rotation.sync_all(function(s)
		if type(Config.screen_spec) == "function" then
			return Config.screen_spec(s)
		end

		return nil
	end)
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	args = args or {}

	M.api = {
		ui = args.ui or {},
		config = safe_require("ui.wallpaper.config"),
		source = safe_require("ui.wallpaper.source"),
		colors = safe_require("ui.wallpaper.colors"),
		scope = safe_require("ui.wallpaper.scope"),
		apply = safe_require("ui.wallpaper.apply"),
		refresh = safe_require("ui.wallpaper.refresh"),
		rotation = safe_require("ui.wallpaper.rotation"),
	}

	local cfg = args.cfg or args or {}
	local Refresh = refresh_api()
	local Rotation = rotation_api()

	set_runtime_cfg(cfg)

	if Refresh and type(Refresh.init) == "function" then
		Refresh.init({
			cfg = runtime_cfg,
			refresh = M.refresh,
		})
	end

	if Rotation and type(Rotation.init) == "function" then
		Rotation.init({
			cfg = runtime_cfg,
			refresh = M.refresh,
			spec_for_screen = function(s)
				local Config = config_api()
				if not (Config and type(Config.screen_spec) == "function") then
					return nil
				end

				return Config.screen_spec(s)
			end,
			source_api = source_api(),
		})
	end

	M.refresh()

	return M
end

function M.refresh()
	local Config = config_api()
	local Scope = scope_api()
	local Apply = apply_api()
	local Colors = colors_api()

	if not (Config and Apply) then
		return
	end

	-- Ausnahme: ein Wallpaper über die gesamte Desktopfläche spannen
	if type(Config.span_across_screens) == "function" and Config.span_across_screens() == true then
		local spec = resolved_global_spec()
		if not spec then
			return
		end

		for s in screen do
			if type(Apply.apply_desktop) == "function" then
				Apply.apply_desktop(s, spec, Scope, Colors)
			elseif type(Apply.apply_screen) == "function" then
				Apply.apply_screen(s, spec, Colors)
			end
		end

		sync_rotation()
		return
	end

	-- Default: alle Screens, mit optionalen screen[index]-Overrides
	for s in screen do
		local spec = resolved_spec_for_screen(s)

		if spec and type(Apply.apply_screen) == "function" then
			Apply.apply_screen(s, spec, Colors)
		end
	end

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
