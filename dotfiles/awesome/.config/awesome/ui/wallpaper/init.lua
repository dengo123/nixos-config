-- ~/.config/awesome/ui/wallpaper/init.lua
local function req(path)
	local ok, mod = pcall(require, path)
	if ok and mod ~= nil then
		return mod
	end

	error(("ui.wallpaper.init: require failed: %s\n%s"):format(path, tostring(mod)))
end

local M = {}

local runtime = {
	cfg = {},
	ui = {},
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function set_runtime_cfg(cfg)
	runtime.cfg = cfg or {}

	local Controller = M.controller
	local Source = M.source
	local Scope = M.scope
	local RefreshMod = M.refresh_mod
	local RotationMod = M.rotation_mod

	if Controller and type(Controller.set_runtime_cfg) == "function" then
		Controller.set_runtime_cfg(runtime.cfg)
	end

	if Source and type(Source.set_runtime_cfg) == "function" then
		Source.set_runtime_cfg(runtime.cfg)
	end

	if Scope and type(Scope.set_runtime_cfg) == "function" then
		Scope.set_runtime_cfg(runtime.cfg)
	end

	if RefreshMod and type(RefreshMod.set_runtime_cfg) == "function" then
		RefreshMod.set_runtime_cfg(runtime.cfg)
	end

	if RotationMod and type(RotationMod.set_runtime_cfg) == "function" then
		RotationMod.set_runtime_cfg(runtime.cfg)
	end
end

local function resolved_spec_for_screen(s)
	local Controller = M.controller
	local Source = M.source

	if not (Controller and type(Controller.screen_spec) == "function") then
		return nil
	end

	local spec = Controller.screen_spec(s)
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
	local Controller = M.controller
	local Source = M.source
	local out = {}

	if not (Controller and type(Controller.screens_for_desktop_span) == "function") then
		return out
	end

	for _, entry in ipairs(Controller.screens_for_desktop_span() or {}) do
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
	local RotationMod = M.rotation_mod
	local Controller = M.controller

	if not (RotationMod and type(RotationMod.sync_all) == "function") then
		return
	end

	if has_desktop_span() then
		if type(RotationMod.stop_all) == "function" then
			RotationMod.stop_all()
		end
		return
	end

	RotationMod.sync_all(function(s)
		if Controller and type(Controller.screen_spec) == "function" then
			return Controller.screen_spec(s)
		end

		return nil
	end)
end

local function apply_desktop_wallpapers()
	local Apply = M.apply
	local Scope = M.scope
	local Colors = M.colors

	if not (Apply and type(Apply.apply_desktop) == "function") then
		return
	end

	for _, entry in ipairs(desktop_entries()) do
		Apply.apply_desktop(entry.screen, entry.spec, Scope, Colors)
	end
end

local function apply_screen_wallpapers()
	local Apply = M.apply
	local Colors = M.colors
	local Controller = M.controller

	if not (Apply and type(Apply.apply_screen) == "function") then
		return
	end

	for s in screen do
		local spans_desktop = Controller
			and type(Controller.screen_spans_desktop) == "function"
			and Controller.screen_spans_desktop(s)

		if not spans_desktop then
			local spec = resolved_spec_for_screen(s)

			if spec then
				Apply.apply_screen(s, spec, Colors)
			end
		end
	end
end

local function init_refresh()
	local RefreshMod = M.refresh_mod

	if RefreshMod and type(RefreshMod.init) == "function" then
		RefreshMod.init({
			cfg = runtime.cfg,
			refresh = M.refresh,
		})
	end
end

local function init_rotation()
	local RotationMod = M.rotation_mod
	local Controller = M.controller

	if not (RotationMod and type(RotationMod.init) == "function") then
		return
	end

	RotationMod.init({
		cfg = runtime.cfg,
		refresh = M.refresh,
		spec_for_screen = function(s)
			if Controller and type(Controller.screen_spec) == "function" then
				return Controller.screen_spec(s)
			end

			return nil
		end,
		source = M.source,
	})
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	args = args or {}

	runtime.ui = args.ui or runtime.ui or {}

	M.ui = runtime.ui
	M.controller = req("ui.wallpaper.controller")
	M.source = req("ui.wallpaper.source")
	M.colors = req("ui.wallpaper.colors")
	M.scope = req("ui.wallpaper.scope")
	M.apply = req("ui.wallpaper.apply")
	M.refresh_mod = req("ui.wallpaper.refresh")
	M.rotation_mod = req("ui.wallpaper.rotation")

	set_runtime_cfg(args.cfg or args or {})

	if M.source and type(M.source.reset_all) == "function" then
		M.source.reset_all()
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
	runtime.cfg.ui = runtime.cfg.ui or {}
	runtime.cfg.ui.wallpaper = runtime.cfg.ui.wallpaper or {}
	runtime.cfg.ui.wallpaper.source = source

	if M.source and type(M.source.reset_all) == "function" then
		M.source.reset_all()
	end

	set_runtime_cfg(runtime.cfg)
	M.refresh()
end

return M
