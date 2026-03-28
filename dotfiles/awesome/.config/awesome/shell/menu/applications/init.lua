-- ~/.config/awesome/shell/menu/applications/init.lua
local function safe_require(path)
	local ok, mod = pcall(require, path)
	if ok then
		return mod
	end

	return nil
end

local M = {
	loader = nil,
	dedupe = nil,
	builder = nil,
	categories = nil,
	overrides = nil,
}

local runtime = {
	cfg = {},
	ui = {},
	items = {},
	loaded = false,
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function cfg()
	return runtime.cfg or {}
end

local function ui()
	return runtime.ui or {}
end

local function init_submodule(mod, opts)
	if mod and type(mod.init) == "function" then
		mod.init(opts)
	end
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(opts)
	opts = opts or {}

	runtime.cfg = opts.cfg or runtime.cfg
	runtime.ui = opts.ui or runtime.ui

	M.loader = safe_require("shell.menu.applications.loader")
	M.dedupe = safe_require("shell.menu.applications.dedupe")
	M.builder = safe_require("shell.menu.applications.builder")
	M.categories = safe_require("shell.menu.applications.categories")
	M.overrides = safe_require("shell.menu.applications.overrides")

	init_submodule(M.loader, {
		cfg = cfg(),
		ui = ui(),
	})

	init_submodule(M.dedupe, {
		overrides = M.overrides,
	})

	init_submodule(M.builder, {
		categories = M.categories,
		overrides = M.overrides,
	})

	runtime.items = {}
	runtime.loaded = false

	return M
end

function M.load(callback)
	local Loader = M.loader
	local Dedupe = M.dedupe
	local Builder = M.builder

	if not (Loader and type(Loader.load) == "function") then
		runtime.items = {}
		runtime.loaded = true

		if type(callback) == "function" then
			callback(runtime.items)
		end

		return
	end

	Loader.load(function(entries)
		local deduped = entries or {}

		if Dedupe and type(Dedupe.run) == "function" then
			deduped = Dedupe.run(deduped)
		end

		local built = deduped
		if Builder and type(Builder.build) == "function" then
			built = Builder.build(deduped)
		end

		runtime.items = built or {}
		runtime.loaded = true

		awesome.emit_signal("menu::applications_loaded", #runtime.items)

		if type(callback) == "function" then
			callback(runtime.items)
		end
	end)
end

function M.items()
	return runtime.items or {}
end

function M.is_loaded()
	return runtime.loaded == true
end

return M
