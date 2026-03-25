-- ~/.config/awesome/shell/menu/applications/init.lua
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

local runtime = {
	items = {},
	loaded = false,
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function api()
	return M.api or {}
end

local function mod(name)
	return api()[name]
end

local function init_submodule(name, args)
	local sub = mod(name)

	if sub and type(sub.init) == "function" then
		sub.init(args)
	end
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	args = args or {}

	M.api = {
		loader = safe_require("shell.menu.applications.loader"),
		dedupe = safe_require("shell.menu.applications.dedupe"),
		builder = safe_require("shell.menu.applications.builder"),
		categories = safe_require("shell.menu.applications.categories"),
		overrides = safe_require("shell.menu.applications.overrides"),
	}

	local shared = {
		api = api(),
		cfg = args.cfg or {},
		ui = args.ui or {},
	}

	init_submodule("loader", shared)
	init_submodule("dedupe", shared)
	init_submodule("builder", shared)

	runtime.items = {}
	runtime.loaded = false

	return M
end

function M.load(callback)
	local Loader = mod("loader")
	local Dedupe = mod("dedupe")
	local Builder = mod("builder")

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
