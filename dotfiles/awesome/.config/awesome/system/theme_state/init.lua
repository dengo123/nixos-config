-- ~/.config/awesome/system/theme_state/init.lua
local function safe_require(path)
	local ok, mod = pcall(require, path)
	if ok then
		return mod
	end

	return nil
end

local M = {
	store = nil,
	apply = nil,
}

local runtime = {
	ctx = {},
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function ctx()
	return runtime.ctx or {}
end

local function ensure_ctx_roots()
	local c = ctx()
	c.system = c.system or {}
	return c
end

local function init_module(mod, args)
	if mod and type(mod.init) == "function" then
		mod.init(args)
	end
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	runtime.ctx = args or {}
	local c = ensure_ctx_roots()

	M.store = safe_require("system.theme_state.store")
	M.apply = safe_require("system.theme_state.apply")

	c.system.theme_state = M

	init_module(M.store, {})
	init_module(M.apply, {
		apply_on_init = true,
	})

	return M
end

function M.export(theme)
	local Store = M.store
	if Store and type(Store.export) == "function" then
		return Store.export(theme)
	end

	return false, "theme_state.store.export missing"
end

function M.apply_now()
	local Apply = M.apply
	if Apply and type(Apply.apply_now) == "function" then
		return Apply.apply_now()
	end

	return nil
end

return M
