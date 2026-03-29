-- ~/.config/awesome/shell/example/init.lua
local function safe_require(path)
	local ok, mod = pcall(require, path)
	if ok then
		return mod
	end

	return nil
end

local M = {
	input = nil,
	runtime = {},
	ui_mods = {},
	policy = {},
}

local runtime = {
	ctx = {},
	example = {},
	initialized = false,
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function ctx()
	return runtime.ctx or {}
end

local function cfg()
	return ctx().cfg or {}
end

local function ui()
	return ctx().ui or {}
end

local function shell()
	return ctx().shell or {}
end

local function init_module(mod, args)
	if mod and type(mod.init) == "function" then
		return mod.init(args)
	end

	return mod
end

local function apply_module(mod, args)
	if mod and type(mod.apply) == "function" then
		return mod.apply(args)
	end

	return mod
end

local function build_example()
	local conf = cfg()

	return {
		cfg = conf,
		ui = ui(),
		shell = shell(),

		example_cfg = conf.example or {},

		runtime = M.runtime or {},
		ui_mods = M.ui_mods or {},
		policy = M.policy or {},

		controller = M.runtime.controller,
		actions = M.runtime.actions,
		signals = M.runtime.signals,

		popup = M.ui_mods.popup,
		theme = M.ui_mods.theme,
	}
end

local function build_input(example)
	return {
		main = {
			open = example.actions and example.actions.open or nil,
			close = example.actions and example.actions.close or nil,
			toggle = example.actions and example.actions.toggle or nil,
		},
	}
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	runtime.ctx = args or {}

	if runtime.initialized then
		return M
	end

	M.runtime = {
		controller = safe_require("shell.example.runtime.controller"),
		actions = safe_require("shell.example.runtime.actions"),
		signals = safe_require("shell.example.runtime.signals"),
	}

	M.ui_mods = {
		popup = safe_require("shell.example.ui.popup"),
		theme = safe_require("shell.example.ui.theme"),
	}

	M.policy = {
		rules = safe_require("shell.example.policy.rules"),
	}

	runtime.example = build_example()
	local example = runtime.example

	init_module(example.theme, {
		cfg = example.cfg,
		ui = example.ui,
	})

	init_module(example.controller, {
		example = example,
	})

	init_module(example.actions, {
		example = example,
	})

	init_module(example.signals, {
		example = example,
	})

	apply_module(example.rules, {
		example = example,
	})

	M.input = build_input(example)

	runtime.initialized = true
	return M
end

return M
