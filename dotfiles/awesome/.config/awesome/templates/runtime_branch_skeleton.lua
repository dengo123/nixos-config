-- ~/.config/awesome/<tree>/runtime/<name>.lua
local M = {}

local runtime = {
	parent = {},
	state = {},
	signals_ready = false,
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function parent()
	return runtime.parent or {}
end

local function cfg()
	return parent().cfg or {}
end

local function ui()
	return parent().ui or {}
end

local function shell()
	return parent().shell or {}
end

local function state()
	return runtime.state or {}
end

local function set_signals_ready(v)
	runtime.signals_ready = (v == true)
end

local function signals_ready()
	return runtime.signals_ready == true
end

local function build_state()
	return {
		cfg = cfg(),
		ui = ui(),
		shell = shell(),

		active = false,
		cache = {},
	}
end

local function register_signals()
	if signals_ready() then
		return
	end

	set_signals_ready(true)

	-- Beispiel:
	-- client.connect_signal("manage", function(c)
	-- 	...
	-- end)
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	args = args or {}

	runtime.parent = args.parent or args.runtime_parent or args.module or {}
	runtime.state = build_state()

	register_signals()

	return M
end

function M.refresh()
	runtime.state = build_state()
	return M
end

function M.reset()
	local s = state()
	s.active = false
	s.cache = {}
	return M
end

function M.get_state()
	return state()
end

return M
