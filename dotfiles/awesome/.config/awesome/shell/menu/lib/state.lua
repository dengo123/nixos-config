-- ~/.config/awesome/shell/menu/lib/state.lua
local M = {}

local ctx = {
	ui = nil,
	cfg = nil,
	dialogs = nil,
}

local runtime = {
	menu = nil,
	keygrab = nil,
	root_buttons = nil,
	client_callback = nil,
}

-- =========================================================================
-- Context
-- =========================================================================

function M.set_context(args)
	args = args or {}

	ctx.ui = args.ui or {}
	ctx.cfg = args.cfg or {}
	ctx.dialogs = args.dialogs
end

function M.get_context()
	return ctx
end

-- =========================================================================
-- Runtime
-- =========================================================================

function M.get_menu()
	return runtime.menu
end

function M.set_menu(menu)
	runtime.menu = menu
end

function M.clear_menu()
	runtime.menu = nil
end

function M.get_keygrab()
	return runtime.keygrab
end

function M.set_keygrab(keygrab)
	runtime.keygrab = keygrab
end

function M.clear_keygrab()
	runtime.keygrab = nil
end

function M.get_root_buttons()
	return runtime.root_buttons
end

function M.set_root_buttons(buttons)
	runtime.root_buttons = buttons
end

function M.clear_root_buttons()
	runtime.root_buttons = nil
end

function M.get_client_callback()
	return runtime.client_callback
end

function M.set_client_callback(callback)
	runtime.client_callback = callback
end

function M.clear_client_callback()
	runtime.client_callback = nil
end

-- =========================================================================
-- Helpers
-- =========================================================================

function M.reset_runtime()
	runtime.menu = nil
	runtime.keygrab = nil
	runtime.root_buttons = nil
	runtime.client_callback = nil
end

return M
