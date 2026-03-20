-- ~/.config/awesome/shell/windowing/runtime/state.lua
local M = {}

-- =========================================================================
-- Layout State
-- =========================================================================

function M.layout_mode(cfg)
	local tags_cfg = (cfg and cfg.tags) or {}
	local layouts_cfg = tags_cfg.layouts or {}
	local mode = tostring(layouts_cfg.mode or "tiling"):lower()

	if mode == "floating" then
		return "floating"
	end

	return "tiling"
end

function M.layout_state_mode(cfg)
	if M.layout_mode(cfg) == "floating" then
		return "maximized"
	end

	return "floating"
end

function M.is_layout_state_active(c, cfg)
	if not (c and c.valid) then
		return false
	end

	local mode = M.layout_state_mode(cfg)

	if mode == "maximized" then
		return c.maximized == true
	end

	return c.floating == true
end

function M.layout_state_label(cfg)
	local mode = M.layout_state_mode(cfg)

	if mode == "maximized" then
		return "toggle maximized"
	end

	return "toggle floating"
end

function M.layout_state_menu_label(cfg)
	local mode = M.layout_state_mode(cfg)

	if mode == "maximized" then
		return "Maximize / Restore"
	end

	return "Floating / Tiling"
end

return M
