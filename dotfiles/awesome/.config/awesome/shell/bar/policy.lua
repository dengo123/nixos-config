local awful = require("awful")

local M = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function is_landscape_screen(s)
	return s and s.geometry and s.geometry.width >= s.geometry.height
end

local function normalize_screen_policy(value, default)
	return tostring(value or default or "all"):lower()
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.matches_screen_policy(s, policy)
	local primary = screen.primary or awful.screen.focused()
	local is_primary = (s == primary)
	local is_landscape = is_landscape_screen(s)

	policy = normalize_screen_policy(policy, "all")

	if policy == "off" then
		return false
	end

	if policy == "primary_only" then
		return is_primary
	end

	if policy == "landscape_only" then
		return is_landscape
	end

	return true
end

function M.bar_enabled_on_screen(s, bar_cfg)
	bar_cfg = bar_cfg or {}
	return M.matches_screen_policy(s, bar_cfg.screen)
end

function M.start_enabled_on_screen(s, bar_cfg)
	bar_cfg = bar_cfg or {}

	local start_cfg = bar_cfg.start or {}

	if start_cfg.show ~= nil then
		return M.matches_screen_policy(s, start_cfg.show)
	end

	-- Backward compatibility
	if bar_cfg.start_on_primary_only == true then
		return M.matches_screen_policy(s, "primary_only")
	end

	return true
end

function M.start_action(bar_cfg)
	bar_cfg = bar_cfg or {}

	local start_cfg = bar_cfg.start or {}

	if start_cfg.action ~= nil then
		return tostring(start_cfg.action):lower()
	end

	-- Backward compatibility
	return tostring(bar_cfg.start_action or "menu"):lower()
end

function M.normalize_screen_policy(value, default)
	return normalize_screen_policy(value, default)
end

return M
