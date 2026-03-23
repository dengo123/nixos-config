-- ~/.config/awesome/ui/colors.lua
local Themes = require("ui.themes")

local M = {}

local runtime_cfg = {}

local DERIVED = {
	transparent = "#00000000",
	overlay_40 = "#00000066",
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function clamp(x, a, b)
	return math.max(a, math.min(b, x))
end

local function palette()
	local theme = Themes.resolve(runtime_cfg)
	local src = (theme and theme.colors) or {}
	local out = {}

	for k, v in pairs(src) do
		out[k] = v
	end

	for k, v in pairs(DERIVED) do
		if out[k] == nil then
			out[k] = v
		end
	end

	return out
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.set_runtime_cfg(cfg)
	runtime_cfg = cfg or {}
end

function M.get()
	return palette()
end

function M.resolve(value)
	local p = palette()

	if type(value) == "string" and p[value] then
		return p[value]
	end

	return value
end

function M.hex_to_rgb(hex)
	hex = M.resolve(hex)
	hex = tostring(hex or "#000000"):gsub("^#", "")

	if #hex == 3 then
		hex = hex:gsub(".", "%1%1")
	end

	if #hex == 8 then
		hex = hex:sub(1, 6)
	end

	if #hex ~= 6 then
		return 0, 0, 0
	end

	local r = tonumber(hex:sub(1, 2), 16) or 0
	local g = tonumber(hex:sub(3, 4), 16) or 0
	local b = tonumber(hex:sub(5, 6), 16) or 0

	return r, g, b
end

function M.hex_to_rgb01(hex)
	local r, g, b = M.hex_to_rgb(hex)
	return r / 255, g / 255, b / 255
end

function M.rgb_to_hex(r, g, b)
	return string.format(
		"#%02X%02X%02X",
		clamp(math.floor((r or 0) + 0.5), 0, 255),
		clamp(math.floor((g or 0) + 0.5), 0, 255),
		clamp(math.floor((b or 0) + 0.5), 0, 255)
	)
end

function M.adjust_color(hex, pct)
	local r, g, b = M.hex_to_rgb(hex)
	local factor = 1 + (pct or 0) / 100
	return M.rgb_to_hex(r * factor, g * factor, b * factor)
end

function M.export_globals(enable)
	if not enable then
		return
	end

	for k, v in pairs(M.get()) do
		_G[k] = v
	end
end

return M
