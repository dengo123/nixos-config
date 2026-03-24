-- ~/.config/awesome/ui/lib/colors.lua
local M = {}

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

local function shallow_copy(src)
	local out = {}

	for k, v in pairs(src or {}) do
		out[k] = v
	end

	return out
end

-- =========================================================================
-- Theme Materialization
-- =========================================================================

function M.build(theme)
	theme = theme or {}

	local palette = theme.palette or {}
	local roles = theme.roles or {}

	local colors = shallow_copy(palette)

	for role, ref in pairs(roles) do
		if type(ref) == "string" and palette[ref] ~= nil then
			colors[role] = palette[ref]
		else
			colors[role] = ref
		end
	end

	local utils = shallow_copy(DERIVED)

	for k, v in pairs(utils) do
		if colors[k] == nil then
			colors[k] = v
		end
	end

	return {
		colors = colors,
		utils = utils,
	}
end

function M.resolve(color_map, value)
	local colors = color_map or {}

	if type(value) == "string" and colors[value] ~= nil then
		return colors[value]
	end

	return value
end

-- =========================================================================
-- Color Conversion
-- =========================================================================

function M.hex_to_rgb(value)
	local hex = tostring(value or "#000000"):gsub("^#", "")

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

function M.hex_to_rgb01(value)
	local r, g, b = M.hex_to_rgb(value)
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

function M.adjust_color(value, pct)
	local r, g, b = M.hex_to_rgb(value)
	local factor = 1 + (pct or 0) / 100

	return M.rgb_to_hex(r * factor, g * factor, b * factor)
end

return M
