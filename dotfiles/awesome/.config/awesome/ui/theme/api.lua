-- ui/theme/api.lua
local API = {}

-- ===== Utils =====
local function clamp(x, a, b)
	return math.max(a, math.min(b, x))
end
local function hex_to_rgb(hex)
	hex = hex or "#000000"
	local r, g, b = hex:match("#?(%x%x)(%x%x)(%x%x)")
	return tonumber(r, 16), tonumber(g, 16), tonumber(b, 16)
end
local function rgb_to_hex(r, g, b)
	return string.format(
		"#%02X%02X%02X",
		clamp(math.floor(r + 0.5), 0, 255),
		clamp(math.floor(g + 0.5), 0, 255),
		clamp(math.floor(b + 0.5), 0, 255)
	)
end

function API.adjust(hex, pct)
	local r, g, b = hex_to_rgb(hex)
	local f = 1 + (pct or 0) / 100
	return rgb_to_hex(r * f, g * f, b * f)
end

-- ===== Typo/Icon Resolver (kompatibel zu deinen bisherigen Signaturen) =====
local DEFAULTS = { text_ratio = 0.25, icon_ratio = 0.90 }

function API.resolve_text_size_number(t, eff_h, kind)
	t = t or {}
	if kind == "header" and t.text_size_header then
		return t.text_size_header
	end
	if kind == "rows" and t.text_size_rows then
		return t.text_size_rows
	end
	if kind == "power" and t.text_size_power then
		return t.text_size_power
	end
	if t.text_size then
		return t.text_size
	end

	local ratio = (kind == "header" and t.text_ratio_header)
		or (kind == "rows" and t.text_ratio_rows)
		or (kind == "power" and t.text_ratio_power)
		or t.text_ratio
		or DEFAULTS.text_ratio

	local sz = math.max(1, math.floor((eff_h or 1) * ratio + 0.5))
	if t.text_min then
		sz = math.max(sz, t.text_min)
	end
	if t.text_max then
		sz = math.min(sz, t.text_max)
	end
	return sz
end

function API.resolve_font(t, eff_h, kind)
	local family = (t and t.font_family) or "Sans"
	local size = API.resolve_text_size_number(t, eff_h, kind)
	return family .. " " .. tostring(size)
end

function API.resolve_icon_size(t, eff_h, kind)
	t = t or {}
	if kind == "header" then
		if t.icon_size_header then
			return t.icon_size_header
		end
		if t.avatar_size then
			return t.avatar_size
		end
	elseif kind == "rows" and t.icon_size_rows then
		return t.icon_size_rows
	elseif kind == "power" then
		if t.icon_size_power then
			return t.icon_size_power
		end
		if t.power_icon_size then
			return t.power_icon_size
		end
	end
	if t.icon_size then
		return t.icon_size
	end

	local ratio = (kind == "header" and t.icon_ratio_header)
		or (kind == "rows" and t.icon_ratio_rows)
		or (kind == "power" and t.icon_ratio_power)
		or t.icon_ratio
		or DEFAULTS.icon_ratio

	return math.max(1, math.floor((eff_h or 1) * ratio + 0.5))
end

-- Markiere/ergänze API am Theme
function API.ensure(t)
	t = t or {}
	t.__theme_api = API
	t.__raw_theme = true -- signalisiert Widgets: Theme ist bereits normalisiert
	return t
end

return API
