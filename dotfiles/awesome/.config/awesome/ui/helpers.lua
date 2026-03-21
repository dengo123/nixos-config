-- ~/.config/awesome/ui/helpers.lua
local xr = require("beautiful.xresources")
local dpi = xr.apply_dpi

local H = {}

-- =========================================================================
-- Tables
-- =========================================================================

function H.merge(a, b)
	local out = {}

	for k, v in pairs(a or {}) do
		out[k] = v
	end

	for k, v in pairs(b or {}) do
		out[k] = v
	end

	return out
end

function H.deepcopy(value)
	if type(value) ~= "table" then
		return value
	end

	local out = {}

	for k, v in pairs(value) do
		out[H.deepcopy(k)] = H.deepcopy(v)
	end

	return out
end

function H.with_colors(ui, value)
	local colors = (ui or {}).colors or {}
	return H.merge(colors, value or {})
end

-- =========================================================================
-- Numbers
-- =========================================================================

function H.clamp(x, a, b)
	return math.max(a, math.min(b, x))
end

function H.dpi(x)
	return dpi(x)
end

-- =========================================================================
-- Colors
-- =========================================================================

function H.hex_to_rgb(hex)
	hex = tostring(hex or "#000000")
	local r, g, b = hex:match("#?(%x%x)(%x%x)(%x%x)")

	return tonumber(r or "00", 16), tonumber(g or "00", 16), tonumber(b or "00", 16)
end

function H.hex_to_rgb01(hex)
	local r, g, b = H.hex_to_rgb(hex)
	return r / 255, g / 255, b / 255
end

function H.rgb_to_hex(r, g, b)
	return string.format(
		"#%02X%02X%02X",
		H.clamp(math.floor((r or 0) + 0.5), 0, 255),
		H.clamp(math.floor((g or 0) + 0.5), 0, 255),
		H.clamp(math.floor((b or 0) + 0.5), 0, 255)
	)
end

function H.adjust_color(hex, pct)
	local r, g, b = H.hex_to_rgb(hex)
	local factor = 1 + (pct or 0) / 100
	return H.rgb_to_hex(r * factor, g * factor, b * factor)
end

-- =========================================================================
-- Locks
-- =========================================================================

function H.freeze_table(tbl, mode)
	mode = mode or "error"

	local naughty_ok, naughty = pcall(require, "naughty")

	local function freeze(t)
		for k, v in pairs(t) do
			if type(v) == "table" then
				t[k] = freeze(v)
			end
		end

		return setmetatable({}, {
			__index = t,
			__newindex = function(_, k, v)
				local msg = ("Attempt to modify locked table field: %s = %s"):format(tostring(k), tostring(v))

				if mode == "error" then
					error(msg, 2)
				elseif mode == "warn" and naughty_ok then
					naughty.notify({
						title = "Theme table lock",
						text = msg,
					})
				end
			end,
			__pairs = function()
				return pairs(t)
			end,
			__ipairs = function()
				return ipairs(t)
			end,
			__len = function()
				return #t
			end,
		})
	end

	if type(tbl) ~= "table" then
		return tbl
	end

	return freeze(tbl)
end

function H.lock_beautiful_keys(keys, mode)
	local beautiful = require("beautiful")
	local naughty_ok, naughty = pcall(require, "naughty")

	mode = mode or "error"

	local mt = getmetatable(beautiful) or {}
	if not getmetatable(beautiful) then
		debug.setmetatable(beautiful, mt)
	end

	local initial = {}
	local locked = {}

	for _, key in ipairs(keys or {}) do
		initial[key] = rawget(beautiful, key)
		locked[key] = true
	end

	local prev_newindex = mt.__newindex

	mt.__newindex = function(t, key, val)
		if locked[key] and val ~= initial[key] then
			local msg = ("Attempt to modify locked theme key: beautiful.%s"):format(key)

			if mode == "error" then
				error(msg, 2)
			elseif mode == "warn" and naughty_ok then
				naughty.notify({
					title = "Theme lock",
					text = msg,
				})
			end

			return
		end

		return prev_newindex and prev_newindex(t, key, val) or rawset(t, key, val)
	end
end

function H.lock_beautiful_by_prefix(prefixes, mode)
	local beautiful = require("beautiful")
	local keys = {}

	for k, _ in pairs(beautiful) do
		if type(k) == "string" then
			for _, prefix in ipairs(prefixes or {}) do
				if k:sub(1, #prefix) == prefix then
					table.insert(keys, k)
					break
				end
			end
		end
	end

	H.lock_beautiful_keys(keys, mode or "error")
end

return H
