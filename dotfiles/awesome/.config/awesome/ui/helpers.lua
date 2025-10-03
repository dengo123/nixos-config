-- ui/helpers.lua
local xr = require("beautiful.xresources")
local dpi = xr.apply_dpi
local H = {}

function H.merge(a, b)
	local o = {}
	for k, v in pairs(a or {}) do
		o[k] = v
	end
	for k, v in pairs(b or {}) do
		o[k] = v
	end
	return o
end

function H.deepcopy(t)
	if type(t) ~= "table" then
		return t
	end
	local r = {}
	for k, v in pairs(t) do
		r[H.deepcopy(k)] = H.deepcopy(v)
	end
	return r
end

function H.clamp(x, a, b)
	return math.max(a, math.min(b, x))
end

function H.dpi(x)
	return dpi(x)
end

-- hex/rgb/adjust
local function hex_to_rgb(hex)
	hex = hex or "#000000"
	local r, g, b = hex:match("#?(%x%x)(%x%x)(%x%x)")
	return tonumber(r or "00", 16), tonumber(g or "00", 16), tonumber(b or "00", 16)
end
local function rgb_to_hex(r, g, b)
	return string.format(
		"#%02X%02X%02X",
		H.clamp(math.floor((r or 0) + 0.5), 0, 255),
		H.clamp(math.floor((g or 0) + 0.5), 0, 255),
		H.clamp(math.floor((b or 0) + 0.5), 0, 255)
	)
end
function H.adjust_color(hex, pct)
	local r, g, b = hex_to_rgb(hex)
	local f = 1 + (pct or 0) / 100
	return rgb_to_hex(r * f, g * f, b * f)
end

-- Tiefes Sperren von Tabellen (damit Felder nicht später überschrieben werden)
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
				elseif naughty_ok and mode == "warn" then
					naughty.notify({ title = "Theme table lock", text = msg })
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

-- Bequemer: alle beautiful-Keys mit bestimmten Präfixen sperren
function H.lock_beautiful_by_prefix(prefixes, mode)
	local beautiful = require("beautiful")
	local keys = {}
	for k, _ in pairs(beautiful) do
		if type(k) == "string" then
			for _, p in ipairs(prefixes or {}) do
				if k:sub(1, #p) == p then
					table.insert(keys, k)
					break
				end
			end
		end
	end
	H.lock_beautiful_keys(keys, mode or "error")
end

-- lock_beautiful_keys (generisch)
function H.lock_beautiful_keys(keys, mode)
	local beautiful = require("beautiful")
	local naughty_ok, naughty = pcall(require, "naughty")
	mode = mode or "error"
	local mt = getmetatable(beautiful) or {}
	if not getmetatable(beautiful) then
		debug.setmetatable(beautiful, mt)
	end
	local INIT, LOCK = {}, {}
	for _, k in ipairs(keys or {}) do
		INIT[k] = rawget(beautiful, k)
		LOCK[k] = true
	end
	local prev_newindex = mt.__newindex
	mt.__newindex = function(t, key, val)
		if LOCK[key] and val ~= INIT[key] then
			local msg = ("Attempt to modify locked theme key: beautiful.%s"):format(key)
			if mode == "error" then
				error(msg, 2)
			elseif mode == "warn" and naughty_ok then
				naughty.notify({ title = "Theme lock", text = msg })
			end
			return
		end
		return prev_newindex and prev_newindex(t, key, val) or rawset(t, key, val)
	end
end

return H
