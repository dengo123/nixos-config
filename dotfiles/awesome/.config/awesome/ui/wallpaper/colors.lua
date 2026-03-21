-- ~/.config/awesome/ui/wallpaper/colors.lua
local beautiful = require("beautiful")
local awful = require("awful")

local M = {}

local cache = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function fallback_color()
	return tostring(beautiful.wallpaper_bg or beautiful.bg_normal or "#000000")
end

local function cache_key(path, style)
	return tostring(path or "") .. "::" .. tostring(style or "solid")
end

local function sanitize_hex(value)
	value = tostring(value or ""):match("^%s*(.-)%s*$")
	value = value:gsub("^#", ""):upper()

	if value:match("^%x%x%x%x%x%x%x%x$") then
		value = value:sub(1, 6)
	end

	if value:match("^%x%x%x%x%x%x$") then
		return "#" .. value
	end

	return nil
end

local function pick_tool()
	local magick = os.execute("command -v magick >/dev/null 2>&1")
	if magick == true or magick == 0 then
		return "magick"
	end

	local convert = os.execute("command -v convert >/dev/null 2>&1")
	if convert == true or convert == 0 then
		return "convert"
	end

	return nil
end

local function shell_quote(s)
	s = tostring(s or "")
	return "'" .. s:gsub("'", "'\\''") .. "'"
end

local function parse_edge_output(out)
	out = tostring(out or ""):match("^%s*(.-)%s*$")
	local top, bottom = out:match("([#%x]+)%s+([#%x]+)")
	return sanitize_hex(top), sanitize_hex(bottom)
end

local function average_hex(a, b)
	if not a then
		return sanitize_hex(b)
	end
	if not b then
		return sanitize_hex(a)
	end

	local ar, ag, ab = tonumber(a:sub(2, 3), 16), tonumber(a:sub(4, 5), 16), tonumber(a:sub(6, 7), 16)
	local br, bg, bb = tonumber(b:sub(2, 3), 16), tonumber(b:sub(4, 5), 16), tonumber(b:sub(6, 7), 16)

	local function avg(x, y)
		return math.floor((x + y) / 2 + 0.5)
	end

	return string.format("#%02X%02X%02X", avg(ar, br), avg(ag, bg), avg(ab, bb))
end

local function edge_command(path)
	local tool = pick_tool()
	if not tool then
		return nil
	end

	local p = shell_quote(path)

	if tool == "magick" then
		return "sh -c "
			.. shell_quote(
				"TOP=$(magick "
					.. p
					.. " -gravity north -crop 100%x1+0+0 +repage -scale 1x1 -format '%[hex:p{0,0}]' info:) && "
					.. "BOTTOM=$(magick "
					.. p
					.. " -gravity south -crop 100%x1+0+0 +repage -scale 1x1 -format '%[hex:p{0,0}]' info:) && "
					.. 'printf \'#%s #%s\' "$TOP" "$BOTTOM"'
			)
	end

	return "sh -c "
		.. shell_quote(
			"TOP=$(convert "
				.. p
				.. " -gravity north -crop 100%x1+0+0 +repage -scale 1x1 -format '%[hex:p{0,0}]' info:) && "
				.. "BOTTOM=$(convert "
				.. p
				.. " -gravity south -crop 100%x1+0+0 +repage -scale 1x1 -format '%[hex:p{0,0}]' info:) && "
				.. 'printf \'#%s #%s\' "$TOP" "$BOTTOM"'
		)
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.resolve_sync(path, style)
	style = tostring(style or "solid"):lower()

	local key = cache_key(path, style)
	if cache[key] then
		return cache[key]
	end

	local cmd = edge_command(path)
	if not cmd then
		local fallback = {
			style = style,
			solid = fallback_color(),
			top = fallback_color(),
			bottom = fallback_color(),
		}
		cache[key] = fallback
		return fallback
	end

	local handle = io.popen(cmd)
	if not handle then
		local fallback = {
			style = style,
			solid = fallback_color(),
			top = fallback_color(),
			bottom = fallback_color(),
		}
		cache[key] = fallback
		return fallback
	end

	local out = handle:read("*a") or ""
	handle:close()

	local top, bottom = parse_edge_output(out)
	top = top or fallback_color()
	bottom = bottom or fallback_color()

	local resolved = {
		style = style,
		solid = average_hex(top, bottom),
		top = top,
		bottom = bottom,
	}

	cache[key] = resolved
	return resolved
end

function M.resolve_async(path, style, cb)
	local key = cache_key(path, style)
	if cache[key] then
		if type(cb) == "function" then
			cb(cache[key])
		end
		return
	end

	local cmd = edge_command(path)
	if not cmd then
		local fallback = {
			style = tostring(style or "solid"):lower(),
			solid = fallback_color(),
			top = fallback_color(),
			bottom = fallback_color(),
		}
		cache[key] = fallback
		if type(cb) == "function" then
			cb(fallback)
		end
		return
	end

	awful.spawn.easy_async_with_shell(cmd, function(out)
		local top, bottom = parse_edge_output(out or "")
		top = top or fallback_color()
		bottom = bottom or fallback_color()

		local resolved = {
			style = tostring(style or "solid"):lower(),
			solid = average_hex(top, bottom),
			top = top,
			bottom = bottom,
		}

		cache[key] = resolved

		if type(cb) == "function" then
			cb(resolved)
		end
	end)
end

function M.invalidate(path)
	if not path then
		cache = {}
		return
	end

	for key, _ in pairs(cache) do
		if key:match("^" .. path:gsub("([^%w])", "%%%1")) then
			cache[key] = nil
		end
	end
end

return M
