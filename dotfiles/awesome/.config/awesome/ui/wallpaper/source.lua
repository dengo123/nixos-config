-- ~/.config/awesome/ui/wallpaper/source.lua
local gfs = require("gears.filesystem")

local M = {}

local runtime_cfg = {}

local state_by_screen = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function expand_source_path(source)
	source = tostring(source or "")

	if source == "" then
		return nil
	end

	if source:match("^~/") then
		local home = os.getenv("HOME") or ""
		return home .. source:sub(2)
	end

	if source:match("^/") then
		return source
	end

	return gfs.get_configuration_dir() .. source
end

local function file_exists(path)
	return type(path) == "string" and gfs.file_readable(path)
end

local function is_directory(path)
	return type(path) == "string" and gfs.dir_readable(path)
end

local function is_image_file(name)
	if type(name) ~= "string" then
		return false
	end

	local lower = name:lower()

	return lower:match("%.png$") ~= nil
		or lower:match("%.jpg$") ~= nil
		or lower:match("%.jpeg$") ~= nil
		or lower:match("%.webp$") ~= nil
		or lower:match("%.bmp$") ~= nil
end

local function sorted_image_files(dir)
	local out = {}

	if not is_directory(dir) then
		return out
	end

	for name in io.popen("find " .. string.format("%q", dir) .. " -maxdepth 1 -type f 2>/dev/null"):lines() do
		if is_image_file(name) then
			table.insert(out, name)
		end
	end

	table.sort(out)
	return out
end

local function screen_key(s)
	return tostring((s and s.index) or 0)
end

local function screen_state(s)
	local key = screen_key(s)

	state_by_screen[key] = state_by_screen[key] or {
		source_key = nil,
		files = nil,
		index = 1,
	}

	return state_by_screen[key]
end

local function source_key_for_spec(spec)
	local rotation = spec.rotation or {}

	return table.concat({
		tostring(spec.source or ""),
		tostring(rotation.enabled == true),
		tostring(rotation.interval or ""),
		tostring(rotation.random == true),
	}, "::")
end

local function ensure_state_for_spec(s, spec)
	local st = screen_state(s)
	local source = expand_source_path(spec.source)
	local key = source_key_for_spec({
		source = source,
		rotation = spec.rotation,
	})

	if st.source_key == key and st.files ~= nil then
		return st, source
	end

	st.source_key = key
	st.index = 1
	st.files = nil

	if is_directory(source) then
		st.files = sorted_image_files(source)
	end

	return st, source
end

local function current_file_from_state(st, fallback)
	if type(st.files) == "table" and #st.files > 0 then
		local idx = tonumber(st.index) or 1
		if idx < 1 then
			idx = 1
		end
		if idx > #st.files then
			idx = #st.files
		end

		st.index = idx
		return st.files[idx]
	end

	return fallback
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.set_runtime_cfg(cfg)
	runtime_cfg = cfg or {}
end

function M.resolve_for_screen(s, spec)
	spec = spec or {}

	local st, source = ensure_state_for_spec(s, spec)
	local resolved = current_file_from_state(st, source)

	if type(resolved) == "string" and resolved ~= "" then
		return resolved
	end

	return spec.source
end

function M.advance_for_screen(s, spec)
	spec = spec or {}

	local st, source = ensure_state_for_spec(s, spec)
	local rotation = spec.rotation or {}

	if type(st.files) ~= "table" or #st.files == 0 then
		return source or spec.source
	end

	if rotation.random == true then
		if #st.files == 1 then
			st.index = 1
		else
			local current = tonumber(st.index) or 1
			local next_idx = current

			while next_idx == current do
				next_idx = math.random(1, #st.files)
			end

			st.index = next_idx
		end
	else
		st.index = ((tonumber(st.index) or 1) % #st.files) + 1
	end

	return current_file_from_state(st, source) or spec.source
end

function M.reset_for_screen(s)
	local key = screen_key(s)
	state_by_screen[key] = nil
end

function M.reset_all()
	state_by_screen = {}
end

return M
