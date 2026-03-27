-- ~/.config/awesome/shell/menu/applications/loader.lua
local menu_gen = require("menubar.menu_gen")
local menubar_utils = require("menubar.utils")

local M = {}

local runtime = {
	ctx = {},
	api = {},
	cfg = {},
	ui = {},
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function ctx()
	return runtime.ctx or {}
end

local function first_string(...)
	for i = 1, select("#", ...) do
		local v = select(i, ...)
		if type(v) == "string" and v ~= "" then
			return v
		end
	end

	return nil
end

local function desktop_file_dirs()
	local home = os.getenv("HOME") or ""
	local user = os.getenv("USER") or ""

	return {
		home .. "/.local/share/applications",
		"/run/current-system/sw/share/applications",
		"/etc/profiles/per-user/" .. user .. "/share/applications",
		"/usr/share/applications",
	}
end

local function list_desktop_files()
	local files = {}
	local seen = {}

	for _, dir in ipairs(desktop_file_dirs()) do
		local cmd = string.format('find %q -type f -name "*.desktop" 2>/dev/null', dir)
		local handle = io.popen(cmd)

		if handle then
			for file in handle:lines() do
				if file ~= "" and not seen[file] then
					seen[file] = true
					table.insert(files, file)
				end
			end
			handle:close()
		end
	end

	return files
end

local function desktop_entries_from_files()
	local out = {}

	for _, file in ipairs(list_desktop_files()) do
		local ok, data = pcall(menubar_utils.parse_desktop_file, file)
		if ok and type(data) == "table" then
			table.insert(out, data)
		end
	end

	return out
end

local function build_desktop_index(entries)
	local by_name = {}

	for _, entry in ipairs(entries or {}) do
		local name = first_string(entry.Name, entry.name)
		if name and not by_name[name] then
			by_name[name] = entry
		end
	end

	return by_name
end

local function merge_entry(raw, desktop)
	if not desktop then
		return raw
	end

	local merged = {}

	for k, v in pairs(desktop) do
		merged[k] = v
	end

	for k, v in pairs(raw or {}) do
		merged[k] = v
	end

	return merged
end

local function merge_menu_entries(raw_entries, desktop_index)
	local out = {}

	for _, raw in ipairs(raw_entries or {}) do
		local label = first_string(raw.Name, raw.name, raw.label, raw.text, raw[1])
		local desktop = label and desktop_index[label] or nil
		table.insert(out, merge_entry(raw, desktop))
	end

	return out
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	runtime.ctx = (args and (args.ctx or args)) or {}
	runtime.api = (args and args.api) or {}
	runtime.cfg = (args and args.cfg) or (runtime.ctx.cfg or {})
	runtime.ui = (args and args.ui) or (runtime.ctx.ui or {})

	return M
end

function M.load(callback)
	menu_gen.generate(function(raw_entries)
		local desktop_index = build_desktop_index(desktop_entries_from_files())
		local merged_entries = merge_menu_entries(raw_entries or {}, desktop_index)

		if type(callback) == "function" then
			callback(merged_entries)
		end
	end)
end

return M
