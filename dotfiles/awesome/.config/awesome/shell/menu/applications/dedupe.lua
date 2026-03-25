-- ~/.config/awesome/shell/menu/applications/dedupe.lua
local M = {}

local runtime = {
	api = {},
	cfg = {},
	ui = {},
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function api()
	return runtime.api or {}
end

local function mod(name)
	return api()[name]
end

local function normalize_exec(cmd)
	if type(cmd) ~= "string" then
		return nil
	end

	cmd = cmd:gsub("%%[%a%%]", "")
	cmd = cmd:gsub("%s+$", "")

	if cmd == "" then
		return nil
	end

	return cmd
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

local function dedupe_key_for_entry(entry)
	local Overrides = mod("overrides")

	if Overrides and type(Overrides.dedupe_key) == "function" then
		local forced = Overrides.dedupe_key(entry)
		if type(forced) == "string" and forced ~= "" then
			return forced
		end
	end

	local label = first_string(
		entry and entry.Name,
		entry and entry.name,
		entry and entry.label,
		entry and entry.text,
		entry and entry[1]
	)

	local exec = normalize_exec(
		first_string(
			entry and entry.cmdline,
			entry and entry.cmd,
			entry and entry.Exec,
			entry and entry.exec,
			entry and entry.command,
			type(entry and entry[2]) == "string" and entry[2] or nil
		)
	)

	if exec and exec ~= "" then
		return "exec:" .. exec:lower()
	end

	if label and label ~= "" then
		return "label:" .. label:lower()
	end

	return nil
end

local function entry_label(entry)
	return first_string(
		entry and entry.Name,
		entry and entry.name,
		entry and entry.label,
		entry and entry.text,
		entry and entry[1]
	) or ""
end

local function entry_icon(entry)
	return first_string(entry and entry.icon_path, entry and entry.icon, entry and entry.Icon, entry and entry[3])
end

local function entry_score(entry)
	local score = 0
	local label = entry_label(entry)
	local icon = entry_icon(entry)

	if type(icon) == "string" and icon ~= "" then
		score = score + 1000
	end

	score = score + math.min(#label, 200)

	local lower = label:lower()
	if lower == "emacs" or lower == "textmaker" or lower == "planmaker" or lower == "presentations" then
		score = score - 10
	end

	return score
end

local function better_entry(a, b)
	if not a then
		return b
	end

	if not b then
		return a
	end

	if entry_score(b) > entry_score(a) then
		return b
	end

	return a
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	args = args or {}

	runtime.api = args.api or {}
	runtime.cfg = args.cfg or {}
	runtime.ui = args.ui or {}

	return M
end

function M.run(entries)
	local passthrough = {}
	local grouped = {}

	for _, entry in ipairs(entries or {}) do
		local key = dedupe_key_for_entry(entry)

		if not key then
			table.insert(passthrough, entry)
		else
			grouped[key] = better_entry(grouped[key], entry)
		end
	end

	local out = {}

	for _, entry in ipairs(passthrough) do
		table.insert(out, entry)
	end

	for _, entry in pairs(grouped) do
		table.insert(out, entry)
	end

	return out
end

return M
