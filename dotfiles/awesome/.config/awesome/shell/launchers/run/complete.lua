-- ~/.config/awesome/shell/launchers/run/complete.lua
local C = {}

local CACHE = {
	apps = false,
	home = false,
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function split_path(p)
	local out = {}

	for seg in (p or ""):gmatch("([^:]+)") do
		out[#out + 1] = seg
	end

	return out
end

local function read_lines(cmd)
	local out = {}
	local f = io.popen(cmd)

	if not f then
		return out
	end

	for line in f:lines() do
		if line and #line > 0 then
			out[#out + 1] = line
		end
	end

	f:close()
	return out
end

local function unique_sorted(list)
	local out = {}
	local seen = {}

	for _, item in ipairs(list or {}) do
		if item and #item > 0 and not seen[item] then
			out[#out + 1] = item
			seen[item] = true
		end
	end

	table.sort(out)
	return out
end

local function fuzzy_score(needle, hay)
	needle = (needle or ""):lower()
	hay = (hay or ""):lower()

	if needle == "" or hay == "" then
		return -math.huge
	end

	local n = 1
	local h = 1
	local score = 0
	local streak = 0

	while n <= #needle and h <= #hay do
		local cn = needle:sub(n, n)
		local ch = hay:sub(h, h)

		if cn == ch then
			local bonus = 5
			local prev = (h == 1) and "/" or hay:sub(h - 1, h - 1)

			if prev == "/" or prev == "-" or prev == "_" or prev == " " or prev == "." then
				bonus = bonus + 4
			end

			streak = streak + 1
			score = score + bonus + math.min(streak, 3)
			n = n + 1
		else
			streak = 0
		end

		h = h + 1
	end

	if n <= #needle then
		return -math.huge
	end

	return score + math.max(0, 6 - math.abs(#hay - #needle))
end

-- =========================================================================
-- Sources
-- =========================================================================

local function list_path_bins()
	local out = {}
	local seen = {}

	for _, dir in ipairs(split_path(os.getenv("PATH") or "")) do
		local cmd = string.format("ls -1 %q 2>/dev/null", dir)

		for _, name in ipairs(read_lines(cmd)) do
			if not seen[name] then
				out[#out + 1] = name
				seen[name] = true
			end
		end
	end

	table.sort(out)
	return out
end

local function list_desktop_names()
	local home = os.getenv("HOME") or ""
	local xdg_data_home = os.getenv("XDG_DATA_HOME") or (home ~= "" and (home .. "/.local/share") or "")

	local paths = {
		xdg_data_home ~= "" and (xdg_data_home .. "/applications") or nil,
		"/usr/share/applications",
		"/usr/local/share/applications",
		home ~= "" and (home .. "/.local/share/flatpak/exports/share/applications") or nil,
		"/var/lib/flatpak/exports/share/applications",
		"/var/lib/snapd/desktop/applications",
	}

	local quoted = {}

	for _, p in ipairs(paths) do
		if p and #p > 0 then
			quoted[#quoted + 1] = string.format("%q", p)
		end
	end

	if #quoted == 0 then
		return {}
	end

	local cmd = "find "
		.. table.concat(quoted, " ")
		.. " -type f -name '*.desktop' 2>/dev/null | sed 's#.*/##;s#\\.desktop$##' | sort -u"

	return read_lines(cmd)
end

local function list_home_entries()
	local home = os.getenv("HOME")
	if not home or home == "" then
		return {}
	end

	local out = {}

	for _, name in ipairs(read_lines(string.format("cd %q && ls -a 2>/dev/null", home))) do
		if name ~= "." and name ~= ".." then
			out[#out + 1] = name
		end
	end

	table.sort(out)
	return out
end

-- =========================================================================
-- Cache
-- =========================================================================

function C.ensure()
	if not CACHE.apps then
		CACHE.apps = unique_sorted({
			table.unpack(list_desktop_names()),
			table.unpack(list_path_bins()),
		})
	end

	if not CACHE.home then
		CACHE.home = list_home_entries()
	end
end

function C.refresh()
	CACHE = {
		apps = false,
		home = false,
	}

	C.ensure()
end

-- =========================================================================
-- Public API
-- =========================================================================

function C.candidates(mode)
	C.ensure()

	if mode == "local" then
		return CACHE.home or {}
	end

	if mode == "web" then
		return {}
	end

	return CACHE.apps or {}
end

function C.best(list, q)
	if not list or #list == 0 then
		return nil
	end

	if not q or q == "" then
		return list[1]
	end

	local pick = nil
	local best_score = -math.huge

	for _, item in ipairs(list) do
		local score = fuzzy_score(q, item)

		if score > best_score then
			pick = item
			best_score = score
		end
	end

	return pick
end

function C.top(list, q, n)
	n = n or 5

	if not list or #list == 0 then
		return {}
	end

	if not q or q == "" then
		local out = {}

		for i = 1, math.min(n, #list) do
			out[i] = list[i]
		end

		return out
	end

	local scored = {}

	for _, item in ipairs(list) do
		local score = fuzzy_score(q, item)

		if score > -math.huge then
			scored[#scored + 1] = {
				item = item,
				score = score,
			}
		end
	end

	table.sort(scored, function(a, b)
		if a.score ~= b.score then
			return a.score > b.score
		end
		return a.item < b.item
	end)

	local out = {}

	for i = 1, math.min(n, #scored) do
		out[i] = scored[i].item
	end

	return out
end

return C
