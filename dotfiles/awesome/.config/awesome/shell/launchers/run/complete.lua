-- ~/.config/awesome/shell/launchers/run/complete.lua
local gears = require("gears")

local C = {}

-- --- subsequence fuzzy score (leicht fzy-inspiriert) ---
local function fuzzy_score(needle, hay)
	needle = (needle or ""):lower()
	hay = (hay or ""):lower()
	if needle == "" then
		return -math.huge
	end
	local n, h, score, streak = 1, 1, 0, 0
	while n <= #needle and h <= #hay do
		local cn, ch = needle:sub(n, n), hay:sub(h, h)
		if cn == ch then
			local bonus = 5
			local prev = h == 1 and "/" or hay:sub(h - 1, h - 1)
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
	score = score + math.max(0, 6 - math.abs(#hay - #needle))
	return score
end

local function split_path(p)
	local t = {}
	for seg in (p or ""):gmatch("([^:]+)") do
		t[#t + 1] = seg
	end
	return t
end

local function list_path_bins()
	local out, seen = {}, {}
	for _, dir in ipairs(split_path(os.getenv("PATH") or "")) do
		local f = io.popen(string.format("ls -1 %q 2>/dev/null", dir))
		if f then
			for name in f:lines() do
				if name and #name > 0 and not seen[name] then
					out[#out + 1] = name
					seen[name] = true
				end
			end
			f:close()
		end
	end
	table.sort(out)
	return out
end

local function list_desktop_names()
	local paths = {
		(os.getenv("XDG_DATA_HOME") or (os.getenv("HOME") .. "/.local/share")) .. "/applications",
		"/usr/share/applications",
		"/usr/local/share/applications",
		os.getenv("HOME") .. "/.local/share/flatpak/exports/share/applications",
		"/var/lib/flatpak/exports/share/applications",
		"/var/lib/snapd/desktop/applications",
	}
	local quoted = {}
	for _, p in ipairs(paths) do
		quoted[#quoted + 1] = string.format("%q", p)
	end
	local cmd = "find "
		.. table.concat(quoted, " ")
		.. " -type f -name '*.desktop' 2>/dev/null | sed 's#.*/##;s#\\.desktop$##' | sort -u"
	local out, f = {}, io.popen(cmd)
	if f then
		for name in f:lines() do
			out[#out + 1] = name
		end
		f:close()
	end
	return out
end

local CACHE = { apps = false }

function C.ensure_indexes()
	if CACHE.apps then
		return
	end
	local names = list_desktop_names()
	local bins = list_path_bins()
	local seen, union = {}, {}
	for _, v in ipairs(names) do
		if not seen[v] then
			union[#union + 1] = v
			seen[v] = true
		end
	end
	for _, v in ipairs(bins) do
		if not seen[v] then
			union[#union + 1] = v
			seen[v] = true
		end
	end
	table.sort(union)
	CACHE.apps = union
end

local function top(list, q, limit)
	limit = limit or 20
	if not list or #list == 0 then
		return {}
	end
	if not q or q == "" then
		local out = {}
		for i = 1, math.min(limit, #list) do
			out[i] = list[i]
		end
		return out
	end
	local scored = {}
	for _, item in ipairs(list) do
		local s = fuzzy_score(q, item)
		if s > -math.huge then
			scored[#scored + 1] = { item = item, score = s }
		end
	end
	table.sort(scored, function(a, b)
		if a.score ~= b.score then
			return a.score > b.score
		end
		return a.item < b.item
	end)
	local out = {}
	for i = 1, math.min(limit, #scored) do
		out[i] = scored[i].item
	end
	return out
end

function C.complete(mode, q)
	if mode == "local" then
		-- einfache Liste: Einträge im $HOME (non-recursive)
		local home = os.getenv("HOME") or "~"
		local files, f = {}, io.popen(string.format("cd %q && ls -a 2>/dev/null", home))
		if f then
			for name in f:lines() do
				files[#files + 1] = name
			end
			f:close()
		end
		return top(files, q, 20)
	elseif mode == "web" then
		return {} -- optional: Such-History implementieren
	else
		C.ensure_indexes()
		return top(CACHE.apps, q, 20)
	end
end

return C
