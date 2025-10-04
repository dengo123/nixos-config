-- ~/.config/awesome/shell/launchers/run/complete.lua
-- Kandidaten + Fuzzy + Caches (apps/home/zoxide)
local C = {}

-- ---------------- internal helpers ----------------
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

local function list_home_entries()
	local home = os.getenv("HOME") or "~"
	local out, f = {}, io.popen(string.format("cd %q && ls -a 2>/dev/null", home))
	if f then
		for name in f:lines() do
			out[#out + 1] = name
		end
		f:close()
	end
	table.sort(out)
	return out
end

local function zoxide_list_all()
	local out, f = {}, io.popen("zoxide query -l 2>/dev/null")
	if f then
		for line in f:lines() do
			if line and #line > 0 then
				out[#out + 1] = line
			end
		end
		f:close()
	end
	return out
end

-- fzy-ish score
local function fuzzy_score(needle, hay)
	needle = (needle or ""):lower()
	hay = (hay or ""):lower()
	if needle == "" or hay == "" then
		return -math.huge
	end
	local n, h, score, streak = 1, 1, 0, 0
	while n <= #needle and h <= #hay do
		local cn, ch = needle:sub(n, n), hay:sub(h, h)
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

-- ---------------- public API ----------------
local CACHE = { apps = false, home = false, zox = false }

function C.ensure()
	if not CACHE.apps then
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
	if not CACHE.home then
		CACHE.home = list_home_entries()
	end
	if not CACHE.zox then
		CACHE.zox = zoxide_list_all()
	end
end

function C.candidates(mode)
	C.ensure()
	if mode == "local" then
		local seen, union = {}, {}
		for _, v in ipairs(CACHE.zox or {}) do
			if not seen[v] then
				union[#union + 1] = v
				seen[v] = true
			end
		end
		for _, v in ipairs(CACHE.home or {}) do
			if not seen[v] then
				union[#union + 1] = v
				seen[v] = true
			end
		end
		return union
	elseif mode == "web" then
		return {} -- optional: query history
	else
		return CACHE.apps or {}
	end
end

function C.best(list, q)
	if not list or #list == 0 then
		return nil
	end
	if not q or q == "" then
		return list[1]
	end
	local pick, bs = nil, -math.huge
	for _, item in ipairs(list) do
		local s = fuzzy_score(q, item)
		if s > bs then
			pick, bs = item, s
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
		local t = {}
		for i = 1, math.min(n, #list) do
			t[i] = list[i]
		end
		return t
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
	for i = 1, math.min(n, #scored) do
		out[i] = scored[i].item
	end
	return out
end

function C.refresh()
	CACHE = { apps = false, home = false, zox = false }
	C.ensure()
end

return C
