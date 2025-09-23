-- features/shell/menu/hotkeys/hotkey_loader.lua
local M = {}

-- Anzeige-Reihenfolge der Modifier
local MOD_ORDER = { "Mod4", "Mod1", "Control", "Shift" }

-- Standardlabels (überschreibbar)
local MOD_LABELS_DEFAULT = {
	Mod4 = "Win",
	Mod1 = "Alt",
	Control = "Ctrl",
	Shift = "Shift",
}

local function deepcopy(t)
	if type(t) ~= "table" then
		return t
	end
	local o = {}
	for k, v in pairs(t) do
		o[k] = deepcopy(v)
	end
	return o
end

local function sort_mods(mods)
	local idx = {}
	for i, m in ipairs(MOD_ORDER) do
		idx[m] = i
	end
	table.sort(mods, function(a, b)
		return (idx[a] or 99) < (idx[b] or 99)
	end)
	return mods
end

local function combo_string(mods, key, labels)
	labels = labels or MOD_LABELS_DEFAULT
	local parts = {}
	for _, m in ipairs(sort_mods(deepcopy(mods or {}))) do
		table.insert(parts, labels[m] or m)
	end
	table.insert(parts, tostring(key or ""))
	return table.concat(parts, " + ")
end

local function ingest_keys(keylist, by_group, opts)
	if type(keylist) ~= "table" then
		return
	end
	local labels = opts.labels or MOD_LABELS_DEFAULT
	local include_undesc = opts.include_undesc or false
	local default_group = opts.default_group or "misc"

	for _, k in ipairs(keylist) do
		local mods = k.modifiers or {}
		local key = k.key
		local desc = rawget(k, "description")
		local grp = rawget(k, "group") or default_group

		if (desc and desc ~= "") or include_undesc then
			local combo = combo_string(mods, key, labels)
			by_group[grp] = by_group[grp] or {}
			table.insert(by_group[grp], { combo = combo, desc = desc or "" })
		end
	end
end

--- scan(opts) -> { {group=..., items={ {combo,desc}, ...}}, ... } (sortiert)
-- opts:
--   key_sets = { root.keys(), clientkeys, ... }   -- wenn leer: nimmt root.keys()
--   include_undesc = false
--   labels = { Mod4="Win", ... }
--   default_group = "misc"
function M.scan(opts)
	opts = opts or {}
	local by_group = {}

	if not opts.key_sets or #opts.key_sets == 0 then
		local ok, rk = pcall(function()
			return root.keys()
		end)
		if ok and type(rk) == "table" then
			ingest_keys(rk, by_group, opts)
		end
	else
		for _, set in ipairs(opts.key_sets) do
			ingest_keys(set, by_group, opts)
		end
	end

	local out = {}
	for g, list in pairs(by_group) do
		table.insert(out, { group = g, items = list })
	end
	table.sort(out, function(a, b)
		return a.group < b.group
	end)
	for _, G in ipairs(out) do
		table.sort(G.items, function(a, b)
			return (a.combo or "") < (b.combo or "")
		end)
	end
	return out
end

--- load_into_registry(registry, opts) -> data
-- registry erwartet .bulk(group, items) ODER .add(group, item)
function M.load_into_registry(registry, opts)
	local data = M.scan(opts or {})
	if not registry then
		return data
	end
	for _, G in ipairs(data) do
		if type(registry.bulk) == "function" then
			registry.bulk(G.group, G.items)
		elseif type(registry.add) == "function" then
			for _, it in ipairs(G.items) do
				registry.add(G.group, it)
			end
		end
	end
	return data
end

return M
