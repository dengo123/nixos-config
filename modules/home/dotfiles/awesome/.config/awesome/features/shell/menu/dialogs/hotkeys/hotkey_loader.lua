-- features/shell/menu/dialogs/hotkeys/hotkey_loader.lua
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

-- Sicherer Getter: unterstützt table UND userdata (__index)
local function getfield(obj, ...)
	for i = 1, select("#", ...) do
		local name = select(i, ...)
		local v
		if type(obj) == "table" then
			v = rawget(obj, name)
		end
		if v == nil then
			local ok, res = pcall(function()
				return obj[name]
			end)
			if ok then
				v = res
			end
		end
		if v ~= nil then
			return v
		end
	end
	return nil
end

-- Rendert "Key" als String, unterstützt key ODER keygroup
local function key_to_string(k)
	local key = getfield(k, "key", "_key")
	if key ~= nil and key ~= "" then
		return tostring(key)
	end
	local keygroup = getfield(k, "keygroup", "_keygroup")
	if keygroup ~= nil and keygroup ~= "" then
		-- einfache, neutrale Darstellung von Keygroups
		return string.format("[%s]", tostring(keygroup))
	end
	return "" -- notfalls leer lassen; Beschreibung zeigt dann trotzdem was
end

local function combo_string(mods, key_str, labels)
	labels = labels or MOD_LABELS_DEFAULT
	local parts = {}
	for _, m in ipairs(sort_mods(deepcopy(mods or {}))) do
		table.insert(parts, labels[m] or m)
	end
	table.insert(parts, key_str or "")
	return table.concat(parts, " + ")
end

local function ingest_keys(keylist, by_group, opts)
	if type(keylist) ~= "table" then
		return
	end

	local labels = opts.labels or MOD_LABELS_DEFAULT
	local include_undesc = opts.include_undesc or false
	local default_group = opts.default_group or "misc"

	-- WICHTIG: pairs statt ipairs (gears.table.join/root.keys sind oft nicht „array-sequentiell“)
	for _, k in pairs(keylist) do
		local mods = getfield(k, "modifiers")
		if type(mods) ~= "table" then
			mods = (mods ~= nil) and { mods } or {}
		end

		local desc = getfield(k, "description", "_description")
		local grp = getfield(k, "group", "_group") or default_group

		if (desc and desc ~= "") or include_undesc then
			local key_str = key_to_string(k) -- unterstützt key/keygroup
			local combo = combo_string(mods, key_str, labels)
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
