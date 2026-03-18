-- ~/.config/awesome/shell/notify/history.lua
local M = {}

local entries = {}
local unread_count = 0
local max_entries = 100
local next_id = 1

-- =========================================================================
-- Helpers
-- =========================================================================

local function shallow_copy(t)
	local out = {}

	for k, v in pairs(t or {}) do
		out[k] = v
	end

	return out
end

local function find_index_by_id(id)
	for i, entry in ipairs(entries) do
		if entry.id == id then
			return i
		end
	end

	return nil
end

local function clamp_history()
	while #entries > max_entries do
		local removed = table.remove(entries, 1)

		if removed and removed.read ~= true then
			unread_count = math.max(0, unread_count - 1)
		end
	end
end

local function emit_changed()
	awesome.emit_signal("notify::history_changed")
	awesome.emit_signal("notify::unread_count", unread_count)
end

local function normalize_entry(notification)
	local app_name = notification.app_name or notification.appname or notification.app or nil
	local title = notification.title or ""
	local message = notification.message or notification.text or ""
	local icon = notification.icon or notification.image or nil
	local urgency = notification.urgency or "normal"

	local entry = {
		id = next_id,
		app_name = app_name,
		title = title,
		message = message,
		icon = icon,
		urgency = urgency,
		timestamp = os.time(),
		read = false,
		raw = notification,
	}

	next_id = next_id + 1

	return entry
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(opts)
	opts = opts or {}

	if tonumber(opts.max_entries) then
		max_entries = math.max(1, math.floor(tonumber(opts.max_entries)))
		clamp_history()
		emit_changed()
	end
end

function M.add(notification)
	local entry = normalize_entry(notification)

	table.insert(entries, entry)
	unread_count = unread_count + 1

	clamp_history()
	emit_changed()

	return shallow_copy(entry)
end

function M.list()
	local out = {}

	for i = #entries, 1, -1 do
		table.insert(out, shallow_copy(entries[i]))
	end

	return out
end

function M.raw_list()
	return entries
end

function M.count()
	return #entries
end

function M.has_unread()
	return unread_count > 0
end

function M.get_unread_count()
	return unread_count
end

function M.get_by_id(id)
	local index = find_index_by_id(id)

	if not index then
		return nil
	end

	return shallow_copy(entries[index])
end

function M.mark_all_read()
	local changed = false

	for _, entry in ipairs(entries) do
		if entry.read ~= true then
			entry.read = true
			changed = true
		end
	end

	if changed then
		unread_count = 0
		emit_changed()
	end
end

function M.mark_read(index)
	local entry = entries[index]
	if not entry or entry.read == true then
		return
	end

	entry.read = true
	unread_count = math.max(0, unread_count - 1)
	emit_changed()
end

function M.mark_read_by_id(id)
	local index = find_index_by_id(id)

	if not index then
		return false
	end

	local entry = entries[index]

	if entry.read == true then
		return true
	end

	entry.read = true
	unread_count = math.max(0, unread_count - 1)
	emit_changed()

	return true
end

function M.remove_by_id(id)
	local index = find_index_by_id(id)

	if not index then
		return false
	end

	local removed = table.remove(entries, index)

	if removed and removed.read ~= true then
		unread_count = math.max(0, unread_count - 1)
	end

	emit_changed()

	return true
end

function M.clear()
	entries = {}
	unread_count = 0
	emit_changed()
end

function M.set_max_entries(value)
	local n = tonumber(value)
	if not n then
		return
	end

	max_entries = math.max(1, math.floor(n))
	clamp_history()
	emit_changed()
end

return M
