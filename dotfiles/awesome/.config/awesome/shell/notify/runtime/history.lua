-- ~/.config/awesome/shell/notify/runtime/history.lua
local gears = require("gears")

local M = {}

local runtime = {
	max_entries = 100,
	entries = {},
	unread_count = 0,
}

-- =========================================================================
-- Internal
-- =========================================================================

local function emit_changed()
	awesome.emit_signal("notify::history_changed")
	awesome.emit_signal("notify::unread_count", runtime.unread_count)
end

local function shallow_copy(t)
	local out = {}

	for k, v in pairs(t or {}) do
		out[k] = v
	end

	return out
end

local function set_unread_count(value)
	runtime.unread_count = math.max(0, tonumber(value) or 0)
end

local function clamp_history()
	while #runtime.entries > runtime.max_entries do
		local removed = table.remove(runtime.entries, 1)

		if removed and removed.read ~= true then
			set_unread_count(runtime.unread_count - 1)
		end
	end
end

local function normalize_text(value)
	if value == nil then
		return ""
	end

	local text = tostring(value)
	text = text:gsub("<br%s*/?>", "\n")
	text = text:gsub("<.->", "")
	text = gears.string.xml_unescape(text)

	return text
end

local function copy_actions(notification)
	local out = {}
	local actions = notification.actions or {}

	for label, callback in pairs(actions) do
		table.insert(out, {
			label = normalize_text(label),
			callback = callback,
		})
	end

	table.sort(out, function(a, b)
		return (a.label or "") < (b.label or "")
	end)

	return out
end

local function normalize_entry(notification)
	local app_name = notification.app_name or notification.appname or notification.app or nil
	local title = normalize_text(notification.title)
	local message = normalize_text(notification.message or notification.text)
	local icon = notification.icon or notification.image or nil
	local urgency = notification.urgency or "normal"

	return {
		app_name = app_name,
		title = title,
		message = message,
		icon = icon,
		urgency = urgency,
		timestamp = os.time(),
		read = false,
		raw = notification,
		actions = copy_actions(notification),
	}
end

local function same_entry(a, b)
	if not a or not b then
		return false
	end

	if a.raw ~= nil and b.raw ~= nil and a.raw == b.raw then
		return true
	end

	return a.timestamp == b.timestamp and a.title == b.title and a.message == b.message and a.app_name == b.app_name
end

local function find_entry_index(target)
	if type(target) ~= "table" then
		return nil
	end

	for i, entry in ipairs(runtime.entries) do
		if same_entry(entry, target) then
			return i
		end
	end

	return nil
end

local function remove_at(index)
	index = tonumber(index)

	if not index or index < 1 or index > #runtime.entries then
		return nil
	end

	local removed = table.remove(runtime.entries, index)

	if removed and removed.read ~= true then
		set_unread_count(runtime.unread_count - 1)
	end

	emit_changed()
	return removed
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(opts)
	opts = opts or {}

	local n = tonumber(opts.max_entries)
	if n then
		runtime.max_entries = math.max(1, math.floor(n))
	end

	clamp_history()
	emit_changed()

	return M
end

function M.add(notification)
	local entry = normalize_entry(notification)

	table.insert(runtime.entries, entry)
	set_unread_count(runtime.unread_count + 1)

	clamp_history()
	emit_changed()

	return entry
end

function M.list()
	local out = {}

	for i = #runtime.entries, 1, -1 do
		table.insert(out, shallow_copy(runtime.entries[i]))
	end

	return out
end

function M.raw_list()
	return runtime.entries
end

function M.get_unread_count()
	return runtime.unread_count
end

function M.get_count()
	return #runtime.entries
end

function M.mark_all_read()
	local changed = false

	for _, entry in ipairs(runtime.entries) do
		if entry.read ~= true then
			entry.read = true
			changed = true
		end
	end

	if changed then
		set_unread_count(0)
		emit_changed()
	end
end

function M.mark_read_reverse(index)
	local count = #runtime.entries
	local raw_index = count - index + 1
	local entry = runtime.entries[raw_index]

	if not entry or entry.read == true then
		return
	end

	entry.read = true
	set_unread_count(runtime.unread_count - 1)
	emit_changed()
end

function M.remove(entry)
	local index = find_entry_index(entry)
	if not index then
		return nil
	end

	return remove_at(index)
end

function M.delete(entry)
	return M.remove(entry)
end

function M.dismiss(entry)
	return M.remove(entry)
end

function M.clear()
	runtime.entries = {}
	set_unread_count(0)
	emit_changed()
end

function M.set_max_entries(value)
	local n = tonumber(value)

	if not n then
		return
	end

	runtime.max_entries = math.max(1, math.floor(n))
	clamp_history()
	emit_changed()
end

return M
