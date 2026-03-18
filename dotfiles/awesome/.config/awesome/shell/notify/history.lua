-- ~/.config/awesome/shell/notify/history.lua
local gears = require("gears")

local M = {}

-- =========================================================================
-- State
-- =========================================================================

local entries = {}
local unread_count = 0
local max_entries = 100

-- =========================================================================
-- Internal
-- =========================================================================

local function shallow_copy(t)
	local out = {}

	for k, v in pairs(t or {}) do
		out[k] = v
	end

	return out
end

local function emit_changed()
	awesome.emit_signal("notify::history_changed")
	awesome.emit_signal("notify::unread_count", unread_count)
end

local function clamp_history()
	while #entries > max_entries do
		local removed = table.remove(entries, 1)

		if removed and removed.read ~= true then
			unread_count = math.max(0, unread_count - 1)
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

	for _, action in ipairs(notification.actions or {}) do
		table.insert(out, {
			label = normalize_text(action.name or action.label or "Action"),
			object = action,
		})
	end

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

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(opts)
	opts = opts or {}

	if tonumber(opts.max_entries) then
		max_entries = math.max(1, math.floor(tonumber(opts.max_entries)))
	end

	clamp_history()
	emit_changed()
end

function M.add(notification)
	local entry = normalize_entry(notification)

	table.insert(entries, entry)
	unread_count = unread_count + 1

	clamp_history()
	emit_changed()

	return entry
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

function M.get_unread_count()
	return unread_count
end

function M.get_count()
	return #entries
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

function M.mark_read_reverse(index)
	local count = #entries
	local raw_index = count - index + 1
	local entry = entries[raw_index]

	if not entry or entry.read == true then
		return
	end

	entry.read = true
	unread_count = math.max(0, unread_count - 1)
	emit_changed()
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
