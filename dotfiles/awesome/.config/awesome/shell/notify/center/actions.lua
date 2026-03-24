local M = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function history_module()
	return require("shell.notify.history")
end

local function should_close_center(cfg)
	return cfg and cfg.notify and cfg.notify.actions and cfg.notify.actions.invoke_closes_center == true
end

local function emit_history_changed()
	awesome.emit_signal("notify::history_changed")
end

local function maybe_close_center(cfg)
	if should_close_center(cfg) then
		awesome.emit_signal("notify::close_center")
	end
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.remove(entry, cfg)
	local History = history_module()

	if type(History.remove) == "function" then
		History.remove(entry)
	elseif type(History.delete) == "function" then
		History.delete(entry)
	elseif type(History.dismiss) == "function" then
		History.dismiss(entry)
	elseif entry and entry.raw and type(entry.raw.destroy) == "function" then
		pcall(function()
			entry.raw:destroy()
		end)
	end

	emit_history_changed()
	maybe_close_center(cfg)
end

function M.invoke(entry, item, cfg)
	if type(item) ~= "table" then
		return
	end

	if type(item.callback) == "function" then
		item.callback(entry and entry.raw or nil)
	elseif type(item.invoke) == "function" then
		item.invoke(entry and entry.raw or nil)
	end

	maybe_close_center(cfg)
end

function M.open(entry, cfg)
	if type(entry) == "table" and type(entry.actions) == "table" and #entry.actions > 0 then
		M.invoke(entry, entry.actions[1], cfg)
	else
		M.remove(entry, cfg)
	end
end

function M.dismiss(entry, cfg)
	M.remove(entry, cfg)
end

return M
