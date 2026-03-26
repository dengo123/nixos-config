-- ~/.config/awesome/shell/windowing/runtime/minimized.lua
local M = {}

local runtime = {
	api = {},
	stacks = setmetatable({}, { __mode = "k" }),
	signals_ready = false,
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function ensure_stack(s)
	if not runtime.stacks[s] then
		runtime.stacks[s] = {}
	end

	return runtime.stacks[s]
end

local function selected_tag_clients(s, include_minimized)
	local t = s and s.selected_tag or nil
	if not t then
		return {}
	end

	local out = {}

	for _, c in ipairs(t:clients() or {}) do
		if c.valid and not c.skip_taskbar and c.screen == s then
			if include_minimized or not c.minimized then
				table.insert(out, c)
			end
		end
	end

	return out
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	args = args or {}
	runtime.api = args.api or {}
	return M
end

function M.remove(c)
	if not c then
		return
	end

	for s, st in pairs(runtime.stacks) do
		for i = #st, 1, -1 do
			local x = st[i]
			if (not x) or not x.valid or x == c then
				table.remove(st, i)
			end
		end

		if #st == 0 then
			runtime.stacks[s] = nil
		end
	end
end

function M.push(c)
	if not (c and c.valid and c.screen) then
		return
	end

	M.remove(c)

	local st = ensure_stack(c.screen)
	table.insert(st, 1, c)
end

function M.track(c)
	if not (c and c.valid) then
		return
	end

	if c.minimized then
		M.push(c)
	else
		M.remove(c)
	end
end

function M.pop_on_screen(s)
	if not (s and s.valid) then
		return nil
	end

	local st = runtime.stacks[s]
	if st then
		for i, c in ipairs(st) do
			if not (c and c.valid) then
				table.remove(st, i)
				return M.pop_on_screen(s)
			end

			if c.minimized and c.screen == s then
				return c
			end
		end

		if #st == 0 then
			runtime.stacks[s] = nil
		end
	end

	for _, c in ipairs(selected_tag_clients(s, true)) do
		if c.minimized then
			return c
		end
	end

	return nil
end

function M.attach_signals()
	if runtime.signals_ready then
		return
	end

	runtime.signals_ready = true

	client.connect_signal("property::minimized", function(c)
		M.track(c)
	end)

	client.connect_signal("property::screen", function(c)
		M.remove(c)
		M.track(c)
	end)

	client.connect_signal("tagged", function(c)
		M.track(c)
	end)

	client.connect_signal("untagged", function(c)
		M.track(c)
	end)

	client.connect_signal("unmanage", function(c)
		M.remove(c)
	end)
end

return M
