-- ~/.config/awesome/shell/windowing/minimize_stack.lua
local M = {}

-- schwache Keys -> Screens können GC'd werden
local stacks = setmetatable({}, { __mode = "k" })

local function ensure_stack(s)
	if not stacks[s] then
		stacks[s] = {}
	end
	return stacks[s]
end

-- Aufrufen, wenn ein Client minimiert wird
function M.push(c)
	if not (c and c.valid) then
		return
	end
	local s = c.screen
	local st = ensure_stack(s)
	-- vorne einfügen = LIFO
	table.insert(st, 1, c)
end

-- Entferne spezifischen Client aus allen Stacks (z. B. bei unmanage)
function M.remove(c)
	if not c then
		return
	end
	for s, st in pairs(stacks) do
		for i = #st, 1, -1 do
			local x = st[i]
			if (not x) or not x.valid or x == c then
				table.remove(st, i)
			end
		end
		if #st == 0 then
			stacks[s] = nil
		end
	end
end

-- Restore auf einem Screen: hole den ersten gültigen, noch minimierten Client
function M.pop_on_screen(s)
	local st = stacks[s]
	if st then
		for i, c in ipairs(st) do
			if c and c.valid and c.minimized and c.screen == s then
				table.remove(st, i)
				return c
			end
			if not (c and c.valid) then
				table.remove(st, i)
			end
		end
	end
	-- Fallback: beliebigen minimierten Client auf diesem Screen suchen
	for _, c in ipairs(s.clients) do
		if c.minimized then
			return c
		end
	end
	return nil
end

return M
