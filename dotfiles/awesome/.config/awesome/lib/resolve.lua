-- ~/.config/awesome/lib/resolve.lua
local M = {}

local function split_path(path)
	local out = {}

	if type(path) ~= "string" or path == "" then
		return out
	end

	for part in path:gmatch("[^%.]+") do
		table.insert(out, part)
	end

	return out
end

function M.path(root, path)
	if type(root) ~= "table" then
		return nil
	end

	local node = root

	for _, key in ipairs(split_path(path)) do
		if type(node) ~= "table" then
			return nil
		end

		node = node[key]

		if node == nil then
			return nil
		end
	end

	return node
end

function M.first(root, paths)
	if type(paths) ~= "table" then
		return nil
	end

	for _, path in ipairs(paths) do
		local value = M.path(root, path)
		if value ~= nil then
			return value
		end
	end

	return nil
end

function M.pick(root, ...)
	local n = select("#", ...)

	for i = 1, n do
		local getter = select(i, ...)
		if type(getter) == "function" then
			local ok, value = pcall(getter, root)
			if ok and value ~= nil then
				return value
			end
		end
	end

	return nil
end

function M.ensure(root, path)
	if type(root) ~= "table" or type(path) ~= "string" or path == "" then
		return nil
	end

	local node = root
	local parts = split_path(path)

	for _, key in ipairs(parts) do
		if type(node[key]) ~= "table" then
			node[key] = {}
		end

		node = node[key]
	end

	return node
end

function M.set(root, path, value)
	if type(root) ~= "table" or type(path) ~= "string" or path == "" then
		return nil
	end

	local parts = split_path(path)
	if #parts == 0 then
		return nil
	end

	local last = table.remove(parts)
	local parent = root

	for _, key in ipairs(parts) do
		if type(parent[key]) ~= "table" then
			parent[key] = {}
		end

		parent = parent[key]
	end

	parent[last] = value
	return value
end

return M
