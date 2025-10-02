-- ~/.config/awesome/shell/menu/lib/init.lua
local M = {}

local function safe_require(path)
	local ok, mod = pcall(require, path)
	if ok then
		return mod
	end
	return nil
end

local Actions = safe_require("shell.menu.lib.actions")
local Focus = safe_require("shell.menu.lib.focus")
local Layout = safe_require("shell.menu.lib.layout")
local Placement = safe_require("shell.menu.lib.placement")
local Term = safe_require("shell.menu.lib.term")
local ItemsMod = safe_require("shell.menu.lib.items") -- <- HIER liegen Start+Tabs Items

function M.attach(api, opts)
	opts = opts or {}
	api.lib = api.lib or {}

	api.lib.actions = Actions
	api.lib.focus = Focus
	api.lib.layout = Layout
	api.lib.placement = Placement
	api.lib.term = Term
	api.lib.items = ItemsMod

	if Actions and type(Actions.init) == "function" then
		Actions.init(api)
	end
	if Focus and type(Focus.init) == "function" then
		Focus.init(api)
	end
	if Layout and type(Layout.init) == "function" then
		Layout.init(api)
	end
	if Placement and type(Placement.init) == "function" then
		Placement.init(api)
	end
	if Term and type(Term.init) == "function" then
		Term.init(api)
	end

	if opts.flatten_actions and Actions then
		for k, v in pairs(Actions) do
			if api[k] == nil then
				api[k] = v
			end
		end
	end
	if opts.flatten_focus and Focus then
		for k, v in pairs(Focus) do
			if api[k] == nil then
				api[k] = v
			end
		end
	end
	if opts.flatten_layout and Layout then
		for k, v in pairs(Layout) do
			if api[k] == nil then
				api[k] = v
			end
		end
	end
	if opts.flatten_placement and Placement then
		for k, v in pairs(Placement) do
			if api[k] == nil then
				api[k] = v
			end
		end
	end
	if opts.flatten_term and Term then
		for k, v in pairs(Term) do
			if api[k] == nil then
				api[k] = v
			end
		end
	end

	return api
end

M.init = M.attach

local passthrough = { "run", "run_dialog", "run_shell", "run_bin", "cmd", "lua", "signal", "click" }
for _, k in ipairs(passthrough) do
	M[k] = function(...)
		if not Actions or not Actions[k] then
			return nil
		end
		return Actions[k](...)
	end
end

-- optionaler Convenience-Alias (delegiert direkt ins Items-Modul)
function M.defaults(ctx)
	if not ItemsMod or type(ItemsMod.build_start) ~= "function" then
		return nil
	end
	return ItemsMod.build_start(ctx)
end

-- Falls du das Items-Modul direkt brauchst:
M.items = ItemsMod

return M
