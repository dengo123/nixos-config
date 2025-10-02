-- ~/.config/awesome/shell/menu/lib/init.lua
local M = {}

-- --- helper: safe require ---------------------------------------------------
local function safe_require(path)
	local ok, mod = pcall(require, path)
	if ok then
		return mod
	end
	return nil
end

-- --- core modules (aggregiert; keine Legacy-/features-Fallbacks) -------------
local Actions = safe_require("shell.menu.lib.actions")
local Focus = safe_require("shell.menu.lib.focus")
local Layout = safe_require("shell.menu.lib.layout")
local Placement = safe_require("shell.menu.lib.placement")
local Term = safe_require("shell.menu.lib.term")
local ItemsMod = safe_require("shell.menu.lib.items") -- <— enthält start + tabs items

-- --- public: attach aggregator ----------------------------------------------
-- Hängt alle Libs unter api.lib an. Optional: flatten_* um direkt auf api zu legen.
function M.attach(api, opts)
	opts = opts or {}
	api.lib = api.lib or {}

	api.lib.actions = Actions
	api.lib.focus = Focus
	api.lib.layout = Layout
	api.lib.placement = Placement
	api.lib.term = Term
	api.lib.items = ItemsMod -- <- Quelle für Start- & Tabs-Items

	-- optionale Initializer der Submodule (falls vorhanden)
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

	-- optional: flatten (APIs direkt auf 'api' legen)
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

-- Kompat-Alias für Altcode
M.init = M.attach

-- --- actions passthroughs (optional & gefahrlos) ----------------------------
local passthrough = {
	"run",
	"run_dialog",
	"run_shell",
	"run_bin",
	"cmd",
	"lua",
	"signal",
	"click",
}
for _, k in ipairs(passthrough) do
	M[k] = function(...)
		if not Actions or not Actions[k] then
			return nil
		end
		return Actions[k](...)
	end
end

-- --- optionale Komfort-Funktion ---------------------------------------------
-- Nutzt ItemsMod.build_start(ctx), falls vorhanden. Rein pass-through.
function M.defaults(ctx)
	if not ItemsMod then
		return nil
	end
	if type(ItemsMod.build_start) == "function" then
		return ItemsMod.build_start(ctx)
	end
	-- Wenn du keinen Bedarf hast, entferne die Funktion oder lass sie nil zurückgeben.
	return nil
end

-- Items-Modul zusätzlich exponieren (falls du's direkt importieren willst)
M.items = ItemsMod

return M
