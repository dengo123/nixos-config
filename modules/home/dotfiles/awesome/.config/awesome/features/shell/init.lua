-- features/shell/init.lua
-- zentraler Einstiegspunkt: Menü + Bar (Model/View)

local M = {
	menu = require("features.shell.menu"),
	bar = {
		model = require("features.shell.model"),
		view = require("features.shell.view"),
	},
}

-- wrapper: damit rc.lua nur shell.bar.setup nutzen muss
function M.bar.setup(s, opts)
	local model = M.bar.model.build(s, opts or {})
	return M.bar.view.place(s, model, opts or {})
end

return M
