-- ~/.config/awesome/lib/context.lua
local M = {}

local function ensure_table(v)
	if type(v) == "table" then
		return v
	end

	return {}
end

function M.new(seed)
	seed = ensure_table(seed)

	local ctx = {
		cfg = ensure_table(seed.cfg),
		ui = ensure_table(seed.ui),
		state = ensure_table(seed.state),

		modkey = seed.modkey,

		services = ensure_table(seed.services),
		features = ensure_table(seed.features),
		policy = ensure_table(seed.policy),

		input = ensure_table(seed.input),
		system = ensure_table(seed.system),
		shell = ensure_table(seed.shell),

		external = ensure_table(seed.external),
		api = ensure_table(seed.api),
	}

	return ctx
end

return M
