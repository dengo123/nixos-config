-- ~/.config/awesome/lib/compat.lua
local Resolve = require("lib.resolve")

local M = {}

local function ensure_base_tables(ctx)
	Resolve.ensure(ctx, "api")
	Resolve.ensure(ctx, "external")
	Resolve.ensure(ctx, "services")
	Resolve.ensure(ctx, "features")
	Resolve.ensure(ctx, "policy")
	Resolve.ensure(ctx, "shell")
	Resolve.ensure(ctx, "input")
	Resolve.ensure(ctx, "system")
end

local function mirror(ctx, target_path, candidate_paths)
	local value = Resolve.first(ctx, candidate_paths)

	if value ~= nil then
		Resolve.set(ctx, target_path, value)
	end
end

function M.apply(ctx)
	if type(ctx) ~= "table" then
		return ctx
	end

	ensure_base_tables(ctx)

	-- ---------------------------------------------------------------------
	-- Windowing / shell features
	-- ---------------------------------------------------------------------

	mirror(ctx, "external.clients", {
		"features.windowing.clients",
		"shell.windowing.clients",
		"api.clients",
	})

	mirror(ctx, "external.titlebars", {
		"features.windowing.titlebars",
		"shell.windowing.titlebars",
		"api.titlebars",
	})

	mirror(ctx, "external.floating", {
		"features.windowing.floating",
		"shell.windowing.floating",
		"api.floating",
	})

	mirror(ctx, "external.actions", {
		"features.windowing.actions",
		"shell.windowing.actions",
		"api.actions",
	})

	-- ---------------------------------------------------------------------
	-- Services
	-- ---------------------------------------------------------------------

	mirror(ctx, "external.minimized", {
		"services.minimized",
		"features.windowing.minimized",
		"shell.windowing.minimized",
		"api.minimized",
	})

	-- ---------------------------------------------------------------------
	-- API compatibility mirrors
	-- ---------------------------------------------------------------------

	mirror(ctx, "api.clients", {
		"external.clients",
		"features.windowing.clients",
		"shell.windowing.clients",
	})

	mirror(ctx, "api.titlebars", {
		"external.titlebars",
		"features.windowing.titlebars",
		"shell.windowing.titlebars",
	})

	mirror(ctx, "api.floating", {
		"external.floating",
		"features.windowing.floating",
		"shell.windowing.floating",
	})

	mirror(ctx, "api.actions", {
		"external.actions",
		"features.windowing.actions",
		"shell.windowing.actions",
	})

	mirror(ctx, "api.minimized", {
		"external.minimized",
		"services.minimized",
		"features.windowing.minimized",
		"shell.windowing.minimized",
	})

	return ctx
end

return M
