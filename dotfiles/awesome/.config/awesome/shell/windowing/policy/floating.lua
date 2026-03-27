-- ~/.config/awesome/shell/windowing/policy/floating.lua
local gears = require("gears")

local M = {}

local runtime = {
	clients = nil,
	portrait_autosize_ready = false,
	centered_autosize_ready = false,
	tall_centered_reinforce_ready = false,
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function clients()
	return runtime.clients
end

-- =========================================================================
-- Portrait Autosize
-- =========================================================================

function M.portrait_autosize_apply(c)
	if not (c and c.valid) then
		return
	end

	if not c.portrait_autosize then
		return
	end

	if not c.floating or c.fullscreen or c.maximized then
		return
	end

	local s = c.screen
	if not s then
		return
	end

	local wa = s.workarea
	if wa.height <= wa.width then
		return
	end

	local w = math.floor(wa.width * 0.92)
	local h = math.floor(wa.height / 3)
	local x = wa.x + math.floor((wa.width - w) / 2)
	local y = wa.y + math.floor((wa.height - h) / 2)

	c:geometry({
		x = x,
		y = y,
		width = w,
		height = h,
	})
end

local function hook_portrait_autosize()
	if runtime.portrait_autosize_ready then
		return
	end

	client.connect_signal("manage", function(c)
		gears.timer.delayed_call(function()
			M.portrait_autosize_apply(c)
		end)
	end)

	client.connect_signal("property::floating", function(c)
		M.portrait_autosize_apply(c)
	end)

	screen.connect_signal("property::geometry", function(s)
		for _, c in ipairs(s.clients) do
			M.portrait_autosize_apply(c)
		end
	end)

	runtime.portrait_autosize_ready = true
end

-- =========================================================================
-- Centered Autosize
-- =========================================================================

function M.centered_autosize_apply(c)
	if not (c and c.valid) then
		return
	end

	if not c.centered_autosize then
		return
	end

	if not c.floating or c.fullscreen then
		return
	end

	local s = c.screen
	if not s then
		return
	end

	local wa = s.workarea
	local width = math.floor(math.min(wa.width * 0.36, 620))
	width = math.max(width, 420)

	local height = math.floor(width * 1.60)
	local max_height = math.floor(wa.height * 0.90)

	if height > max_height then
		height = max_height
		width = math.floor(height / 1.60)
	end

	local x = wa.x + math.floor((wa.width - width) / 2)
	local y = wa.y + math.floor((wa.height - height) / 2)

	c.maximized = false
	c.maximized_horizontal = false
	c.maximized_vertical = false

	c:geometry({
		x = x,
		y = y,
		width = width,
		height = height,
	})
end

local function hook_centered_autosize()
	if runtime.centered_autosize_ready then
		return
	end

	client.connect_signal("manage", function(c)
		gears.timer.delayed_call(function()
			M.centered_autosize_apply(c)
		end)
	end)

	client.connect_signal("property::floating", function(c)
		M.centered_autosize_apply(c)
	end)

	screen.connect_signal("property::geometry", function(s)
		for _, c in ipairs(s.clients) do
			M.centered_autosize_apply(c)
		end
	end)

	runtime.centered_autosize_ready = true
end

-- =========================================================================
-- Tall Centered Reinforce
-- =========================================================================

function M.normalize_tall_centered_client(c)
	if not (c and c.valid) then
		return
	end

	c.floating = true
	c.fullscreen = false
	c.maximized = false
	c.maximized_horizontal = false
	c.maximized_vertical = false
	c.centered_autosize = true
	c.portrait_autosize = false

	M.centered_autosize_apply(c)
end

function M.reinforce_tall_centered_client(c)
	local Clients = clients()
	if not (Clients and Clients.is_tall_centered_client and Clients.is_tall_centered_client(c)) then
		return
	end

	M.normalize_tall_centered_client(c)

	gears.timer.delayed_call(function()
		M.normalize_tall_centered_client(c)
	end)

	gears.timer.start_new(0.15, function()
		M.normalize_tall_centered_client(c)
		return false
	end)

	gears.timer.start_new(0.50, function()
		M.normalize_tall_centered_client(c)
		return false
	end)
end

local function hook_tall_centered_reinforce()
	if runtime.tall_centered_reinforce_ready then
		return
	end

	client.connect_signal("property::maximized", function(c)
		local Clients = clients()
		if Clients and Clients.is_tall_centered_client and Clients.is_tall_centered_client(c) and c.floating then
			gears.timer.delayed_call(function()
				M.normalize_tall_centered_client(c)
			end)
		end
	end)

	client.connect_signal("property::fullscreen", function(c)
		local Clients = clients()
		if Clients and Clients.is_tall_centered_client and Clients.is_tall_centered_client(c) and c.floating then
			gears.timer.delayed_call(function()
				M.normalize_tall_centered_client(c)
			end)
		end
	end)

	runtime.tall_centered_reinforce_ready = true
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	args = args or {}
	runtime.clients = args.clients or runtime.clients

	hook_portrait_autosize()
	hook_centered_autosize()
	hook_tall_centered_reinforce()

	return M
end

return M
