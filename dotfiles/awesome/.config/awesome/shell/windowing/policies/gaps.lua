-- windowing/policies/gaps.lua
local awful = require("awful")
local beautiful = require("beautiful")
local xr = require("beautiful.xresources")
local dpi = xr.apply_dpi

local G = {}
G.__index = G

function G.init(o)
	local gap_dpi = (o and o.gap_default_dpi) or 8
	local keep_one = (o and o.keep_gap_single_client) ~= false

	local function apply_to_tag(t)
		if not t then
			return
		end
		local l = t.layout or awful.layout.get(t.screen)
		t.gap = (l == awful.layout.suit.max) and 0 or dpi(gap_dpi)
		t.gap_single_client = keep_one
	end

	awful.screen.connect_for_each_screen(function(s)
		for _, t in ipairs(s.tags or {}) do
			apply_to_tag(t)
		end
	end)

	tag.connect_signal("property::layout", apply_to_tag)
	tag.connect_signal("property::selected", apply_to_tag)
end

return G
