-- shell/windowing/policies/gaps.lua
local awful = require("awful")
local beautiful = require("beautiful")
local xr = require("beautiful.xresources")
local dpi = xr.apply_dpi

local G = {}

function G.init(o)
	o = o or {}
	-- Nimm *Pixel* aus beautiful, sonst fallback
	local GAP_PX = beautiful.useless_gap or dpi(8)
	local KEEP_ONE = (beautiful.gap_single_client ~= false)

	local PAD_MAX = (beautiful.max_pad_on ~= false) -- default: an
	local PAD_SAME = (beautiful.max_pad_same_as_gap ~= false) -- default: Padding = Gap
	local PAD_USER = beautiful.max_padding -- optional {top,right,bottom,left} in px

	local function set_padding(s, pad)
		if not s then
			return
		end
		awful.screen.padding(s, pad or { left = 0, right = 0, top = 0, bottom = 0 })
	end

	local function compute_pad(s, t)
		local lay = (t and t.layout) or awful.layout.get(s)
		if lay ~= awful.layout.suit.max or not PAD_MAX then
			return nil
		end
		if type(PAD_USER) == "table" then
			return {
				top = PAD_USER.top or 0,
				right = PAD_USER.right or 0,
				bottom = PAD_USER.bottom or 0,
				left = PAD_USER.left or 0,
			}
		end
		if PAD_SAME then
			local g = GAP_PX
			return { top = g, right = g, bottom = g, left = g }
		end
		return nil
	end

	local function apply_for_screen(s)
		if not s or not s.selected_tag then
			return
		end
		local t = s.selected_tag
		t.gap = GAP_PX -- bereits px (dpi angewendet im Theme)
		t.gap_single_client = KEEP_ONE
		set_padding(s, compute_pad(s, t)) -- Rand im max-Layout
	end

	awful.screen.connect_for_each_screen(apply_for_screen)
	tag.connect_signal("property::layout", function(t)
		if t and t.screen then
			apply_for_screen(t.screen)
		end
	end)
	tag.connect_signal("property::selected", function(t)
		if t and t.screen then
			apply_for_screen(t.screen)
		end
	end)
	screen.connect_signal("tag::history::update", apply_for_screen)
end

return G
