-- ~/.config/awesome/input/keys/helpers.lua
local awful = require("awful")

local H = {}

function H.scr_in_dir(dir)
	local s = awful.screen.focused()
	return s and s:get_next_in_direction(dir) or nil
end

function H.view_tag_idx(delta, s)
	s = s or awful.screen.focused()
	if not s or not s.selected_tag then
		return
	end
	awful.tag.viewidx(delta, s)
end

function H.move_tag_to_screen(dir)
	local s = awful.screen.focused()
	if not s or not s.selected_tag then
		return
	end
	local target = H.scr_in_dir(dir)
	if not target then
		return
	end
	local t = s.selected_tag
	t.screen = target
	t:view_only()
	awful.screen.focus(target)
end

function H.move_client_dir(dir)
	local c = client.focus
	if not c then
		return
	end
	awful.client.swap.bydirection(dir, c, nil)
end

function H.move_client_to_screen(dir)
	local c = client.focus
	if not c then
		return
	end
	local target = H.scr_in_dir(dir)
	if not target then
		return
	end
	c:move_to_screen(target)
	local t = target.selected_tag or (target.tags and target.tags[1])
	if t then
		c:move_to_tag(t)
		t:view_only()
	end
	c:raise()
end

function H.move_client_to_neighbor_tag(delta, follow)
	local s = awful.screen.focused()
	if not s or not s.selected_tag then
		return
	end
	local t = s.selected_tag
	local tags = s.tags
	local idx = t.index + delta
	local nt = tags[idx]
	local c = client.focus
	if c and nt then
		c:move_to_tag(nt)
		if follow then
			nt:view_only()
			client.focus = c
			c:raise()
		end
	end
end

return H
