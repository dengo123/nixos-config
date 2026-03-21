-- ~/.config/awesome/input/global/media.lua
local awful = require("awful")
local gears = require("gears")
local naughty = require("naughty")

local M = {}

local vol_notif_id

-- =========================================================================
-- Commands
-- =========================================================================

local function cmd_volume(step)
	return "sh -c 'command -v wpctl >/dev/null && wpctl set-volume @DEFAULT_AUDIO_SINK@ "
		.. step
		.. " || pactl set-sink-volume @DEFAULT_SINK@ "
		.. step
		.. "'"
end

local function cmd_mute_toggle()
	return "sh -c 'command -v wpctl >/dev/null && wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle || pactl set-sink-mute @DEFAULT_SINK@ toggle'"
end

local function cmd_mic_toggle()
	return "sh -c 'command -v wpctl >/dev/null && wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle || pactl set-source-mute @DEFAULT_SOURCE@ toggle'"
end

local QUERY_SINK = [[
sh -c '
if command -v wpctl >/dev/null; then
  v=$(wpctl get-volume @DEFAULT_AUDIO_SINK@ | awk "{print \$2}")
  m=$(wpctl get-mute   @DEFAULT_AUDIO_SINK@ | awk "{print \$2}")
  p=$(awk -v x="$v" "BEGIN{printf(\"%d\", (x*100)+0.5)}")
  [ "$m" = "true" ] && echo "$p on" || echo "$p off"
else
  v=$(pactl get-sink-volume @DEFAULT_SINK@  | awk "NR==1{print \$5}")
  m=$(pactl get-sink-mute   @DEFAULT_SINK@  | awk "{print \$2}")
  p=${v%%%}
  [ "$m" = "yes" ] && echo "$p on" || echo "$p off"
fi
']]

local QUERY_SRC = [[
sh -c '
if command -v wpctl >/dev/null; then
  m=$(wpctl get-mute @DEFAULT_AUDIO_SOURCE@ | awk "{print \$2}")
  [ "$m" = "true" ] && echo "on" || echo "off"
else
  m=$(pactl get-source-mute @DEFAULT_SOURCE@ | awk "{print \$2}")
  [ "$m" = "yes" ] && echo "on" || echo "off"
fi
']]

-- =========================================================================
-- Helpers
-- =========================================================================

local function media_cfg(cfg)
	local input_cfg = (cfg and cfg.input) or {}
	return input_cfg.media or {}
end

local function volume_step(cfg)
	local step = tonumber(media_cfg(cfg).volume_step)
	if not step or step <= 0 then
		return 3
	end

	return math.floor(step)
end

local function osd_timeout(cfg)
	local timeout = tonumber(media_cfg(cfg).osd_timeout)
	if not timeout or timeout <= 0 then
		return 1.2
	end

	return timeout
end

local function show_volume_osd(timeout)
	awful.spawn.easy_async_with_shell(QUERY_SINK, function(out)
		local p, mute = out:match("^(%d+)%s+(%S+)")
		local pct = tonumber(p or 0) or 0
		local muted = (mute == "on")
		local txt = ("%d%%"):format(pct)

		if muted then
			txt = txt .. " (Stumm)"
		end

		local n = naughty.notify({
			text = txt,
			timeout = timeout,
			replaces_id = vol_notif_id,
			skip_history = true,
		})

		vol_notif_id = n and n.id or vol_notif_id
	end)
end

local function show_mic_osd(timeout)
	awful.spawn.easy_async_with_shell(QUERY_SRC, function(out)
		local muted = (out:match("%S+") == "on")
		local txt = muted and "Mic: Stumm" or "Mic: Aktiv"

		local n = naughty.notify({
			text = txt,
			timeout = timeout,
			replaces_id = vol_notif_id,
			skip_history = true,
		})

		vol_notif_id = n and n.id or vol_notif_id
	end)
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.build(_modkey, cfg)
	local step = volume_step(cfg)
	local timeout = osd_timeout(cfg)
	local step_up = tostring(step) .. "%+"
	local step_down = tostring(step) .. "%-"

	return gears.table.join(
		awful.key({}, "XF86AudioRaiseVolume", function()
			awful.spawn(cmd_volume(step_up), false)
			show_volume_osd(timeout)
		end, {
			description = "Raise Volume +" .. tostring(step) .. "% + OSD",
			group = "Media",
		}),

		awful.key({}, "XF86AudioLowerVolume", function()
			awful.spawn(cmd_volume(step_down), false)
			show_volume_osd(timeout)
		end, {
			description = "Lower Volume -" .. tostring(step) .. "% + OSD",
			group = "Media",
		}),

		awful.key({}, "XF86AudioMute", function()
			awful.spawn(cmd_mute_toggle(), false)
			show_volume_osd(timeout)
		end, {
			description = "Toggle Mute + OSD",
			group = "Media",
		}),

		awful.key({}, "XF86AudioMicMute", function()
			awful.spawn(cmd_mic_toggle(), false)
			show_mic_osd(timeout)
		end, {
			description = "Toggle Mic Mute + OSD",
			group = "Media",
		})
	)
end

return function(modkey, cfg)
	return M.build(modkey, cfg)
end
