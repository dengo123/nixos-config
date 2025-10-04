-- ~/.config/awesome/input/keys/global/media.lua
local awful = require("awful")
local gears = require("gears")
local naughty = require("naughty")

-- -------- Befehle (PipeWire bevorzugt, sonst Pulse) --------
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

-- Liefert: "<percent> <mutestate>"
--  PipeWire: percent aus 0.00..1.00, mutestate true/false
--  Pulse:    percent mit %, mutestate yes/no
local QUERY_SINK = [[
sh -c '
if command -v wpctl >/dev/null; then
  v=$(wpctl get-volume @DEFAULT_AUDIO_SINK@ | awk "{print \$2}")
  m=$(wpctl get-mute   @DEFAULT_AUDIO_SINK@ | awk "{print \$2}")
  p=$(awk -v x="$v" "BEGIN{printf(\"%d\", (x*100)+0.5)}")
  [ "$m" = "true" ] && echo "$p on" || echo "$p off"
else
  v=$(pactl get-sink-volume @DEFAULT_SINK@  | awk "NR==1{print \$5}")  # e.g. 57%
  m=$(pactl get-sink-mute   @DEFAULT_SINK@  | awk "{print \$2}")       # yes/no
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

-- -------- OSD (nur Text, keine Emojis, nutzt dein notify-Theme) --------
local vol_notif_id

local function show_volume_osd()
	awful.spawn.easy_async_with_shell(QUERY_SINK, function(out)
		local p, mute = out:match("^(%d+)%s+(%S+)")
		local pct = tonumber(p or 0) or 0
		local muted = (mute == "on")
		local txt = ("%d%%/"):format(pct)
		if muted then
			txt = txt .. " (stumm)"
		end

		local n = naughty.notify({
			-- kein Titel, nur Text -> nutzt dein zentrisches Template
			text = txt,
			timeout = 1.2,
			replaces_id = vol_notif_id, -- ersetzt laufendes Popup
			-- keine Styles hier setzen -> alles kommt aus ui/theme/notify.lua
		})
		vol_notif_id = n and n.id or vol_notif_id
	end)
end

local function show_mic_osd()
	awful.spawn.easy_async_with_shell(QUERY_SRC, function(out)
		local muted = (out:match("%S+") == "on")
		local txt = muted and "Mic: stumm" or "Mic: aktiv"
		local n = naughty.notify({
			text = txt,
			timeout = 1.2,
			replaces_id = vol_notif_id,
		})
		vol_notif_id = n and n.id or vol_notif_id
	end)
end

-- -------- Export: Keymap --------
return function(_modkey, _cfg)
	return gears.table.join(
		awful.key({}, "XF86AudioRaiseVolume", function()
			awful.spawn(cmd_volume("3%+"), false)
			show_volume_osd()
		end, { description = "Volume +3% + OSD", group = "media" }),

		awful.key({}, "XF86AudioLowerVolume", function()
			awful.spawn(cmd_volume("3%-"), false)
			show_volume_osd()
		end, { description = "Volume -3% + OSD", group = "media" }),

		awful.key({}, "XF86AudioMute", function()
			awful.spawn(cmd_mute_toggle(), false)
			show_volume_osd()
		end, { description = "Mute toggle + OSD", group = "media" }),

		awful.key({}, "XF86AudioMicMute", function()
			awful.spawn(cmd_mic_toggle(), false)
			show_mic_osd()
		end, { description = "Mic mute toggle + OSD", group = "media" })
	)
end
