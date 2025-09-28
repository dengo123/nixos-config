-- ~/.config/awesome/features/shell/menu/lib/term.lua
local awful = require("awful")

local M = {}

local function read_cfg()
	local ok, cfg = pcall(require, "system.config")
	if ok and type(cfg) == "table" then
		return cfg
	end
	return { terminal = "xterm" }
end

-- schreibe den Command in ein temporäres Bash-Skript und gib den Pfad zurück
local function write_temp_script(cmdline)
	local path = os.tmpname()
	if not path:match("%.sh$") then
		path = path .. ".sh"
	end
	local f = assert(io.open(path, "w"))
	f:write("#!/usr/bin/env bash\n")
	f:write("set -euo pipefail\n")
	f:write(cmdline .. "\n")
	f:close()
	os.execute(string.format("chmod +x %q", path))
	return path
end

function M.run(cmdline)
	local term = (read_cfg().terminal or "xterm"):gsub("%s+$", "")
	local script = write_temp_script(cmdline)

	local function spawn_argv(argv)
		return awful.spawn(argv)
	end
	local function spawn_sh(sh)
		return awful.spawn.with_shell(sh)
	end

	if term:find("wezterm") then
		return spawn_argv({ term, "start", "--", script })
	elseif term:find("alacritty") then
		return spawn_argv({ term, "-e", script })
	elseif term:find("kitty") then
		return spawn_argv({ term, "--", script })
	elseif term:find("konsole") then
		return spawn_argv({ term, "-e", script })
	elseif term:find("foot") then
		return spawn_argv({ term, "-e", script })
	elseif term:find("gnome%-terminal") then
		return spawn_argv({ term, "--", script })
	elseif term:find("ghostty") then
		-- 1) bevorzugt: neues Fenster und dort bash -lc <script>
		local ok = spawn_argv({ term, "+new-window", "-e", "bash", "-lc", script })
		if ok then
			return ok
		end
		-- 2) manche Builds: nur -e
		ok = spawn_argv({ term, "-e", "bash", "-lc", script })
		if ok then
			return ok
		end
		-- 3) letzter Rettungsanker: Shell-Fallback (falls beide argv-Formen nicht starten)
		local chain = table.concat({
			string.format("%q +new-window -e bash -lc %q", term, script),
			string.format("%q -e bash -lc %q", term, script),
			string.format("%q -- bash -lc %q", term, script),
			string.format("xterm -e %q", script),
		}, " || ")
		return spawn_sh(chain)
	else
		return spawn_argv({ term, "-e", script })
	end
end

return M
