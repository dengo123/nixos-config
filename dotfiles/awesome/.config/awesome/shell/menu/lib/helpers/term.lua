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

	local function spawn(argv)
		return awful.spawn(argv)
	end
	local function sh(cmd)
		return awful.spawn.with_shell(cmd)
	end

	if term:find("wezterm") then
		return spawn({ term, "start", "--", script })
	elseif term:find("alacritty") or term:find("foot") or term:find("konsole") then
		return spawn({ term, "-e", script })
	elseif term:find("kitty") then
		return spawn({ term, "--", script })
	elseif term:find("gnome%-terminal") then
		return spawn({ term, "--", script })
	elseif term:find("ghostty") then
		-- WICHTIG: hinter -e genau EIN String
		local cmd = string.format("bash -lc %q", script)
		-- bevorzugt: sauber als argv: {"-e", "<string>"}
		local ok = spawn({ term, "-e", cmd })
		if ok then
			return ok
		end
		-- Fallbacks (nur falls Build -e nicht akzeptiert)
		ok = spawn({ term, "+new-window", "-e", cmd })
		if ok then
			return ok
		end
		return sh(string.format("%q -e %q || xterm -e %q", term, cmd, script))
	else
		return spawn({ term, "-e", script })
	end
end

return M
