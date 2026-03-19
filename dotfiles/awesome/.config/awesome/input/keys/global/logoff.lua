-- ~/.config/awesome/input/keys/global/logoff.lua
local awful = require("awful")
local gears = require("gears")

return function(modkey, launchers_api)
	-- =========================================================================
	-- Helpers
	-- =========================================================================

	local function bind(mods, key, fn, desc)
		return awful.key(mods, key, fn, {
			description = desc,
			group = "launchers",
		})
	end

	local function spawn(cmd)
		if type(cmd) == "table" then
			awful.spawn(cmd, false)
			return
		end

		if type(cmd) == "string" and cmd ~= "" then
			awful.spawn.with_shell(cmd)
		end
	end

	local function can_open_logoff()
		return launchers_api and launchers_api.open and type(launchers_api.open.logoff) == "function"
	end

	-- =========================================================================
	-- Modal Keys
	-- =========================================================================

	local function install_modal_keys(handle)
		local previous_root_keys = root.keys()
		local modal_keys = {}

		local modal_mod_sets = {
			{},
			{ modkey },
		}

		local function close_handle()
			if handle and type(handle.close) == "function" then
				handle.close()
			end
		end

		local function add_modal_key(key, fn, desc)
			for _, mods in ipairs(modal_mod_sets) do
				table.insert(
					modal_keys,
					awful.key(mods, key, fn, {
						description = desc or key,
						group = "launchers",
					})
				)
			end
		end

		-- ---------------------------------------------------------------------
		-- Actions
		-- ---------------------------------------------------------------------

		add_modal_key("l", function()
			close_handle()
			spawn({ "dm-tool", "lock" })
		end, "lock session")

		add_modal_key("o", function()
			close_handle()
			spawn("pkill -KILL -u " .. (os.getenv("USER") or ""))
		end, "log off")

		add_modal_key("u", function()
			close_handle()
			spawn({ "dm-tool", "switch-to-greeter" })
		end, "switch user")

		add_modal_key("Escape", function()
			close_handle()
		end, "cancel")

		-- ---------------------------------------------------------------------
		-- Install
		-- ---------------------------------------------------------------------

		local joined_keys
		if previous_root_keys then
			joined_keys = gears.table.join(previous_root_keys, table.unpack(modal_keys))
		else
			joined_keys = gears.table.join(table.unpack(modal_keys))
		end

		root.keys(joined_keys)

		local original_close = handle.close
		handle.close = function(...)
			if previous_root_keys then
				root.keys(previous_root_keys)
			else
				root.keys(nil)
			end

			if type(original_close) == "function" then
				return original_close(...)
			end
		end
	end

	-- =========================================================================
	-- Public Keys
	-- =========================================================================

	local function open_logoff_dialog()
		if not can_open_logoff() then
			return
		end

		local handle = launchers_api.open.logoff({})

		if handle then
			install_modal_keys(handle)
		end
	end

	return gears.table.join(bind({ modkey }, "Pause", open_logoff_dialog, "open logoff dialog (modal hotkeys)"))
end
