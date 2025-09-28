-- ~/.config/awesome/features/shell/menu/dialogs/control.lua
local Base = require("features.shell.menu.dialogs.base")
local Lib = require("features.shell.menu.lib")
local Term = require("features.shell.menu.lib.term")

local M = {}

local function chain(cmds, title, msg)
	local parts = {}
	for _, c in ipairs(cmds) do
		table.insert(parts, "(" .. c .. ")")
	end
	local try = table.concat(parts, " || ")
	local note = string.format(
		[[ || (command -v notify-send >/dev/null 2>&1 && notify-send %q %q || true)]],
		title or "Control Panel",
		msg or "Kein passendes Tool gefunden."
	)
	return try .. note
end

local policy = { close = "before" }

-- Roh-Actions (emoji/label/on_press) – überall Policies verwenden
local function build_actions()
	return {
		{
			emoji = "🖥️",
			label = "Anzeige",
			on_press = Lib.cmd(
				chain(
					{ "arandr", "autorandr --change" },
					"Anzeige",
					"Installiere arandr oder richte autorandr-Profile ein."
				),
				policy
			),
		},
		{
			emoji = "🔊",
			label = "Audio",
			on_press = Lib.cmd(
				chain(
					{ "pavucontrol", "helvum", "qpwgraph" },
					"Audio",
					"Installiere pavucontrol / helvum / qpwgraph oder nutze alsamixer."
				),
				policy
			),
		},
		{
			emoji = "📶",
			label = "Netzwerk",
			on_press = Lib.cmd("nm-connection-editor", policy),
		},
		{ emoji = "🌀", label = "Bluetooth", on_press = Lib.cmd("blueman-manager", policy) },
		{
			emoji = "💽",
			label = "Datenträger",
			on_press = Lib.cmd(
				chain({ "gnome-disks" }, "Datenträger", "Installiere gnome-disk-utility (gnome-disks)."),
				policy
			),
		},
		{
			emoji = "🖨️",
			label = "Drucker",
			on_press = Lib.cmd(
				chain({ "system-config-printer" }, "Drucker", "CUPS-Web-UI: http://localhost:631"),
				policy
			),
		},
		{
			emoji = "🔐",
			label = "Passwörter",
			on_press = Lib.cmd(
				chain({ "keepassxc", "bitwarden", "1password" }, "Passwörter", "KeePassXC empfohlen."),
				policy
			),
		},
		{
			emoji = "📋",
			label = "Clipboard",
			on_press = Lib.cmd(chain({ "copyq toggle", "copyq" }, "Clipboard", "Installiere CopyQ."), policy),
		},
		{
			emoji = "🗂️",
			label = "Dateien",
			on_press = Lib.cmd(
				chain({ "nemo", "thunar", "pcmanfm" }, "Dateimanager", "Kein Dateimanager gefunden."),
				policy
			),
		},
		{
			emoji = "⚙️",
			label = "Awesome Config",
			on_press = Lib.lua(function()
				Term.run("cd ~/nixos-config/modules/home/dotfiles/awesome/.config/awesome; nvim .")
			end, policy),
		},
		{
			emoji = "🚀",
			label = "Update",
			on_press = Lib.lua(function()
				Term.run([[
    nix flake update --flake ~/nixos-config &&
    sudo nixos-rebuild switch --flake ~/nixos-config
  ]])
			end, policy),
		},
	}
end

local function distribute(items, n)
	n = math.max(1, tonumber(n) or 1)
	local cols = {}
	for i = 1, n do
		cols[i] = {}
	end
	for i, it in ipairs(items or {}) do
		table.insert(cols[((i - 1) % n) + 1], it)
	end
	return cols
end

function M.open(theme_overrides)
	return Base.dialog({
		container = "panel",
		title = "Control Panel",
		theme = theme_overrides,
		popup = { use_backdrop = false, close_on_escape = true },
		focus = { mode = "columns", start_col = 1, mouse_follow = true },

		body_builder = function(th, dims, _get_close)
			-- 1) Items bauen
			local items = {}
			for _, a in ipairs(build_actions()) do
				table.insert(items, {
					text = (a.emoji and (a.emoji .. " ") or "") .. (a.label or ""),
					icon = a.icon,
					on_press = a.on_press,
				})
			end

			-- 2) Spalten
			local ncols = tonumber(th.control_columns) or 2
			local cols_items = distribute(items, ncols)

			-- 3) Spec
			local spec = {}
			local width_each = 1 / ncols
			local row_h = th.row_h or 48
			for i = 1, ncols do
				table.insert(spec, { key = "col" .. i, width = width_each, items = cols_items[i], row_h = row_h })
			end

			-- 4) Lib mit close-Injection, damit Aktionen den Dialog schließen
			local function wrap_lib_for_close(lib, get_close)
				local actions = lib.actions or {}
				return {
					helpers = lib.helpers,
					actions = {
						click = function(item)
							local orig = actions.click(item) -- -> function([close])
							return function()
								local close = get_close()
								pcall(orig, close)
							end
						end,
					},
				}
			end
			local wrapped_lib = wrap_lib_for_close(Lib, _get_close)

			-- 5) Columns direkt bauen, damit wir opts.lib setzen können
			local Columns = require("features.shell.menu.layouts.columns")
			local api = Columns.build(spec, th, {
				lib = wrapped_lib,
				pad_l = dims.pad_h,
				pad_r = dims.pad_h,
				pad_t = dims.pad_v,
				pad_b = dims.pad_v,
				spacing = th.cols_spacing or 0,
			})
			local widget = api.widget
			local focus_lists = (api.get_focus_items and api:get_focus_items()) or {}
			return widget, focus_lists
		end,
	})
end

return M
