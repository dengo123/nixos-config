-- ~/.config/awesome/features/shell/menu/dialogs/control/init.lua
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

local function build_actions()
	return {
		-- Anzeige
		{
			emoji = "🖥️",
			label = "Anzeige",
			on_press = Lib.cmd(
				chain({
					"arandr",
					"autorandr --change",
				}, "Anzeige", "Installiere arandr oder richte autorandr-Profile ein."),
				policy
			),
		},

		-- Audio
		{
			emoji = "🔊",
			label = "Audio",
			on_press = function()
				-- probiere GUI-Tools, sonst Terminal-TUI
				local cmd = chain({
					"pavucontrol",
					"helvum",
					"qpwgraph",
				}, "Audio", "Installiere pavucontrol / helvum / qpwgraph oder nutze alsamixer.")
				-- Erst GUI-Kette versuchen …
				Lib.cmd(cmd, policy)()
				-- … und zusätzlich eine Taste für TUI:
				-- (Wenn du stattdessen eine eigene Kachel willst, sag Bescheid.)
			end,
		},

		-- Netzwerk
		{
			emoji = "📶",
			label = "Netzwerk",
			on_press = function()
				Term.run("nmtui || nm-connection-editor")
			end,
		},

		-- Bluetooth
		{ emoji = "🌀", label = "Bluetooth", on_press = Lib.cmd("blueman-manager", policy) },

		-- Datenträger (GNOME Disks)
		{
			emoji = "💽",
			label = "Datenträger",
			on_press = Lib.cmd(
				chain({ "gnome-disks" }, "Datenträger", "Installiere gnome-disk-utility (gnome-disks)."),
				policy
			),
		},

		-- Drucker (GUI zum Entdecken/Testen; final in Nix pflegen)
		{
			emoji = "🖨️",
			label = "Drucker",
			on_press = Lib.cmd(
				chain({ "system-config-printer" }, "Drucker", "CUPS-Web-UI: http://localhost:631"),
				policy
			),
		},

		-- Passwörter
		{
			emoji = "🔐",
			label = "Passwörter",
			on_press = Lib.cmd(
				chain({ "keepassxc", "bitwarden", "1password" }, "Passwörter", "KeePassXC empfohlen."),
				policy
			),
		},

		-- Clipboard
		{
			emoji = "📋",
			label = "Clipboard",
			on_press = Lib.cmd(chain({ "copyq toggle", "copyq" }, "Clipboard", "Installiere CopyQ."), policy),
		},

		-- Dateien
		{
			emoji = "🗂️",
			label = "Dateien",
			on_press = Lib.cmd(
				chain({ "nemo", "thunar", "pcmanfm" }, "Dateimanager", "Kein Dateimanager gefunden."),
				policy
			),
		},

		-- Nix Config
		{
			emoji = "⚙️",
			label = "Nix Config",
			on_press = function()
				Term.run("cd ~/nixos-config || cd ~/nixforge || cd ~/.dotfiles; nvim .")
			end,
		},

		-- Update (flake update + rebuild)
		{
			emoji = "🚀",
			label = "Update",
			on_press = function()
				Term.run(
					"sudo nix flake update --flake ~/nixos-config && sudo nixos-rebuild switch --flake ~/nixos-config"
				)
			end,
		},
	}
end

function M.open(theme_overrides)
	return Base.dialog({
		container = "panel",
		title = "Control Panel",
		size = { w = 760, h = 640 },
		theme = theme_overrides,
		popup = { use_backdrop = false, close_on_escape = true },
		focus = { mode = "row" }, -- ← dazu
		body_builder = function(th, dims, get_close)
			local grid, items = Base.layouts_grid(build_actions(), th, dims, { cols = 2 }, function()
				return get_close()
			end)
			return grid, items
		end,
	})
end

return M
