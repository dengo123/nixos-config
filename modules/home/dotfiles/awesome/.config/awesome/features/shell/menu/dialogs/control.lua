-- ~/.config/awesome/features/shell/menu/dialogs/control/init.lua
local Base = require("features.shell.menu.dialogs.base")
local Lib = require("features.shell.menu.lib")
local Term = require("features.shell.menu.lib.term")

local M = {}

-- Shell-Kette: probiere mehrere Kommandos, notify-send als Hinweis wenn alles fehlt
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

-- Roh-Actions (emoji/label/on_press)
local function build_actions()
	return {
		-- Anzeige
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

		-- Audio
		{
			emoji = "🔊",
			label = "Audio",
			on_press = function()
				local cmd = chain(
					{ "pavucontrol", "helvum", "qpwgraph" },
					"Audio",
					"Installiere pavucontrol / helvum / qpwgraph oder nutze alsamixer."
				)
				Lib.cmd(cmd, policy)()
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

		-- Datenträger
		{
			emoji = "💽",
			label = "Datenträger",
			on_press = Lib.cmd(
				chain({ "gnome-disks" }, "Datenträger", "Installiere gnome-disk-utility (gnome-disks)."),
				policy
			),
		},

		-- Drucker
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

		-- Update
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

-- Items in N Spalten verteilen (round-robin)
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
		size = { w = 760, h = 640 },
		theme = theme_overrides,
		popup = { use_backdrop = false, close_on_escape = true },

		-- Columns-Fokus aktivieren
		focus = { mode = "columns", start_col = 1, mouse_follow = true },

		body_builder = function(th, dims, _get_close)
			-- 1) Actions → rows.lua-kompatible Items (text/icon/on_press)
			local items = {}
			for _, a in ipairs(build_actions()) do
				table.insert(items, {
					text = (a.emoji and (a.emoji .. " ") or "") .. (a.label or ""),
					icon = a.icon, -- optional (Bildpfad); bei Emoji nil lassen
					on_press = a.on_press, -- wird von Lib.actions.click(item) aufgerufen
				})
			end

			-- 2) Spaltenanzahl (2 Standard; 3 wenn im Theme gewünscht)
			local ncols = tonumber(th.control_columns) or 2
			local cols_items = distribute(items, ncols)

			-- 3) Columns-Spezifikation (Widths gleich verteilt; Rowhöhe aus Theme oder Fallback)
			local spec = {}
			local width_each = 1 / ncols
			local row_h = th.row_h or 48
			for i = 1, ncols do
				table.insert(spec, {
					key = "col" .. i,
					width = width_each,
					items = cols_items[i],
					row_h = row_h,
					-- optional: palette = { bg=..., fg=..., hover=... },
				})
			end

			-- 4) Columns bauen (liefert widget + Fokuslisten)
			local widget, focus_lists = Base.layouts_columns(spec, th, dims)
			return widget, focus_lists
		end,
	})
end

return M
