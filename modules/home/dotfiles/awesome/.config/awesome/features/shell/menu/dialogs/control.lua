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
			on_press = function()
				local cmd = chain(
					{ "pavucontrol", "helvum", "qpwgraph" },
					"Audio",
					"Installiere pavucontrol / helvum / qpwgraph oder nutze alsamixer."
				)
				Lib.cmd(cmd, policy)()
			end,
		},

		{
			emoji = "📶",
			label = "Netzwerk",
			on_press = function()
				Term.run("nmtui || nm-connection-editor")
			end,
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
			label = "Nix Config",
			on_press = function()
				Term.run("cd ~/nixos-config || cd ~/nixforge || cd ~/.dotfiles; nvim .")
			end,
		},

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

-- Hilfsfunktion: Liste in zwei Spalten aufteilen
local function split_even(a)
	local left, right = {}, {}
	for i, it in ipairs(a or {}) do
		if i % 2 == 1 then
			table.insert(left, it)
		else
			table.insert(right, it)
		end
	end
	return left, right
end

function M.open(theme_overrides)
	return Base.dialog({
		container = "panel",
		title = "Control Panel",
		size = { w = 760, h = 640 },
		theme = theme_overrides,
		popup = { use_backdrop = false, close_on_escape = true },
		focus = { mode = "columns", start_col = 1, mouse_follow = true },

		body_builder = function(th, dims, _get_close)
			-- 1) Actions → rows.lua-kompatible Items
			local items = {}
			for _, a in ipairs(build_actions()) do
				table.insert(items, {
					-- rows.lua nutzt 'text' und optional 'icon' (Bild). Emoji geht ins Text.
					text = (a.emoji and (a.emoji .. " ") or "") .. (a.label or ""),
					icon = a.icon, -- optional: Bildpfad; bei Emoji einfach nil lassen
					on_press = a.on_press, -- Lib.actions.click(item) greift das auf
				})
			end

			-- 2) In 2 Spalten verteilen (für 3 Spalten unten alternative Variante)
			local left, right = {}, {}
			for i, it in ipairs(items) do
				if i % 2 == 1 then
					table.insert(left, it)
				else
					table.insert(right, it)
				end
			end

			-- 3) Columns-Spezifikation (Columns baut selbst Widgets via Widgets.rows.list_widget)
			local spec = {
				{ key = "left", width = 0.50, items = left, row_h = th.row_h or 48 },
				{ key = "right", width = 0.50, items = right, row_h = th.row_h or 48 },
			}

			-- (→ Für 3 Spalten:
			-- local c1,c2,c3 = {},{},{}
			-- for i, it in ipairs(items) do local k=(i-1)%3; (k==0 and table.insert(c1,it)) or (k==1 and table.insert(c2,it)) or table.insert(c3,it) end
			-- spec = { {key="c1",width=1/3,items=c1,row_h=...}, {key="c2",...}, {key="c3",...} }
			--)

			local widget, focus_lists = Base.layouts_columns(spec, th, dims)
			return widget, focus_lists
		end,
	})
end

return M
