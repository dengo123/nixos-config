-- ~/.config/awesome/features/shell/menu/dialogs/control.lua
local Base = require("features.shell.menu.dialogs.base")
local Lib = require("features.shell.menu.lib")
local Term = require("features.shell.menu.lib.helpers.term")

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

-- Hilfsfunktion: Höhe aus Zeilenanzahl/Theme/Padding ableiten
local function compute_dialog_height(th, rows_max, per_col)
	th = th or {}
	local header_h = th.header_h or 80
	local footer_h = th.footer_h or 80
	local pad_v = th.pad_v or 14
	local row_h = (per_col and per_col.row_h) or th.row_h or 48
	local list_sp = th.list_spacing or 4

	-- Columns-Padding, so wie du es an Columns.build übergibst
	local cols_pad_t = th.cols_pad_t or 2
	local cols_pad_b = th.cols_pad_b or 2

	-- Body-Höhe = oberes Spaltenpad + Zeilen + Zwischenabstände + unteres Spaltenpad
	local body_h = cols_pad_t + (rows_max * row_h) + (math.max(rows_max - 1, 0) * list_sp) + cols_pad_b

	-- Gesamthöhe = Header + oberes Dialogpad + Body + unteres Dialogpad + Footer
	return header_h + pad_v + body_h + pad_v + footer_h
end

function M.open(theme_overrides)
	return Base.dialog({
		container = "panel",
		title = "Control Panel",
		theme = theme_overrides,
		popup = { use_backdrop = false, close_on_escape = true },
		focus = { mode = "columns", start_col = 1, mouse_follow = true },

		body_builder = function(th, dims, _get_close)
			-- 1) Items bauen (wie bisher)
			local items = {}
			for _, a in ipairs(build_actions()) do
				items[#items + 1] = {
					text = (a.emoji and (a.emoji .. " ") or "") .. (a.label or ""),
					icon = a.icon,
					on_press = a.on_press,
				}
			end

			-- 2) Spaltenanzahl & Verteilung
			local ncols = tonumber(th.control_columns) or 2
			local cols_items = (function()
				local out = {}
				for i = 1, ncols do
					out[i] = {}
				end
				for i, it in ipairs(items) do
					out[((i - 1) % ncols) + 1][#out[((i - 1) % ncols) + 1] + 1] = it
				end
				return out
			end)()

			-- 3) rows_max bestimmen
			local rows_max = 0
			for i = 1, ncols do
				rows_max = math.max(rows_max, #cols_items[i])
			end

			-- 4) Höhe berechnen und an Base.dialog zurückmelden
			local desired_h = compute_dialog_height(th, rows_max, { row_h = th.row_h or 48 })
			-- dims wird erst NACH Rückgabe von body_builder in Base gesetzt,
			-- daher geben wir die Größe via return-„Nebenkanal“ zurück:
			-- Trick: setze _G.__control_panel_height (oder pack es in th)
			th._computed_dialog_h = desired_h

			-- 5) Columns bauen (wie gehabt)
			local Columns = require("features.shell.menu.layouts.columns")

			-- wrapped lib, damit Klicks den Dialog schließen
			local function wrap_lib_for_close(lib, get_close)
				local actions = lib.actions or {}
				return {
					helpers = lib.helpers,
					actions = {
						click = function(item)
							local orig = actions.click(item)
							return function()
								local close = get_close()
								pcall(orig, close)
							end
						end,
					},
				}
			end
			local wrapped_lib = wrap_lib_for_close(require("features.shell.menu.lib"), _get_close)

			-- 2-Spalten Build (wenn du spec willst, geht’s analog)
			local api = Columns.build(cols_items[1], cols_items[2], th, {
				deps = { lib = wrapped_lib, styler = require("features.shell.menu.lib.theme") },
				left_opts = { row_h = th.row_h or 48 },
				right_opts = { row_h = th.row_h or 48 },
				pad_l = th.pad_h or 16,
				pad_r = th.pad_h or 16,
				pad_t = th.pad_v or 14,
				pad_b = th.pad_v or 14,
				spacing = th.cols_spacing or 0,
			})

			return api.widget, (api.get_focus_items and api:get_focus_items()) or {}
		end,

		-- Größe hier setzen: greift VOR Render
		size = (function()
			-- wenn body_builder schon lief, nimm berechnete Höhe; sonst Fallback
			local th = (type(theme_overrides) == "function") and (theme_overrides() or {}) or (theme_overrides or {})
			local h = rawget(th, "_computed_dialog_h")
			return h and { h = h } or nil
		end)(),
	})
end

return M
