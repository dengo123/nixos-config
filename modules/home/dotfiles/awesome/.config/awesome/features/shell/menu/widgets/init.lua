-- features/shell/menu/widgets/init.lua
local P = {}

-- Theme (Utilities)
local theme = require("features.shell.menu.lib.theme")

-- Widgets (physische Bausteine)
local icons = require("features.shell.menu.widgets.icons")
local cancel = require("features.shell.menu.widgets.cancel")
local rows = require("features.shell.menu.widgets.rows")
local power = require("features.shell.menu.widgets.power")
local header = require("features.shell.menu.widgets.header_content")

-- Theme re-exports
P.DEFAULTS = theme.DEFAULTS
P.with_defaults = theme.with_defaults
P.adjust = theme.adjust
P.resolve_text_size_number = theme.resolve_text_size_number
P.resolve_font = theme.resolve_font
P.resolve_icon_size = theme.resolve_icon_size

-- Buttons namespace (neu, sauber gruppiert)
P.buttons = {
	icons = icons, -- icons.mk_icon_button(...)
	cancel = cancel, -- cancel.mk_cancel_button(...)
}

-- Back-compat (alte Call-Sites erwarten diese Top-Level-APIs)
P.mk_icon_button = icons.mk_icon_button
P.mk_cancel_button = cancel.mk_cancel_button

-- Rows / Lists (Menü)
P.row_widget = rows.row_widget
P.list_widget = rows.list_widget
-- optional: als Namespace exponieren, falls irgendwo erwartet:
P.rows = rows

-- Power (nur falls irgendwo genutzt)
P.power_button = power.power_button
P.power_bar = power.power_bar

-- Header (geteilt von Menü + Dialogs)
--  - mk_header_content(title, th)  ← Dialog-Titelzeile
--  - build_user_header(user, t)    ← Menü-Userzeile (avatar + name [+ sub])
P.mk_header_content = header.mk_header_content
P.build_user_header = header.build_user_header or header.build_header_content
-- Alias für Back-compat:
P.build_header_content = P.build_user_header

return P
