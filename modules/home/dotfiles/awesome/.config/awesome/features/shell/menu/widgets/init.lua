local P = {}

local theme = require("features.shell.menu.lib.theme")
local rows = require("features.shell.menu.widgets.rows")
local power = require("features.shell.menu.widgets.power")
local header = require("features.shell.menu.widgets.header_content")

-- Re-exports
P.DEFAULTS = theme.DEFAULTS
P.with_defaults = theme.with_defaults
P.adjust = theme.adjust
P.resolve_text_size_number = theme.resolve_text_size_number
P.resolve_font = theme.resolve_font
P.resolve_icon_size = theme.resolve_icon_size

P.row_widget = rows.row_widget
P.list_widget = rows.list_widget

P.power_button = power.power_button
P.power_bar = power.power_bar

P.build_header_content = header.build_header_content

return P
