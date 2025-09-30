local P = {}

-- Optional: Theme-API kann injiziert werden (z. B. vom Container)
-- Falls niemand injiziert, bleibt P.theme_api = {} und Widgets nutzen ihre Fallbacks.
P.theme_api = {}

function P.setup_theme_api(api)
	P.theme_api = api or {}
end

-- Buttons namespace
local icons = require("shell.menu.widgets.icons")
local cancel = require("shell.menu.widgets.cancel")
local rows = require("shell.menu.widgets.rows")
local power = require("shell.menu.widgets.power")
local header = require("shell.menu.widgets.header_content")

P.buttons = {
	icons = icons,
	cancel = cancel,
}

-- Back-compat:
P.mk_icon_button = icons.mk_icon_button
P.mk_cancel_button = cancel.mk_cancel_button

P.row_widget = rows.row_widget
P.list_widget = rows.list_widget
P.rows = rows

P.power_button = power.power_button
P.power_bar = power.power_bar

P.mk_header_content = header.mk_header_content
P.build_user_header = header.build_user_header or header.build_header_content
P.build_header_content = P.build_user_header

return P
