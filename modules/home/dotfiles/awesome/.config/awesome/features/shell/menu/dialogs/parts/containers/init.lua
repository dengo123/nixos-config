-- Container-Registry: wählt den Frame-Typ
local Containers = {}

function Containers.build(kind, th, dims, widgets)
	-- widgets = { title, body, cancel_btn, header_h, footer_h }
	if kind == "panel" then
		return require("features.shell.menu.dialogs.parts.containers.panel").build(th, dims, widgets)
	else -- default
		return require("features.shell.menu.dialogs.parts.containers.power").build(th, dims, widgets)
	end
end

return Containers
