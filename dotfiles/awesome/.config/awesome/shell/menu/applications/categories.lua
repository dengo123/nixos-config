-- ~/.config/awesome/shell/menu/applications/categories.lua
local M = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function normalize_category_name(name)
	if type(name) ~= "string" or name == "" then
		return nil
	end

	local map = {
		AudioVideo = "Multimedia",
		Audio = "Multimedia",
		Video = "Multimedia",
		Player = "Multimedia",
		Recorder = "Multimedia",

		Network = "Internet",
		WebBrowser = "Internet",
		Email = "Internet",
		FileTransfer = "Internet",
		Chat = "Internet",
		IRCClient = "Internet",

		Office = "Office",
		WordProcessor = "Office",
		Spreadsheet = "Office",
		Presentation = "Office",
		Viewer = "Office",
		PDFViewer = "Office",

		Graphics = "Graphics",
		Photography = "Graphics",
		VectorGraphics = "Graphics",
		RasterGraphics = "Graphics",
		Scanning = "Graphics",

		Game = "Games",

		Settings = "System",
		System = "System",
		FileManager = "System",
		PackageManager = "System",

		Development = "Development",
		IDE = "Development",
		TextEditor = "Development",
		RevisionControl = "Development",

		Science = "Science",
		Education = "Education",

		Utility = "Accessories",
		Accessories = "Accessories",
	}

	return map[name]
end

local function categories_field(entry)
	if type(entry) ~= "table" then
		return nil
	end

	return entry.categories or entry.Categories
end

local function iter_categories(value)
	local out = {}

	if type(value) == "table" then
		for _, v in ipairs(value) do
			if type(v) == "string" and v ~= "" then
				table.insert(out, v)
			end
		end
	elseif type(value) == "string" and value ~= "" then
		for cat in value:gmatch("([^;]+)") do
			if cat ~= "" then
				table.insert(out, cat)
			end
		end
	end

	return out
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.resolve(entry)
	if type(entry) ~= "table" then
		return "Other"
	end

	if entry.show_terminal == true or entry.terminal == true or entry.Terminal == true then
		return "Terminal"
	end

	for _, cat in ipairs(iter_categories(categories_field(entry))) do
		local normalized = normalize_category_name(cat)
		if normalized then
			return normalized
		end
	end

	return "Other"
end

return M
