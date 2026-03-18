-- ~/.config/awesome/shell/workspaces/policies/client_policy.lua
local M = {}

local runtime_cfg = {
	kill_clients = true,
	soft_mode = "exclusive",
}

-- =========================================================================
-- Config
-- =========================================================================

function M.init(cfg)
	cfg = cfg or {}

	local tags_cfg = cfg.tags or {}
	local delete_cfg = tags_cfg.delete or {}

	if delete_cfg.kill_clients ~= nil then
		runtime_cfg.kill_clients = (delete_cfg.kill_clients == true)
	end

	if delete_cfg.soft_mode == "exclusive" or delete_cfg.soft_mode == "all" then
		runtime_cfg.soft_mode = delete_cfg.soft_mode
	end
end

-- =========================================================================
-- Internal
-- =========================================================================

local function remove_tag_from_client(c, t)
	local keep = {}

	for _, tagx in ipairs(c:tags() or {}) do
		if tagx ~= t then
			table.insert(keep, tagx)
		end
	end

	c:tags(keep)
end

local function kill_or_detag_client(c, t, force)
	if force == true then
		c:kill()
		return
	end

	if runtime_cfg.soft_mode == "all" then
		c:kill()
		return
	end

	local ctags = c:tags() or {}

	if #ctags <= 1 then
		c:kill()
		return
	end

	remove_tag_from_client(c, t)
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.kill_clients_in_tag(t, force)
	if not t or runtime_cfg.kill_clients ~= true then
		return
	end

	for _, c in ipairs(t:clients()) do
		kill_or_detag_client(c, t, force)
	end
end

return M
