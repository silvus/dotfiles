return {
	{
		'rmagatti/auto-session',
		lazy = false,

		---enables autocomplete for opts
		---@module "auto-session"
		---@type AutoSession.Config
		opts = {
			auto_create = false, -- Enables/disables auto creating new session files. Can take a function that should return true/false if a new session file should be created or not
			-- suppressed_dirs = nil, -- Suppress session restore/create in certain directories
			-- allowed_dirs = nil, -- Allow session restore/create in certain directories

			-- args_allow_files_auto_save = false, -- Allow saving a session even when launched with a file argument (or multiple files/dirs). It does not load any existing session first. While you can just set this to true, you probably want to set it to a function that decides when to save a session when launched with file args. See documentation for more detail
			-- show_auto_restore_notif = true, -- Whether to show a notification when auto-restoring
		},
		keys = {
			-- Will use Telescope if installed or a vim.ui.select picker otherwise
			{ '<leader>s', '<cmd>SessionSearch<CR>', desc = 'Session search' },
			-- { '<leader>ws', '<cmd>SessionSave<CR>', desc = 'Save session' },
			-- { '<leader>wa', '<cmd>SessionToggleAutoSave<CR>', desc = 'Toggle autosave' },
		},
	},
}
