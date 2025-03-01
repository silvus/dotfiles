return {
	{
		"mikavilpas/yazi.nvim",
		event = "VeryLazy",
		dependencies = { "folke/snacks.nvim", lazy = true },
		keys = {
			{
				"<leader>py",
				"<cmd>Yazi toggle<cr>",
				desc = "Resume the last yazi session",
			},
		},
		opts = {
			-- if you want to open yazi instead of netrw, see below for more info
			open_for_directories = true,
			floating_window_scaling_factor = 1,
			yazi_floating_window_border = "none",
			-- keymaps = {
			-- 	show_help = "<f1>",
			-- },
		},
	},
}
