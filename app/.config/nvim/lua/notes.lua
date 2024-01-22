function NoteOpenToday()
	-- TODO: get note dir path from en variable
	local today_note_path = '/tmp/test/' .. os.date("%Y-%m-%d") .. '.md'

	-- Create parents folders if needed
	if not vim.loop.fs_stat(today_note_path) then
		vim.fn.mkdir(vim.fn.fnamemodify(today_note_path, ":p:h"), "p")
	end

	vim.cmd.edit(today_note_path)

	-- TODO: cd to doc folder
	-- vim.api.nvim_set_current_dir(client.config.root_dir)

end

vim.keymap.set('n', '<C-l>', NoteOpenToday , { silent = true, desc = 'Open today\'s note' })
vim.api.nvim_create_user_command('NoteToday', NoteOpenToday, {})
