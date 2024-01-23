-- Create a command and a binding to open today's note.
function NoteOpenToday()
	local note_dir = (os.getenv('NOTE_PATH') or os.getenv('SILVUSDOC'))
	local today_note_path = note_dir .. '/journalogs/' .. os.date('%Y') .. '/' .. os.date('%m') .. '/' .. os.date('%Y-%m-%d') .. '.md'

	-- Create parents folders if needed
	if not vim.loop.fs_stat(today_note_path) then
		vim.fn.mkdir(vim.fn.fnamemodify(today_note_path, ':p:h'), 'p')
	end

	-- cd to note folder
	vim.api.nvim_set_current_dir(note_dir)

	-- Open note
	vim.cmd.edit(today_note_path)
end

-- Command to open Today's Note
vim.api.nvim_create_user_command('NoteToday', NoteOpenToday, {})

-- Global binding to open Today's Note
vim.keymap.set('n', '<C-l>', NoteOpenToday , { silent = false, desc = 'Open today\'s note' })
vim.api.nvim_create_autocmd('filetype', {
	pattern = 'netrw',
	desc = 'Bind NoteOpenToday on netrw',
	callback = function()
		local bind = function(lhs, rhs)
			vim.keymap.set('n', lhs, rhs, {remap = true, buffer = true})
		end
		-- Ctrl-l is "refresh" on netrw
		bind( '<C-l>', NoteOpenToday)
	
		-- edit new file
		-- bind('n', '%')
	
		-- rename file
		-- bind('r', 'R')
	end
})

-- Ensure that changes to buffers are saved when you navigate away from that buffer
vim.api.nvim_create_autocmd('filetype', {
	pattern = 'markdown',
	desc = 'Ensure that changes to buffers are saved when you navigate away from that buffer',
	command = 'set awa',
})
