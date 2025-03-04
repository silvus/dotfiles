-- Create a command and a binding to open today's note.
function NoteOpenToday()
	local note_dir = (os.getenv('NOTE_PATH') or os.getenv('SILVUSDOC'))
	local today_note_path = note_dir .. '/journalogs/' .. os.date('%Y') .. '/' .. os.date('%m') .. '/' .. os.date('%Y-%m-%d') .. '.org'

	-- Create parents folders if needed
	if not vim.loop.fs_stat(today_note_path) then
		vim.fn.mkdir(vim.fn.fnamemodify(today_note_path, ':p:h'), 'p')
	end

	-- cd to note folder
	vim.api.nvim_set_current_dir(note_dir)

	-- Open note
	vim.cmd.edit(today_note_path)
end

function NoteExportToPDF()
	-- Require: pandoc texlive-latex-base texlive-latex-recommended wkhtmltopdf
	local Job = require('plenary.job')

	local current_file_path = vim.fn.expand('%:p')
	local target_file_path = vim.fn.expand('%:r') .. '.pdf'

	Job:new({
		command = '/usr/bin/pandoc',
		args = {
			'--standalone',
			'--from=gfm+hard_line_breaks', -- GitHub-Flavored Markdown + Keep line break
			'--pdf-engine=wkhtmltopdf',
			'--template','eisvogel',
			current_file_path,
			'-o',
			target_file_path,
			'--metadata', 'title=' .. vim.fn.expand('%:t'),
			'--metadata', 'mainfont="DejaVu Serif"',
			'--metadata', 'monofont="DejaVu Sans Mono"',
		},
		cwd = '/tmp',
		on_exit = function(j, return_val)
			if return_val == 0 then
				vim.notify('Pandoc conversion complete to ' .. target_file_path, 'succes', {title = 'Pandoc'})
			else
				vim.notify('Pandoc conversion error: ' .. table.concat(j:stderr_result(), '\n'), 'error', {title = 'Pandoc'})
			end
		end,
	}):sync() -- or start()
end

-- Command to open Today's Note
vim.api.nvim_create_user_command('NoteToday', NoteOpenToday, {})
vim.api.nvim_create_user_command('NoteExportToPDF', NoteExportToPDF, {})

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

-- Insert current date
function NoteInsertDate()
	vim.api.nvim_put({ os.date("%Y-%m-%d") }, 'c', false, true)
end
vim.keymap.set('i', '<C-t>', NoteInsertDate, { silent = false, desc = 'Print current date' })
