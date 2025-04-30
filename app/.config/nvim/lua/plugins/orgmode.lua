return {
	'nvim-orgmode/orgmode',
	dependencies = {
		'nvim-telescope/telescope.nvim',
		'nvim-orgmode/telescope-orgmode.nvim',

		'nvim-orgmode/org-bullets.nvim',
		'hrsh7th/nvim-cmp',
	},
	event = 'VeryLazy',
	ft = { 'org' },
	config = function()
		local org_default_notes_file
		local org_agenda_files

		-- With a /data/doc, it's a personnal env
		local dirstat = vim.uv.fs_stat('/data/doc')
		if dirstat then
			org_default_notes_file = '/data/doc/todo.org'
			org_agenda_files = {
				'/data/doc/**/*',
			}
		else
			org_default_notes_file = '/data/work/todo.org'
			org_agenda_files = {
				'/data/work/**/*',
			}
		end

		local org = require('orgmode').setup({
			org_default_notes_file = org_default_notes_file,
			org_agenda_files = org_agenda_files,
			
			org_startup_indented = true, -- Use virtual indent
			org_todo_keywords = {
				'NEXT(n)',
				'TODO(t)',
				'WAIT(w@/!)',
				'|',
				'INACTIVE(i@)',
				'DELEGATED(g@)',
				'CANCELED(c@)',
				'DONE(d!)',
			},
			-- org_todo_keyword_faces = {
			-- 	WAIT = ':foreground blue :weight bold',
			-- 	DELEGATED = ':background #FFFFFF :slant italic :underline on',
			-- 	INACTIVE = ':background #000000 :foreground red', -- overrides builtin color for `TODO` keyword
			-- },
			
			-- win_split_mode = '80vsplit', -- auto, horizontal, vertical, { 'float', 0.3 }, tabnew
			-- Replace current buffer
			win_split_mode = function(buffer)
				local win = vim.api.nvim_get_current_win()
				local cur_buf = vim.api.nvim_win_get_buf(win)
			
				-- Only wipe the current buffer if it's not the same as the target one
				if cur_buf ~= buffer then
					-- Only wipe unlisted or unnamed buffers to avoid data loss
					local name = vim.api.nvim_buf_get_name(cur_buf)
					if name == '' and vim.bo[cur_buf].buftype == '' then
						vim.api.nvim_buf_delete(cur_buf, { force = true })
					else
						-- Optional: switch to an empty buffer before deleting the old one
						vim.cmd('enew')
						vim.api.nvim_buf_delete(cur_buf, { force = true })
					end
				end
				return win
			end,

			org_startup_folded = 'content', -- Only show the first two levels
			org_log_into_drawer = 'LOGBOOK',
			org_log_done = 'time',
			org_agenda_span = 'week', -- day/week/month/year/number of days
			-- org_agenda_skip_scheduled_if_done = false,
			-- org_agenda_skip_deadline_if_done = false,
			org_time_stamp_rounding_minutes = 15,

			mappings = {
				global = {
					org_agenda = { 'gA', '<prefix>a' },
					org_capture = { 'gC', '<prefix>c' },
				},
				org = {
					org_todo = { 'cit', '<prefix>t'},
					org_agenda_set_tags = { '<prefix>g'},
					-- org_agenda_set_tags = false,
					org_time_stamp = { '<prefix>i.', '<prefix>d' },
					org_change_date = { 'cid', '<prefix>D' },
					org_clock_in = { '<prefix>xi', '<prefix>i' },
					org_clock_out = { '<prefix>xo', '<prefix>x' },
					org_schedule = { '<prefix>is', '<prefix>s' },
					-- org_meta_return = '<CR>', -- Add heading, item or row (context-dependent)
					-- org_return = '<Leader><CR>',
					org_timestamp_up_day = { '+' },
					org_timestamp_down_day = { '-' },
					-- org_toggle_checkbox = '<C-Enter>',
				},
				agenda = {
					org_agenda_later = '<',
					org_agenda_earlier = '>',
					org_agenda_goto_today = { '.', 'T' },
					-- org_agenda_switch_to = '<TAB>',
					-- org_agenda_goto = '<CR>',
				},
			},
			org_agenda_custom_commands = {
				-- "o" is the shortcut that will be used in the prompt
				o = {
					description = 'Custom agenda view', -- Description shown in the prompt for the shortcut
					types = {
						{
							type = 'agenda',
							-- org_agenda_overriding_header = 'Today',
							org_agenda_span = 'day', -- can be any value as org_agenda_span
							-- show_clock_report = true, -- Doesn't work, use 'R' key instead
							-- org_agenda_skip_scheduled_if_done = false, -- Doesn't work, use match instead (if not on agenda type)
							-- org_agenda_skip_deadline_if_done = false, -- Doesn't work, use match instead (if not on agenda type)
							org_agenda_sorting_strategy = { 'todo-state-up' , 'priority-down', 'time-up', 'category-up' },
						},
						{
							type = 'agenda',
							-- org_agenda_overriding_header = 'This week',
							org_agenda_span = 'week',
							org_agenda_start_on_weekday = false, -- start at today
							org_agenda_start_day = '+1d',
							org_agenda_sorting_strategy = { 'time-up', 'priority-down', 'category-up', 'todo-state-up' },
							-- TODO: How to filter done items on a agenda list?
							-- match = 'TODO="TODO"', -- See: https://orgmode.org/manual/Matching-tags-and-properties.html
						},
						{
							type = 'tags_todo', -- Type can be agenda | tags | tags_todo
							org_agenda_overriding_header = 'To schedule',
							-- match = '+PRIORITY="A"', --Same as providing a "Match:" for tags view <leader>oa + m, See: https://orgmode.org/manual/Matching-tags-and-properties.html
							-- org_agenda_todo_ignore_deadlines = 'far', -- Ignore all deadlines that are too far in future (over org_deadline_warning_days). Possible values: all | near | far | past | future
							org_agenda_todo_ignore_scheduled = 'all',
							org_agenda_sorting_strategy = { 'clocked-up', 'todo-state-up', 'priority-down', 'category-up' },

						},
					}
				},
				d = {
					description = 'Done',
					types = {
						{
							type = 'tags', -- Type can be agenda | tags | tags_todo
							org_agenda_overriding_header = 'DONE list',
							match = 'TODO="INACTIVE"|TODO="DELEGATED"|TODO="CANCELED"|TODO="DONE"',
							org_agenda_sorting_strategy = { 'clocked-up', 'todo-state-up', 'priority-down', 'category-up' },
						},
					}
				}
			}
		})

		-- Bullets style
		require('org-bullets').setup()
		-- Completion
		require('cmp').setup({
			sources = {
			  { name = 'orgmode' }
			}
		})
		-- Telescope
		require('telescope').setup()
		require('telescope').load_extension('orgmode')
		
		-- Global key bindings
		vim.keymap.set('n', '<leader>a', function()
			-- org.agenda:agenda({ org_agenda_start_day = '-3d', show_clock_report = true })
			-- TODO: How to open a specific custom agenda?
			org.agenda:agenda({ show_clock_report = true })
		end)
		vim.keymap.set('n', '<leader>fo', require('telescope').extensions.orgmode.search_headings, { desc = 'orgmode search headings' })

		-- Custom org colors
		vim.api.nvim_set_hl(0, '@org.headline.level1', { link = 'markdownH1' })
		vim.api.nvim_set_hl(0, '@org.headline.level2', { link = 'markdownH2' })
		vim.api.nvim_set_hl(0, '@org.headline.level3', { link = 'markdownH3' })
		vim.api.nvim_set_hl(0, '@org.headline.level4', { link = 'markdownH4' })
		vim.api.nvim_set_hl(0, '@org.headline.level5', { link = 'markdownH5' })
		vim.api.nvim_set_hl(0, '@org.headline.level6', { link = 'markdownH6' })
		vim.api.nvim_set_hl(0, '@org.headline.level7', { link = 'markdownH6' })
		vim.api.nvim_set_hl(0, '@org.headline.level8', { link = 'markdownH6' })
		vim.api.nvim_set_hl(0, '@org.keyword.todo', { fg = '#9c1919', bold = true })
		vim.api.nvim_set_hl(0, '@org.keyword.done', { fg = '#779e4d', bold = true })
		vim.api.nvim_set_hl(0, '@org.keyword.plan', { fg = '#e334fa' })
		vim.api.nvim_set_hl(0, '@org.timestamp.active', { fg = '#e334fa' })
		vim.api.nvim_set_hl(0, '@org.properties', { fg = '#91866a' })
		vim.api.nvim_set_hl(0, '@org.drawer', { fg = '#8f8774' })
		vim.api.nvim_set_hl(0, '@org.table.delimiter', { fg = '#8f8774' })
		vim.api.nvim_set_hl(0, '@org.table.heading', { fg = '#a0a8b7' })
		vim.api.nvim_set_hl(0, '@org.agenda.header', { fg = '#e334fa' })
		vim.api.nvim_set_hl(0, '@org.agenda.day', { fg = '#8e979c' })
		vim.api.nvim_set_hl(0, '@org.agenda.weekend', { fg = '#77a3b8' })
		vim.api.nvim_set_hl(0, '@org.agenda.today', { fg = '#1D96D1' })
		vim.api.nvim_set_hl(0, '@org.agenda.scheduled', { link = '@markup.italic' })
		vim.api.nvim_set_hl(0, '@org.agenda.scheduled_past', { fg = '#e2b86b' })

		-- Auto refresh agenda on org file change
		vim.api.nvim_create_autocmd("BufWritePost", {
			group = vim.api.nvim_create_augroup("orgmodeAgendaAutoReload", { clear = true }),
			pattern = "*.org",
			callback = function()
				-- With a multi-split, it's probably an agenda opened, refresh
				-- TODO: how to check if a buffer is an orgmode agenda?
				local win_amount = #vim.api.nvim_tabpage_list_wins(0)
				if win_amount > 1 then
					org.agenda:redo()
				end
			end,
		})

		-- Register the transition from headline to TODO headline or viceversa
		-- https://github.com/nvim-orgmode/orgmode/issues/466#issuecomment-2706270695
		local EventManager = require("orgmode.events")
		local Date = require("orgmode.objects.date")
		EventManager.listen(EventManager.event.TodoChanged, function(event)
			---@cast event OrgTodoChangedEvent
			if event.headline then
				local current_todo, _, type = event.headline:get_todo()
				local old_todo_state = event.old_todo_state
				local now = Date.now()
	
				-- Manage the transition from headline to TODO headline or viceversa
				if current_todo == nil then
					current_todo = "None"
				end
				if old_todo_state == nil then
					old_todo_state = "None"
				end
				-- Record TODO keyword state changes in the logbook
				event.headline:add_note({
					'- State "' .. current_todo .. '" from "' .. old_todo_state .. '" [' .. now:to_string() .. "]",
				})
			end
		end)

	end,

	-- init = function()
		-- local org = require('orgmode')
		-- org.agenda:agenda({ show_clock_report = true })
	-- end,
}
