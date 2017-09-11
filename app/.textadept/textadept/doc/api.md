<a id="_G"></a>
# The `_G` Module

- - -

Extends Lua's _G table to provide extra functions and fields for Textadept.

## Fields defined by `_G`

<a id="BSD"></a>
### `BSD` (bool)

Whether or not Textadept is running on BSD.

<a id="CURSES"></a>
### `CURSES` (bool)

Whether or not Textadept is running in the terminal.
  Curses feature incompatibilities are listed in the [Appendix][].

  [Appendix]: manual.html#Curses.Compatibility

<a id="LINUX"></a>
### `LINUX` (bool)

Whether or not Textadept is running on Linux.

<a id="OSX"></a>
### `OSX` (bool)

Whether or not Textadept is running on Mac OSX as a GUI application.

<a id="WIN32"></a>
### `WIN32` (bool)

Whether or not Textadept is running on Windows.

<a id="_CHARSET"></a>
### `_CHARSET` (string)

The filesystem's character encoding.
  This is used when [working with files](#io).

<a id="_HOME"></a>
### `_HOME` (string)

The path to Textadept's home, or installation, directory.

<a id="_RELEASE"></a>
### `_RELEASE` (string)

The Textadept release version string.

<a id="_USERHOME"></a>
### `_USERHOME` (string)

The path to the user's *~/.textadept/* directory, where all preferences and
  user-data is stored.
  On Windows machines *~/* is the value of the "USERHOME" environment
  variable (typically *C:\Users\username\\* or
  *C:\Documents and Settings\username\\*). On Linux, BSD, and Mac OSX
  machines *~/* is the value of "$HOME" (typically */home/username/* and
  */Users/username/* respectively).


## Functions defined by `_G`

<a id="quit"></a>
### `quit`()

Emits a `QUIT` event, and unless any handler returns `false`, quits
Textadept.

See also:

* [`events.QUIT`](#events.QUIT)

<a id="reset"></a>
### `reset`()

Resets the Lua State by reloading all initialization scripts.
Language modules for opened files are NOT reloaded. Re-opening the files that
use them will reload those modules instead.
This function is useful for modifying user scripts (such as
*~/.textadept/init.lua* and *~/.textadept/modules/textadept/keys.lua*) on
the fly without having to restart Textadept. `arg` is set to `nil` when
reinitializing the Lua State. Any scripts that need to differentiate between
startup and reset can test `arg`.

<a id="spawn"></a>
### `spawn`(*argv, cwd, env, stdout\_cb, stderr\_cb, exit\_cb*)

Spawns an interactive child process *argv* in a separate thread, returning
a handle to that process.
On Windows, *argv* is passed to `cmd.exe`: `%COMSPEC% /c [argv]`.
At the moment, only the Windows terminal version spawns processes in the same
thread.

Parameters:

* *`argv`*: A command line string that contains the program's name followed
  by arguments to pass to it. `PATH` is searched for program names.
* *`cwd`*: Optional current working directory (cwd) for the child
  process. The default value is `nil`, which inherits the parent's cwd.
* *`env`*: Optional list of environment variables for the child process.
  Each element in the list is a 'KEY=VALUE' string. The default value is
  `nil`, which inherits the parent's environment.
  This parameter should be omitted completely instead of specifying `nil`.
* *`stdout_cb`*: Optional Lua function that accepts a string parameter for a
  block of standard output read from the child. Stdout is read asynchronously
  in 1KB or 0.5KB blocks (depending on the platform), or however much data is
  available at the time.
  At the moment, only the Win32 terminal version sends all output, whether it
  be stdout or stderr, to this callback after the process finishes.
* *`stderr_cb`*: Optional Lua function that accepts a string parameter for a
  block of standard error read from the child. Stderr is read asynchronously
  in 1KB or 0.5kB blocks (depending on the platform), or however much data is
  available at the time.
* *`exit_cb`*: Optional Lua function that is called when the child process
  finishes. The child's exit status is passed.

Usage:

* `spawn('lua buffer.filename', nil, print)`
* `proc = spawn('lua -e "print(io.read())"', nil, print)
       proc:write('foo\n')`

Return:

* proc or nil plus an error message on failure

<a id="spawn_proc:close"></a>
### `spawn_proc:close`()

Closes standard input for process *spawn_proc*, effectively sending an EOF
(end of file) to it.

<a id="spawn_proc:kill"></a>
### `spawn_proc:kill`(*signal*)

Kills running process *spawn_proc*, or sends it Unix signal *signal*.

Parameters:

* *`signal`*: Optional Unix signal to send to *spawn_proc*. The default value
  is 9 (`SIGKILL`), which kills the process.

<a id="spawn_proc:read"></a>
### `spawn_proc:read`(*arg*)

Reads and returns stdout from process *spawn_proc*, according to string
format or number *arg*.
Similar to Lua's `io.read()` and blocks for input. *spawn_proc* must still be
running. If an error occurs while reading, returns `nil`, an error code, and
an error message.
Ensure any read operations read all stdout available, as the stdout callback
function passed to `spawn()` will not be called until the stdout buffer is
clear.

Parameters:

* *`arg`*: Optional argument similar to those in Lua's `io.read()`, but "n"
  is not supported. The default value is "l", which reads a line.

Return:

* string of bytes read

<a id="spawn_proc:status"></a>
### `spawn_proc:status`()

Returns the status of process *spawn_proc*, which is either "running" or
"terminated".

Return:

* "running" or "terminated"

<a id="spawn_proc:wait"></a>
### `spawn_proc:wait`()

Blocks until process *spawn_proc* finishes.

<a id="spawn_proc:write"></a>
### `spawn_proc:write`(*...*)

Writes string input to the stdin of process *spawn_proc*.
Note: On Linux, if more than 65536 bytes (64K) are to be written, it is
possible those bytes need to be written in 65536-byte (64K) chunks, or the
process may not receive all input. However, it is also possible that there is
a limit on how many bytes can be written in a short period of time, perhaps
196608 bytes (192K).

Parameters:

* *`...`*: Standard input for *spawn_proc*.

<a id="timeout"></a>
### `timeout`(*interval, f, ...*)

Calls function *f* with the given arguments after *interval* seconds.
If *f* returns `true`, calls *f* repeatedly every *interval* seconds as long
as *f* returns `true`. A `nil` or `false` return value stops repetition.

Parameters:

* *`interval`*: The interval in seconds to call *f* after.
* *`f`*: The function to call.
* *`...`*: Additional arguments to pass to *f*.


## Tables defined by `_G`

<a id="_BUFFERS"></a>
### `_BUFFERS`

Table of all open buffers in Textadept.
Numeric keys have buffer values and buffer keys have their associated numeric
keys.

Usage:

* `_BUFFERS[n]      --> buffer at index n`
* `_BUFFERS[buffer] --> index of buffer in _BUFFERS`

See also:

* [`_G.buffer`](#_G.buffer)

<a id="_VIEWS"></a>
### `_VIEWS`

Table of all views in Textadept.
Numeric keys have view values and view keys have their associated numeric
keys.

Usage:

* `_VIEWS[n]    --> view at index n`
* `_VIEWS[view] --> index of view in _VIEWS`

See also:

* [`_G.view`](#_G.view)

<a id="arg"></a>
### `arg`

Table of command line parameters passed to Textadept.

See also:

* [`args`](#args)

<a id="_G.buffer"></a>
### `_G.buffer`

The current [buffer](#buffer) in the [current view](#_G.view).

<a id="_G.view"></a>
### `_G.view`

The current [view](#view).

- - -

<a id="_L"></a>
# The `_L` Module

- - -

Map of all messages used by Textadept to their localized form.
If the table does not contain the localized version of a given message, it
returns a string that starts with "No Localization:" via a metamethod.

- - -

<a id="_M"></a>
# The `_M` Module

- - -

A table of loaded Textadept language modules.

Language modules are a special kind of module that Textadept automatically
loads when editing source code in a particular programming language. The only
thing "special" about them is they are named after a lexer. Otherwise they
are plain Lua modules. The *~/.textadept/modules/* directory houses language
modules (along with other modules).

A language module is designed to provide extra functionality for a single
programming language. Some examples of what language modules can do:

  * Specify block comment syntax for lines of code
  * Define compile and run commands for source files
  * Set language-specific editor properties like indentation rules
  * Specify code autocompletion routines
  * Declare snippets
  * Define commands and key bindings for them
  * Add to the top-level menu or right-click editor context menu

Examples of these features are described in the sections below.


<a id="_M.Block.Comment"></a>

## Block Comment

Many languages have different syntaxes for single line comments and
multi-line comments in source code. Textadept's block comment feature only
uses one of those syntaxes for a given language. If you prefer the other
syntax, or if Textadept does not support block comments for a particular
language, modify the [`textadept.editing.comment_string`](#textadept.editing.comment_string) table. For
example:

    textadept.editing.comment_string.ansi_c = '//' -- change from /* ... */


<a id="_M.Compile.and.Run"></a>

## Compile and Run

Textadept knows most of the commands that compile and/or run code in source
files. However, it does not know all of them, and the ones that it does know
may not be completely accurate in all cases. Compile and run commands are
read from the [`textadept.run.compile_commands`](#textadept.run.compile_commands) and
[`textadept.run.run_commands`](#textadept.run.run_commands) tables using the appropriate lexer key, and
thus can be defined or modified. For Lua, it would look like:

    textadept.run.compile_commands.lua = 'luac "%f"'
    textadept.run.run_commands.lua = 'lua "%f"'

Double-clicking on compile or runtime errors jumps to the error's location.
If Textadept does not recognize your language's errors properly, add an error
pattern to [`textadept.run.error_patterns`](#textadept.run.error_patterns). The Lua error pattern looks
like:

    local patterns = textadept.run.error_patterns
    if not patterns.lua then patterns.lua = {} end
    patterns.lua[#patterns.lua + 1] = '^luac?: (.-):(%d+): (.+)$'


<a id="_M.Buffer.Properties"></a>

## Buffer Properties

By default, Textadept uses 2 spaces for indentation. Some languages have
different indentation guidelines, however. As described in the manual, use
`events.LEXER_LOADED` to change this and any other language-specific editor
properties. For example:

    events.connect(events.LEXER_LOADED, function(lexer)
      if lexer == 'python' then
        buffer.tab_width = 4
        buffer.use_tabs = false
        buffer.view_ws = buffer.WS_VISIBLEALWAYS
      end
    end


<a id="_M.Autocompletion.and.Documentation"></a>

## Autocompletion and Documentation

Textadept has the capability to autocomplete symbols for programming
languages and display API documentation. In order for these to work for a
given language, an [autocompleter](#textadept.editing.autocompleters) and
[API file(s)](#textadept.editing.api_files) must exist. All of Textadept's
included language modules have examples of autocompleters and API
documentation, as well as most of its officially supported language modules.


<a id="_M.Snippets"></a>

## Snippets

[Snippets](#textadept.snippets) for common language constructs are useful.
Some snippets for common Lua control structures look like this:

    snippets.lua = {
      f = "function %1(name)(%2(args))\n\t%0\nend",
      ['for'] = "for i = %1(1), %2(10)%3(, -1) do\n\t%0\nend",
      fori = "for %1(i), %2(val) in ipairs(%3(table)) do\n\t%0\nend",
      forp = "for %1(k), %2(v) in pairs(%3(table)) do\n\t%0\nend",
    }


<a id="_M.Commands"></a>

## Commands

Additional editing features for the language can be useful. For example, the
[C](#_M.ansi_c) module has a feature to add a ';' to the end of the current
line and insert a new line. This command is bound to the `Shift+Enter` (`⇧↩`
on Mac OSX | `S-Enter` in curses) key for easy access:

    keys.ansi_c = {
      ['s\n'] = function()
        buffer:line_end()
        buffer:add_text(';')
        buffer:new_line()
      end
    }

When defining key bindings for other commands, you may make use of a `Ctrl+L`
(`⌘L` on Mac OSX | `M-L` in curses) keychain. Traditionally this prefix has
been reserved for use by language modules (although neither Textadept nor its
modules utilize it at the moment). Users may define this keychain for new or
existing modules and it will not conflict with any default key bindings.
For example:

    keys.lua[not OSX and not CURSES and 'cl' or 'ml'] = {
      ...
    }


<a id="_M.Menus"></a>

## Menus

It may be useful to add language-specific menu options to the top-level menu
and/or right-click context menu in order to access module features without
using key bindings. For example:

    local lua_menu = {
      title = 'Lua',
      {'Item 1', function() ... end},
      {'Item 2', function() ... end}
    }
    local tools = textadept.menu.menubar[_L['_Tools']]
    tools[#tools + 1] = lua_menu
    textadept.menu.context_menu[#textadept.menu.context_menu + 1] = lua_menu

- - -

<a id="_M.ansi_c"></a>
# The `_M.ansi_c` Module

- - -

The ansi_c module.
It provides utilities for editing C code.

## Tables defined by `_M.ansi_c`

<a id="keys.ansi_c"></a>
### `keys.ansi_c`

Table of C-specific key bindings.

+ `Shift+Enter` (`⇧↩` | `S-Enter`)
  Add ';' to the end of the current line and insert a newline.

<a id="snippets.ansi_c"></a>
### `snippets.ansi_c`

Table of C-specific snippets.

<a id="_M.ansi_c.tags"></a>
### `_M.ansi_c.tags`

List of ctags files to use for autocompletion in addition to the current
project's top-level *tags* file or the current directory's *tags* file.

- - -

<a id="_M.lua"></a>
# The `_M.lua` Module

- - -

The lua module.
It provides utilities for editing Lua code.

## Tables defined by `_M.lua`

<a id="keys.lua"></a>
### `keys.lua`

Container for Lua-specific key bindings.

<a id="snippets.lua"></a>
### `snippets.lua`

Container for Lua-specific snippets.

<a id="_M.lua.expr_types"></a>
### `_M.lua.expr_types`

Map of expression patterns to their types.
Used for type-hinting when showing autocompletions for variables.
Expressions are expected to match after the '=' sign of a statement.

Usage:

* `_M.lua.expr_types['^spawn%b()%s*$'] = 'proc'`

<a id="_M.lua.tags"></a>
### `_M.lua.tags`

List of "fake" ctags files to use for autocompletion.
The kind 'm' is recognized as a module, 'f' as a function, 't' as a table and
'F' as a module or table field.
The *modules/lua/tadoc.lua* script can generate *tags* and
[*api*](#textadept.editing.api_files) files for Lua modules via LuaDoc.

- - -

<a id="_SCINTILLA"></a>
# The `_SCINTILLA` Module

- - -

Scintilla constants, functions, and properties.
Do not modify anything in this module. Doing so will have unpredictable
consequences.

## Functions defined by `_SCINTILLA`

<a id="_SCINTILLA.next_image_type"></a>
### `_SCINTILLA.next_image_type`()

Returns a unique image type identier number for use with
`buffer.register_image()` and `buffer.register_rgba_image()`.
Use this function for custom image types in order to prevent clashes with
identifiers of other custom image types.

Usage:

* `local image_type = _SCINTILLA.next_image_type()`

See also:

* [`buffer.register_image`](#buffer.register_image)
* [`buffer.register_rgba_image`](#buffer.register_rgba_image)

<a id="_SCINTILLA.next_indic_number"></a>
### `_SCINTILLA.next_indic_number`()

Returns a unique indicator number for use with custom indicators.
Use this function for custom indicators in order to prevent clashes with
identifiers of other custom indicators.

Usage:

* `local indic_num = _SCINTILLA.next_indic_number()`

See also:

* [`buffer.indic_style`](#buffer.indic_style)

<a id="_SCINTILLA.next_marker_number"></a>
### `_SCINTILLA.next_marker_number`()

Returns a unique marker number for use with `buffer.marker_define()`.
Use this function for custom markers in order to prevent clashes with
identifiers of other custom markers.

Usage:

* `local marknum = _SCINTILLA.next_marker_number()`

See also:

* [`buffer.marker_define`](#buffer.marker_define)

<a id="_SCINTILLA.next_user_list_type"></a>
### `_SCINTILLA.next_user_list_type`()

Returns a unique user list identier number for use with
`buffer.user_list_show()`.
Use this function for custom user lists in order to prevent clashes with
list identifiers of other custom user lists.

Usage:

* `local list_type = _SCINTILLA.next_user_list_type()`

See also:

* [`buffer.user_list_show`](#buffer.user_list_show)


## Tables defined by `_SCINTILLA`

<a id="_SCINTILLA.constants"></a>
### `_SCINTILLA.constants`

Map of Scintilla constant names to their numeric values.

See also:

* [`buffer`](#buffer)

<a id="_SCINTILLA.functions"></a>
### `_SCINTILLA.functions`

Map of Scintilla function names to tables containing their IDs, return types,
wParam types, and lParam types. Types are as follows:

  + `0`: Void.
  + `1`: Integer.
  + `2`: Length of the given lParam string.
  + `3`: Integer position.
  + `4`: Color, in "0xBBGGRR" format.
  + `5`: Boolean `true` or `false`.
  + `6`: Bitmask of Scintilla key modifiers and a key value.
  + `7`: String parameter.
  + `8`: String return value.

<a id="_SCINTILLA.properties"></a>
### `_SCINTILLA.properties`

Map of Scintilla property names to table values containing their "get"
function IDs, "set" function IDs, return types, and wParam types.
The wParam type will be non-zero if the property is indexable.
Types are the same as in the `functions` table.

See also:

* [`_SCINTILLA.functions`](#_SCINTILLA.functions)

- - -

<a id="args"></a>
# The `args` Module

- - -

Processes command line arguments for Textadept.

## Fields defined by `args`

<a id="events.ARG_NONE"></a>
### `events.ARG_NONE` (string)

Emitted when no command line arguments are passed to Textadept on startup.


## Functions defined by `args`

<a id="args.register"></a>
### `args.register`(*short, long, narg, f, description*)

Registers a command line switch with short and long versions *short* and
*long*, respectively. *narg* is the number of arguments the switch accepts,
*f* is the function called when the switch is tripped, and *description* is
the switch's description when displaying help.

Parameters:

* *`short`*: The string short version of the switch.
* *`long`*: The string long version of the switch.
* *`narg`*: The number of expected parameters for the switch.
* *`f`*: The Lua function to run when the switch is tripped.
* *`description`*: The string description of the switch for command line
  help.


- - -

<a id="buffer"></a>
# The `buffer` Module

- - -

A Textadept buffer object.
Constants are documented in the fields they apply to.
While you can work with individual buffer instances, it is really only useful
to work with the global one.
Many of these functions and fields are derived from the Scintilla editing
component, and additional information can be found on the Scintilla website:
[http://scintilla.org/ScintillaDoc.html](
http://scintilla.org/ScintillaDoc.html)

## Fields defined by `buffer`

<a id="buffer.ALPHA_NOALPHA"></a>
### `buffer.ALPHA_NOALPHA` (number, Read-only)




<a id="buffer.ALPHA_OPAQUE"></a>
### `buffer.ALPHA_OPAQUE` (number, Read-only)




<a id="buffer.ALPHA_TRANSPARENT"></a>
### `buffer.ALPHA_TRANSPARENT` (number, Read-only)




<a id="buffer.ANNOTATION_BOXED"></a>
### `buffer.ANNOTATION_BOXED` (number, Read-only)




<a id="buffer.ANNOTATION_HIDDEN"></a>
### `buffer.ANNOTATION_HIDDEN` (number, Read-only)




<a id="buffer.ANNOTATION_INDENTED"></a>
### `buffer.ANNOTATION_INDENTED` (number, Read-only)




<a id="buffer.ANNOTATION_STANDARD"></a>
### `buffer.ANNOTATION_STANDARD` (number, Read-only)




<a id="buffer.AUTOMATICFOLD_CHANGE"></a>
### `buffer.AUTOMATICFOLD_CHANGE` (number, Read-only)




<a id="buffer.AUTOMATICFOLD_CLICK"></a>
### `buffer.AUTOMATICFOLD_CLICK` (number, Read-only)




<a id="buffer.AUTOMATICFOLD_SHOW"></a>
### `buffer.AUTOMATICFOLD_SHOW` (number, Read-only)




<a id="buffer.CARETSTICKY_OFF"></a>
### `buffer.CARETSTICKY_OFF` (number, Read-only)




<a id="buffer.CARETSTICKY_ON"></a>
### `buffer.CARETSTICKY_ON` (number, Read-only)




<a id="buffer.CARETSTICKY_WHITESPACE"></a>
### `buffer.CARETSTICKY_WHITESPACE` (number, Read-only)




<a id="buffer.CARETSTYLE_BLOCK"></a>
### `buffer.CARETSTYLE_BLOCK` (number, Read-only)




<a id="buffer.CARETSTYLE_INVISIBLE"></a>
### `buffer.CARETSTYLE_INVISIBLE` (number, Read-only)




<a id="buffer.CARETSTYLE_LINE"></a>
### `buffer.CARETSTYLE_LINE` (number, Read-only)




<a id="buffer.CARET_EVEN"></a>
### `buffer.CARET_EVEN` (number, Read-only)




<a id="buffer.CARET_JUMPS"></a>
### `buffer.CARET_JUMPS` (number, Read-only)




<a id="buffer.CARET_SLOP"></a>
### `buffer.CARET_SLOP` (number, Read-only)




<a id="buffer.CARET_STRICT"></a>
### `buffer.CARET_STRICT` (number, Read-only)




<a id="buffer.CASEINSENSITIVEBEHAVIOUR_IGNORECASE"></a>
### `buffer.CASEINSENSITIVEBEHAVIOUR_IGNORECASE` (number, Read-only)




<a id="buffer.CASEINSENSITIVEBEHAVIOUR_RESPECTCASE"></a>
### `buffer.CASEINSENSITIVEBEHAVIOUR_RESPECTCASE` (number, Read-only)




<a id="buffer.CASE_LOWER"></a>
### `buffer.CASE_LOWER` (number, Read-only)




<a id="buffer.CASE_MIXED"></a>
### `buffer.CASE_MIXED` (number, Read-only)




<a id="buffer.CASE_UPPER"></a>
### `buffer.CASE_UPPER` (number, Read-only)




<a id="buffer.CURSORARROW"></a>
### `buffer.CURSORARROW` (number, Read-only)




<a id="buffer.CURSORNORMAL"></a>
### `buffer.CURSORNORMAL` (number, Read-only)




<a id="buffer.CURSORREVERSEARROW"></a>
### `buffer.CURSORREVERSEARROW` (number, Read-only)




<a id="buffer.CURSORWAIT"></a>
### `buffer.CURSORWAIT` (number, Read-only)




<a id="buffer.EDGE_BACKGROUND"></a>
### `buffer.EDGE_BACKGROUND` (number, Read-only)




<a id="buffer.EDGE_LINE"></a>
### `buffer.EDGE_LINE` (number, Read-only)




<a id="buffer.EDGE_MULTILINE"></a>
### `buffer.EDGE_MULTILINE` (number, Read-only)




<a id="buffer.EDGE_NONE"></a>
### `buffer.EDGE_NONE` (number, Read-only)




<a id="buffer.EOL_CR"></a>
### `buffer.EOL_CR` (number, Read-only)




<a id="buffer.EOL_CRLF"></a>
### `buffer.EOL_CRLF` (number, Read-only)




<a id="buffer.EOL_LF"></a>
### `buffer.EOL_LF` (number, Read-only)




<a id="buffer.FIND_MATCHCASE"></a>
### `buffer.FIND_MATCHCASE` (number, Read-only)




<a id="buffer.FIND_REGEXP"></a>
### `buffer.FIND_REGEXP` (number, Read-only)




<a id="buffer.FIND_WHOLEWORD"></a>
### `buffer.FIND_WHOLEWORD` (number, Read-only)




<a id="buffer.FIND_WORDSTART"></a>
### `buffer.FIND_WORDSTART` (number, Read-only)




<a id="buffer.FOLDACTION_CONTRACT"></a>
### `buffer.FOLDACTION_CONTRACT` (number, Read-only)




<a id="buffer.FOLDACTION_EXPAND"></a>
### `buffer.FOLDACTION_EXPAND` (number, Read-only)




<a id="buffer.FOLDACTION_TOGGLE"></a>
### `buffer.FOLDACTION_TOGGLE` (number, Read-only)




<a id="buffer.FOLDFLAG_LEVELNUMBERS"></a>
### `buffer.FOLDFLAG_LEVELNUMBERS` (number, Read-only)




<a id="buffer.FOLDFLAG_LINEAFTER_CONTRACTED"></a>
### `buffer.FOLDFLAG_LINEAFTER_CONTRACTED` (number, Read-only)




<a id="buffer.FOLDFLAG_LINEAFTER_EXPANDED"></a>
### `buffer.FOLDFLAG_LINEAFTER_EXPANDED` (number, Read-only)




<a id="buffer.FOLDFLAG_LINEBEFORE_CONTRACTED"></a>
### `buffer.FOLDFLAG_LINEBEFORE_CONTRACTED` (number, Read-only)




<a id="buffer.FOLDFLAG_LINEBEFORE_EXPANDED"></a>
### `buffer.FOLDFLAG_LINEBEFORE_EXPANDED` (number, Read-only)




<a id="buffer.FOLDFLAG_LINESTATE"></a>
### `buffer.FOLDFLAG_LINESTATE` (number, Read-only)




<a id="buffer.FOLDLEVELBASE"></a>
### `buffer.FOLDLEVELBASE` (number, Read-only)




<a id="buffer.FOLDLEVELHEADERFLAG"></a>
### `buffer.FOLDLEVELHEADERFLAG` (number, Read-only)




<a id="buffer.FOLDLEVELNUMBERMASK"></a>
### `buffer.FOLDLEVELNUMBERMASK` (number, Read-only)




<a id="buffer.FOLDLEVELWHITEFLAG"></a>
### `buffer.FOLDLEVELWHITEFLAG` (number, Read-only)




<a id="buffer.INDIC_BOX"></a>
### `buffer.INDIC_BOX` (number, Read-only)




<a id="buffer.INDIC_COMPOSITIONTHICK"></a>
### `buffer.INDIC_COMPOSITIONTHICK` (number, Read-only)




<a id="buffer.INDIC_COMPOSITIONTHIN"></a>
### `buffer.INDIC_COMPOSITIONTHIN` (number, Read-only)




<a id="buffer.INDIC_DASH"></a>
### `buffer.INDIC_DASH` (number, Read-only)




<a id="buffer.INDIC_DIAGONAL"></a>
### `buffer.INDIC_DIAGONAL` (number, Read-only)




<a id="buffer.INDIC_DOTBOX"></a>
### `buffer.INDIC_DOTBOX` (number, Read-only)




<a id="buffer.INDIC_DOTS"></a>
### `buffer.INDIC_DOTS` (number, Read-only)




<a id="buffer.INDIC_FULLBOX"></a>
### `buffer.INDIC_FULLBOX` (number, Read-only)




<a id="buffer.INDIC_HIDDEN"></a>
### `buffer.INDIC_HIDDEN` (number, Read-only)




<a id="buffer.INDIC_MAX"></a>
### `buffer.INDIC_MAX` (number, Read-only)




<a id="buffer.INDIC_PLAIN"></a>
### `buffer.INDIC_PLAIN` (number, Read-only)




<a id="buffer.INDIC_ROUNDBOX"></a>
### `buffer.INDIC_ROUNDBOX` (number, Read-only)




<a id="buffer.INDIC_SQUIGGLE"></a>
### `buffer.INDIC_SQUIGGLE` (number, Read-only)




<a id="buffer.INDIC_SQUIGGLELOW"></a>
### `buffer.INDIC_SQUIGGLELOW` (number, Read-only)




<a id="buffer.INDIC_SQUIGGLEPIXMAP"></a>
### `buffer.INDIC_SQUIGGLEPIXMAP` (number, Read-only)




<a id="buffer.INDIC_STRAIGHTBOX"></a>
### `buffer.INDIC_STRAIGHTBOX` (number, Read-only)




<a id="buffer.INDIC_STRIKE"></a>
### `buffer.INDIC_STRIKE` (number, Read-only)




<a id="buffer.INDIC_TEXTFORE"></a>
### `buffer.INDIC_TEXTFORE` (number, Read-only)




<a id="buffer.INDIC_TT"></a>
### `buffer.INDIC_TT` (number, Read-only)




<a id="buffer.IV_LOOKBOTH"></a>
### `buffer.IV_LOOKBOTH` (number, Read-only)




<a id="buffer.IV_LOOKFORWARD"></a>
### `buffer.IV_LOOKFORWARD` (number, Read-only)




<a id="buffer.IV_NONE"></a>
### `buffer.IV_NONE` (number, Read-only)




<a id="buffer.IV_REAL"></a>
### `buffer.IV_REAL` (number, Read-only)




<a id="buffer.MARGINOPTION_NONE"></a>
### `buffer.MARGINOPTION_NONE` (number, Read-only)




<a id="buffer.MARGINOPTION_SUBLINESELECT"></a>
### `buffer.MARGINOPTION_SUBLINESELECT` (number, Read-only)




<a id="buffer.MARGIN_BACK"></a>
### `buffer.MARGIN_BACK` (number, Read-only)




<a id="buffer.MARGIN_COLOUR"></a>
### `buffer.MARGIN_COLOUR` (number, Read-only)




<a id="buffer.MARGIN_FORE"></a>
### `buffer.MARGIN_FORE` (number, Read-only)




<a id="buffer.MARGIN_NUMBER"></a>
### `buffer.MARGIN_NUMBER` (number, Read-only)




<a id="buffer.MARGIN_RTEXT"></a>
### `buffer.MARGIN_RTEXT` (number, Read-only)




<a id="buffer.MARGIN_SYMBOL"></a>
### `buffer.MARGIN_SYMBOL` (number, Read-only)




<a id="buffer.MARGIN_TEXT"></a>
### `buffer.MARGIN_TEXT` (number, Read-only)




<a id="buffer.MARKER_MAX"></a>
### `buffer.MARKER_MAX` (number, Read-only)




<a id="buffer.MARKNUM_FOLDER"></a>
### `buffer.MARKNUM_FOLDER` (number, Read-only)




<a id="buffer.MARKNUM_FOLDEREND"></a>
### `buffer.MARKNUM_FOLDEREND` (number, Read-only)




<a id="buffer.MARKNUM_FOLDERMIDTAIL"></a>
### `buffer.MARKNUM_FOLDERMIDTAIL` (number, Read-only)




<a id="buffer.MARKNUM_FOLDEROPEN"></a>
### `buffer.MARKNUM_FOLDEROPEN` (number, Read-only)




<a id="buffer.MARKNUM_FOLDEROPENMID"></a>
### `buffer.MARKNUM_FOLDEROPENMID` (number, Read-only)




<a id="buffer.MARKNUM_FOLDERSUB"></a>
### `buffer.MARKNUM_FOLDERSUB` (number, Read-only)




<a id="buffer.MARKNUM_FOLDERTAIL"></a>
### `buffer.MARKNUM_FOLDERTAIL` (number, Read-only)




<a id="buffer.MARK_ARROW"></a>
### `buffer.MARK_ARROW` (number, Read-only)




<a id="buffer.MARK_ARROWDOWN"></a>
### `buffer.MARK_ARROWDOWN` (number, Read-only)




<a id="buffer.MARK_ARROWS"></a>
### `buffer.MARK_ARROWS` (number, Read-only)




<a id="buffer.MARK_AVAILABLE"></a>
### `buffer.MARK_AVAILABLE` (number, Read-only)




<a id="buffer.MARK_BACKGROUND"></a>
### `buffer.MARK_BACKGROUND` (number, Read-only)




<a id="buffer.MARK_BOOKMARK"></a>
### `buffer.MARK_BOOKMARK` (number, Read-only)




<a id="buffer.MARK_BOXMINUS"></a>
### `buffer.MARK_BOXMINUS` (number, Read-only)




<a id="buffer.MARK_BOXMINUSCONNECTED"></a>
### `buffer.MARK_BOXMINUSCONNECTED` (number, Read-only)




<a id="buffer.MARK_BOXPLUS"></a>
### `buffer.MARK_BOXPLUS` (number, Read-only)




<a id="buffer.MARK_BOXPLUSCONNECTED"></a>
### `buffer.MARK_BOXPLUSCONNECTED` (number, Read-only)




<a id="buffer.MARK_CHARACTER"></a>
### `buffer.MARK_CHARACTER` (number, Read-only)




<a id="buffer.MARK_CIRCLE"></a>
### `buffer.MARK_CIRCLE` (number, Read-only)




<a id="buffer.MARK_CIRCLEMINUS"></a>
### `buffer.MARK_CIRCLEMINUS` (number, Read-only)




<a id="buffer.MARK_CIRCLEMINUSCONNECTED"></a>
### `buffer.MARK_CIRCLEMINUSCONNECTED` (number, Read-only)




<a id="buffer.MARK_CIRCLEPLUS"></a>
### `buffer.MARK_CIRCLEPLUS` (number, Read-only)




<a id="buffer.MARK_CIRCLEPLUSCONNECTED"></a>
### `buffer.MARK_CIRCLEPLUSCONNECTED` (number, Read-only)




<a id="buffer.MARK_DOTDOTDOT"></a>
### `buffer.MARK_DOTDOTDOT` (number, Read-only)




<a id="buffer.MARK_EMPTY"></a>
### `buffer.MARK_EMPTY` (number, Read-only)




<a id="buffer.MARK_FULLRECT"></a>
### `buffer.MARK_FULLRECT` (number, Read-only)




<a id="buffer.MARK_LCORNER"></a>
### `buffer.MARK_LCORNER` (number, Read-only)




<a id="buffer.MARK_LCORNERCURVE"></a>
### `buffer.MARK_LCORNERCURVE` (number, Read-only)




<a id="buffer.MARK_LEFTRECT"></a>
### `buffer.MARK_LEFTRECT` (number, Read-only)




<a id="buffer.MARK_MINUS"></a>
### `buffer.MARK_MINUS` (number, Read-only)




<a id="buffer.MARK_PIXMAP"></a>
### `buffer.MARK_PIXMAP` (number, Read-only)




<a id="buffer.MARK_PLUS"></a>
### `buffer.MARK_PLUS` (number, Read-only)




<a id="buffer.MARK_RGBAIMAGE"></a>
### `buffer.MARK_RGBAIMAGE` (number, Read-only)




<a id="buffer.MARK_ROUNDRECT"></a>
### `buffer.MARK_ROUNDRECT` (number, Read-only)




<a id="buffer.MARK_SHORTARROW"></a>
### `buffer.MARK_SHORTARROW` (number, Read-only)




<a id="buffer.MARK_SMALLRECT"></a>
### `buffer.MARK_SMALLRECT` (number, Read-only)




<a id="buffer.MARK_TCORNER"></a>
### `buffer.MARK_TCORNER` (number, Read-only)




<a id="buffer.MARK_TCORNERCURVE"></a>
### `buffer.MARK_TCORNERCURVE` (number, Read-only)




<a id="buffer.MARK_UNDERLINE"></a>
### `buffer.MARK_UNDERLINE` (number, Read-only)




<a id="buffer.MARK_VLINE"></a>
### `buffer.MARK_VLINE` (number, Read-only)




<a id="buffer.MASK_FOLDERS"></a>
### `buffer.MASK_FOLDERS` (number, Read-only)




<a id="buffer.MOD_ALT"></a>
### `buffer.MOD_ALT` (number, Read-only)




<a id="buffer.MOD_CTRL"></a>
### `buffer.MOD_CTRL` (number, Read-only)




<a id="buffer.MOD_META"></a>
### `buffer.MOD_META` (number, Read-only)




<a id="buffer.MOD_SHIFT"></a>
### `buffer.MOD_SHIFT` (number, Read-only)




<a id="buffer.MOD_SUPER"></a>
### `buffer.MOD_SUPER` (number, Read-only)




<a id="buffer.MOUSE_DRAG"></a>
### `buffer.MOUSE_DRAG` (number, Read-only)




<a id="buffer.MOUSE_PRESS"></a>
### `buffer.MOUSE_PRESS` (number, Read-only)




<a id="buffer.MOUSE_RELEASE"></a>
### `buffer.MOUSE_RELEASE` (number, Read-only)




<a id="buffer.MULTIAUTOC_EACH"></a>
### `buffer.MULTIAUTOC_EACH` (number, Read-only)




<a id="buffer.MULTIAUTOC_ONCE"></a>
### `buffer.MULTIAUTOC_ONCE` (number, Read-only)




<a id="buffer.MULTIPASTE_EACH"></a>
### `buffer.MULTIPASTE_EACH` (number, Read-only)




<a id="buffer.MULTIPASTE_ONCE"></a>
### `buffer.MULTIPASTE_ONCE` (number, Read-only)




<a id="buffer.ORDER_CUSTOM"></a>
### `buffer.ORDER_CUSTOM` (number, Read-only)




<a id="buffer.ORDER_PERFORMSORT"></a>
### `buffer.ORDER_PERFORMSORT` (number, Read-only)




<a id="buffer.ORDER_PRESORTED"></a>
### `buffer.ORDER_PRESORTED` (number, Read-only)




<a id="buffer.SEL_LINES"></a>
### `buffer.SEL_LINES` (number, Read-only)




<a id="buffer.SEL_RECTANGLE"></a>
### `buffer.SEL_RECTANGLE` (number, Read-only)




<a id="buffer.SEL_STREAM"></a>
### `buffer.SEL_STREAM` (number, Read-only)




<a id="buffer.SEL_THIN"></a>
### `buffer.SEL_THIN` (number, Read-only)




<a id="buffer.STYLE_BRACEBAD"></a>
### `buffer.STYLE_BRACEBAD` (number, Read-only)




<a id="buffer.STYLE_BRACELIGHT"></a>
### `buffer.STYLE_BRACELIGHT` (number, Read-only)




<a id="buffer.STYLE_CALLTIP"></a>
### `buffer.STYLE_CALLTIP` (number, Read-only)




<a id="buffer.STYLE_CONTROLCHAR"></a>
### `buffer.STYLE_CONTROLCHAR` (number, Read-only)




<a id="buffer.STYLE_DEFAULT"></a>
### `buffer.STYLE_DEFAULT` (number, Read-only)




<a id="buffer.STYLE_FOLDDISPLAYTEXT"></a>
### `buffer.STYLE_FOLDDISPLAYTEXT` (number, Read-only)




<a id="buffer.STYLE_INDENTGUIDE"></a>
### `buffer.STYLE_INDENTGUIDE` (number, Read-only)




<a id="buffer.STYLE_LINENUMBER"></a>
### `buffer.STYLE_LINENUMBER` (number, Read-only)




<a id="buffer.STYLE_MAX"></a>
### `buffer.STYLE_MAX` (number, Read-only)




<a id="buffer.TIME_FOREVER"></a>
### `buffer.TIME_FOREVER` (number, Read-only)




<a id="buffer.UPDATE_CONTENT"></a>
### `buffer.UPDATE_CONTENT` (number, Read-only)




<a id="buffer.UPDATE_H_SCROLL"></a>
### `buffer.UPDATE_H_SCROLL` (number, Read-only)




<a id="buffer.UPDATE_SELECTION"></a>
### `buffer.UPDATE_SELECTION` (number, Read-only)




<a id="buffer.UPDATE_V_SCROLL"></a>
### `buffer.UPDATE_V_SCROLL` (number, Read-only)




<a id="buffer.VISIBLE_SLOP"></a>
### `buffer.VISIBLE_SLOP` (number, Read-only)




<a id="buffer.VISIBLE_STRICT"></a>
### `buffer.VISIBLE_STRICT` (number, Read-only)




<a id="buffer.VS_NONE"></a>
### `buffer.VS_NONE` (number, Read-only)




<a id="buffer.VS_RECTANGULARSELECTION"></a>
### `buffer.VS_RECTANGULARSELECTION` (number, Read-only)




<a id="buffer.VS_USERACCESSIBLE"></a>
### `buffer.VS_USERACCESSIBLE` (number, Read-only)




<a id="buffer.WRAPINDENT_FIXED"></a>
### `buffer.WRAPINDENT_FIXED` (number, Read-only)




<a id="buffer.WRAPINDENT_INDENT"></a>
### `buffer.WRAPINDENT_INDENT` (number, Read-only)




<a id="buffer.WRAPINDENT_SAME"></a>
### `buffer.WRAPINDENT_SAME` (number, Read-only)




<a id="buffer.WRAPVISUALFLAGLOC_DEFAULT"></a>
### `buffer.WRAPVISUALFLAGLOC_DEFAULT` (number, Read-only)




<a id="buffer.WRAPVISUALFLAGLOC_END_BY_TEXT"></a>
### `buffer.WRAPVISUALFLAGLOC_END_BY_TEXT` (number, Read-only)




<a id="buffer.WRAPVISUALFLAGLOC_START_BY_TEXT"></a>
### `buffer.WRAPVISUALFLAGLOC_START_BY_TEXT` (number, Read-only)




<a id="buffer.WRAPVISUALFLAG_END"></a>
### `buffer.WRAPVISUALFLAG_END` (number, Read-only)




<a id="buffer.WRAPVISUALFLAG_MARGIN"></a>
### `buffer.WRAPVISUALFLAG_MARGIN` (number, Read-only)




<a id="buffer.WRAPVISUALFLAG_NONE"></a>
### `buffer.WRAPVISUALFLAG_NONE` (number, Read-only)




<a id="buffer.WRAPVISUALFLAG_START"></a>
### `buffer.WRAPVISUALFLAG_START` (number, Read-only)




<a id="buffer.WRAP_CHAR"></a>
### `buffer.WRAP_CHAR` (number, Read-only)




<a id="buffer.WRAP_NONE"></a>
### `buffer.WRAP_NONE` (number, Read-only)




<a id="buffer.WRAP_WHITESPACE"></a>
### `buffer.WRAP_WHITESPACE` (number, Read-only)




<a id="buffer.WRAP_WORD"></a>
### `buffer.WRAP_WORD` (number, Read-only)




<a id="buffer.WS_INVISIBLE"></a>
### `buffer.WS_INVISIBLE` (number, Read-only)




<a id="buffer.WS_VISIBLEAFTERINDENT"></a>
### `buffer.WS_VISIBLEAFTERINDENT` (number, Read-only)




<a id="buffer.WS_VISIBLEALWAYS"></a>
### `buffer.WS_VISIBLEALWAYS` (number, Read-only)




<a id="buffer.additional_caret_fore"></a>
### `buffer.additional_caret_fore` (number)

The foreground color, in "0xBBGGRR" format, of additional carets.

<a id="buffer.additional_carets_blink"></a>
### `buffer.additional_carets_blink` (bool)

Allow additional carets to blink.
  The default value is `true`.

<a id="buffer.additional_carets_visible"></a>
### `buffer.additional_carets_visible` (bool)

Display additional carets.
  The default value is `true`.

<a id="buffer.additional_sel_alpha"></a>
### `buffer.additional_sel_alpha` (number)

The alpha value, ranging from `0` (transparent) to `255` (opaque), of
  additional selections.
  The default value is `buffer.ALPHA_NOALPHA`, for no alpha.

<a id="buffer.additional_sel_back"></a>
### `buffer.additional_sel_back` (number, Write-only)

The background color, in "0xBBGGRR" format, of additional selections.
  This field has no effect when calling `buffer.set_sel_back(false, ...)`.

<a id="buffer.additional_sel_fore"></a>
### `buffer.additional_sel_fore` (number, Write-only)

The foreground color, in "0xBBGGRR" format, of additional selections.
  This field has no effect when calling `buffer.set_sel_fore(false, ...)`.

<a id="buffer.additional_selection_typing"></a>
### `buffer.additional_selection_typing` (bool)

Type into multiple selections.
  The default value is `false`.

<a id="buffer.all_lines_visible"></a>
### `buffer.all_lines_visible` (bool, Read-only)

Whether or not all lines are visible.

<a id="buffer.anchor"></a>
### `buffer.anchor` (number)

The anchor's position.

<a id="buffer.annotation_lines"></a>
### `buffer.annotation_lines` (table, Read-only)

Table of the number of annotation text lines for line numbers starting from
  zero.

<a id="buffer.annotation_style"></a>
### `buffer.annotation_style` (table)

Table of style numbers for annotation text for line numbers starting from
  zero.
  Only some style attributes are active in annotations: font,
  size/size_fractional, bold/weight, italics, fore, back, and character_set.

<a id="buffer.annotation_text"></a>
### `buffer.annotation_text` (table)

Table of annotation text for line numbers starting from zero.

<a id="buffer.annotation_visible"></a>
### `buffer.annotation_visible` (number)

The annotation visibility mode.

  * `buffer.ANNOTATION_HIDDEN`
    Annotations are invisible.
  * `buffer.ANNOTATION_STANDARD`
    Draw annotations left-justified with no decoration.
  * `buffer.ANNOTATION_BOXED`
    Indent annotations to match the annotated text and outline them with a
    box.
  * `buffer.ANNOTATION_INDENTED`
    Indent non-decorated annotations to match the annotated text.

  The default value is `buffer.ANNOTATION_HIDDEN`.

<a id="buffer.auto_c_auto_hide"></a>
### `buffer.auto_c_auto_hide` (bool)

Automatically cancel an autocompletion or user list when no entries match
  typed text.
  The default value is `true`.

<a id="buffer.auto_c_cancel_at_start"></a>
### `buffer.auto_c_cancel_at_start` (bool)

Cancel an autocompletion list when backspacing to a position before where
  autocompletion started (instead of before the word being completed).
  This option has no effect for a user list.
  The default value is `true`.

<a id="buffer.auto_c_case_insensitive_behaviour"></a>
### `buffer.auto_c_case_insensitive_behaviour` (number)

The behavior mode for a case insensitive autocompletion or user list when
  [`buffer.auto_c_ignore_case`](#buffer.auto_c_ignore_case) is `true`.

  * `buffer.CASEINSENSITIVEBEHAVIOUR_RESPECTCASE`
    Prefer to select case-sensitive matches.
  * `buffer.CASEINSENSITIVEBEHAVIOUR_IGNORECASE`
    No preference.

  The default value is `buffer.CASEINSENSITIVEBEHAVIOUR_RESPECTCASE`.

<a id="buffer.auto_c_choose_single"></a>
### `buffer.auto_c_choose_single` (bool)

Automatically choose the item in a single-item autocompletion list.
  This option has no effect for a user list.
  The default value is `false`.

<a id="buffer.auto_c_current"></a>
### `buffer.auto_c_current` (number, Read-only)

The index of the currently selected item in an autocompletion or user list.

<a id="buffer.auto_c_current_text"></a>
### `buffer.auto_c_current_text` (string, Read-only)

The text of the currently selected item in an autocompletion or user list.

<a id="buffer.auto_c_drop_rest_of_word"></a>
### `buffer.auto_c_drop_rest_of_word` (bool)

Delete any word characters immediately to the right of autocompleted text.
  The default value is `false`.

<a id="buffer.auto_c_fill_ups"></a>
### `buffer.auto_c_fill_ups` (string, Write-only)

The set of characters that choose the currently selected item in an
  autocompletion or user list when the user types one of them.
  The default value is `''`.

<a id="buffer.auto_c_ignore_case"></a>
### `buffer.auto_c_ignore_case` (bool)

Ignore case when searching an autocompletion or user list for matches.
  The default value is `false`.

<a id="buffer.auto_c_max_height"></a>
### `buffer.auto_c_max_height` (number)

The maximum number of items per page to show in autocompletion and user
  lists. The default value is `5`.

<a id="buffer.auto_c_max_width"></a>
### `buffer.auto_c_max_width` (number)

The maximum number of characters per item to show in autocompletion and
  user lists.
  The default value is `0`, which automatically sizes the width to fit the
  longest item.

<a id="buffer.auto_c_multi"></a>
### `buffer.auto_c_multi` (number)

The multiple selection autocomplete mode.

  * `buffer.MULTIAUTOC_ONCE`
    Autocomplete into only the main selection.
  * `buffer.MULTIAUTOC_EACH`
    Autocomplete into all selections.

  The default value is `buffer.MULTIAUTOC_ONCE`.

<a id="buffer.auto_c_order"></a>
### `buffer.auto_c_order` (number)

The order setting for autocompletion and user lists.

  * `buffer.ORDER_PRESORTED`
    Lists passed to [`buffer.auto_c_show()`](#buffer.auto_c_show) are in sorted, alphabetical
    order.
  * `buffer.ORDER_PERFORMSORT`
    Sort autocompletion lists passed to [`buffer.auto_c_show()`](#buffer.auto_c_show).
  * `buffer.ORDER_CUSTOM`
    Lists passed to [`buffer.auto_c_show()`](#buffer.auto_c_show) are already in a custom order.

  The default value is `buffer.ORDER_PRESORTED`.

<a id="buffer.auto_c_separator"></a>
### `buffer.auto_c_separator` (number)

The byte value of the character that separates autocompletion and user list
  list items.
  The default value is `32` (' ').

<a id="buffer.auto_c_type_separator"></a>
### `buffer.auto_c_type_separator` (number)

The character byte that separates autocompletion and user list items and
  their image types.
  Autocompletion and user list items can display both an image and text.
  Register images and their types using [`buffer.register_image()`](#buffer.register_image) or
  [`buffer.register_rgba_image()`](#buffer.register_rgba_image) before appending image types to list
  items after type separator characters.
  The default value is 63 ('?').

<a id="buffer.back_space_un_indents"></a>
### `buffer.back_space_un_indents` (bool)

Un-indent text when backspacing within indentation.
  The default value is `false`.

<a id="buffer.call_tip_fore_hlt"></a>
### `buffer.call_tip_fore_hlt` (number, Write-only)

A call tip's highlighted text foreground color, in "0xBBGGRR" format.

<a id="buffer.call_tip_pos_start"></a>
### `buffer.call_tip_pos_start` (number, Write-only)

The position in which backspacing beyond it hides a visible call tip.

<a id="buffer.call_tip_position"></a>
### `buffer.call_tip_position` (boolean)

Display a call tip above the current line instead of below it.
  The default value is `false`.

<a id="buffer.call_tip_use_style"></a>
### `buffer.call_tip_use_style` (number)

The pixel width of tab characters in call tips.
  When non-zero, also enables the use of style number `buffer.STYLE_CALLTIP`
  instead of `buffer.STYLE_DEFAULT` for call tip styles.
  The default value is `0`.

<a id="buffer.caret_fore"></a>
### `buffer.caret_fore` (number)

The caret's foreground color, in "0xBBGGRR" format.

<a id="buffer.caret_line_back"></a>
### `buffer.caret_line_back` (number)

The background color, in "0xBBGGRR" format, of the line that contains the
  caret.

<a id="buffer.caret_line_back_alpha"></a>
### `buffer.caret_line_back_alpha` (number)

The caret line's background alpha value, ranging from `0` (transparent) to
  `255` (opaque).
  The default value is `buffer.ALPHA_NOALPHA`, for no alpha.

<a id="buffer.caret_line_frame"></a>
### `buffer.caret_line_frame` (number)

The caret line's frame width in pixels.
  When non-zero, the line that contains the caret is framed instead of
  colored in. The `buffer.caret_line_back` and `buffer.caret_line_back_alpha`
  properties apply to the frame.
  The default value is `0`.

<a id="buffer.caret_line_visible"></a>
### `buffer.caret_line_visible` (bool)

Color the background of the line that contains the caret a different color.
  The default value is `false`.

<a id="buffer.caret_line_visible_always"></a>
### `buffer.caret_line_visible_always` (bool)

Always show the caret line, even when the window is not in focus.
  The default value is `false`, showing the line only when the window is in
  focus.

<a id="buffer.caret_period"></a>
### `buffer.caret_period` (number)

The time between caret blinks in milliseconds.
  A value of `0` stops blinking.
  The default value is `500`.

<a id="buffer.caret_sticky"></a>
### `buffer.caret_sticky` (number)

The caret's preferred horizontal position when moving between lines.

  * `buffer.CARETSTICKY_OFF`
    Use the same position the caret had on the previous line.
  * `buffer.CARETSTICKY_ON`
    Use the last position the caret was moved to via the mouse, left/right
    arrow keys, home/end keys, etc. Typing text does not affect the position.
  * `buffer.CARETSTICKY_WHITESPACE`
    Use the position the caret had on the previous line, but prior to any
    inserted indentation.

  The default value is `buffer.CARETSTICKY_OFF`.

<a id="buffer.caret_style"></a>
### `buffer.caret_style` (number)

The caret's visual style.

  * `buffer.CARETSTYLE_INVISIBLE`
    No caret.
  * `buffer.CARETSTYLE_LINE`
    A line caret.
  * `buffer.CARETSTYLE_BLOCK`
    A block caret.

  The default value is `buffer.CARETSTYLE_LINE`.

<a id="buffer.caret_width"></a>
### `buffer.caret_width` (number)

The line caret's pixel width in insert mode, either `0`, `1`, `2`, or `3`.
  The default value is `1`.

<a id="buffer.char_at"></a>
### `buffer.char_at` (table, Read-only)

Table of character bytes at positions starting from zero.

<a id="buffer.column"></a>
### `buffer.column` (table, Read-only)

Table of column numbers (taking tab widths into account) for positions
  starting from zero.
  Multi-byte characters count as single characters.

<a id="buffer.current_pos"></a>
### `buffer.current_pos` (number)

The caret's position.
  When set, does not scroll the caret into view.

<a id="buffer.cursor"></a>
### `buffer.cursor` (number)

The display cursor type.

  * `buffer.CURSORNORMAL`
    The text insert cursor.
  * `buffer.CURSORARROW`
    The arrow cursor.
  * `buffer.CURSORWAIT`
    The wait cursor.
  * `buffer.CURSORREVERSEARROW`
    The reversed arrow cursor.

  The default value is `buffer.CURSORNORMAL`.

<a id="buffer.edge_colour"></a>
### `buffer.edge_colour` (number)

The color, in "0xBBGGRR" format, of the single edge or background for long
  lines according to `buffer.edge_mode`.

<a id="buffer.edge_column"></a>
### `buffer.edge_column` (number)

The column number to mark long lines at.

<a id="buffer.edge_mode"></a>
### `buffer.edge_mode` (number)

The long line mark mode.

  * `buffer.EDGE_NONE`
    Long lines are not marked.
  * `buffer.EDGE_LINE`
    Draw a single vertical line whose color is [`buffer.edge_colour`](#buffer.edge_colour) at
    column [`buffer.edge_column`](#buffer.edge_column).
  * `buffer.EDGE_BACKGROUND`
    Change the background color of text after column [`buffer.edge_column`](#buffer.edge_column)
    to [`buffer.edge_colour`](#buffer.edge_colour).
  * `buffer.EDGE_MULTILINE`
    Draw vertical lines whose colors and columns are defined by calls to
    [`buffer:multi_edge_add_line()`](#buffer.multi_edge_add_line).

<a id="buffer.encoding"></a>
### `buffer.encoding` (string or nil)

The string encoding of the file, or `nil` for binary files.

<a id="buffer.end_at_last_line"></a>
### `buffer.end_at_last_line` (bool)

Disable scrolling past the last line.
  The default value is `true`.

<a id="buffer.end_styled"></a>
### `buffer.end_styled` (number, Read-only)

The current styling position or the last correctly styled character's
  position.

<a id="buffer.eol_mode"></a>
### `buffer.eol_mode` (number)

The current end of line mode. Changing the current mode does not convert
  any of the buffer's existing end of line characters.
  Use [`buffer.convert_eols()`](#buffer.convert_eols) to do so.

  * `buffer.EOL_CRLF`
    Carriage return with line feed ("\r\n").
  * `buffer.EOL_CR`
    Carriage return ("\r").
  * `buffer.EOL_LF`
    Line feed ("\n").

  The default value is `buffer.EOL_CRLF` on Windows platforms,
  `buffer.EOL_LF` otherwise.

<a id="buffer.extra_ascent"></a>
### `buffer.extra_ascent` (number)

The amount of pixel padding above lines.
  The default value is `0`.

<a id="buffer.extra_descent"></a>
### `buffer.extra_descent` (number)

The amount of pixel padding below lines.
  The default is `0`.

<a id="buffer.filename"></a>
### `buffer.filename` (string)

The absolute file path associated with the buffer.

<a id="buffer.first_visible_line"></a>
### `buffer.first_visible_line` (number)

The line number of the line at the top of the view, starting from zero.

<a id="buffer.fold_display_text_style"></a>
### `buffer.fold_display_text_style` (number)

The fold display text mode.

  * `buffer.FOLDDISPLAYTEXT_HIDDEN`
    Fold display text is not shown.
  * `buffer.FOLDDISPLAYTEXT_STANDARD`
    Fold display text is shown with no decoration.
  * `buffer.FOLDDISPLAYTEXT_BOXED`
    Fold display text is shown outlined with a box.

  The default value is `buffer.FOLDDISPLAYTEXT_HIDDEN`.

<a id="buffer.fold_expanded"></a>
### `buffer.fold_expanded` (table)

Table of flags that indicate whether or not fold points are expanded for
  line numbers starting from zero.
  Setting expanded fold states does not toggle folds; it only updates fold
  margin markers. Use [`buffer.toggle_fold()`](#buffer.toggle_fold) instead.

<a id="buffer.fold_flags"></a>
### `buffer.fold_flags` (number, Read-only)

Bit-mask of folding lines to draw in the buffer.

  * `buffer.FOLDFLAG_LINEBEFORE_EXPANDED`
    Draw lines above expanded folds.
  * `buffer.FOLDFLAG_LINEBEFORE_CONTRACTED`
    Draw lines above collapsed folds.
  * `buffer.FOLDFLAG_LINEAFTER_EXPANDED`
    Draw lines below expanded folds.
  * `buffer.FOLDFLAG_LINEAFTER_CONTRACTED`
    Draw lines below collapsed folds.
  * `buffer.FOLDFLAG_LEVELNUMBERS`
    Show hexadecimal fold levels in line margins.
    This option cannot be combined with `FOLDFLAG_LINESTATE`.
  * `buffer.FOLDFLAG_LINESTATE`
    Show line state in line margins.
    This option cannot be combined with `FOLDFLAG_LEVELNUMBERS`.

  The default value is `0`.

<a id="buffer.fold_level"></a>
### `buffer.fold_level` (table)

Table of fold level bit-masks for line numbers starting from zero.
  Fold level masks comprise of an integer level combined with any of the
  following bit flags:

  * `buffer.FOLDLEVELBASE`
    The initial fold level.
  * `buffer.FOLDLEVELWHITEFLAG`
    The line is blank.
  * `buffer.FOLDLEVELHEADERFLAG`
    The line is a header, or fold point.

<a id="buffer.fold_parent"></a>
### `buffer.fold_parent` (table, Read-only)

Table of fold point line numbers for child line numbers starting from zero.
  A line number of `-1` means no line was found.

<a id="buffer.h_scroll_bar"></a>
### `buffer.h_scroll_bar` (bool)

Display the horizontal scroll bar.
  The default value is `true`.

<a id="buffer.highlight_guide"></a>
### `buffer.highlight_guide` (number)

The indentation guide column number to also highlight when highlighting
  matching braces, or `0` to stop indentation guide highlighting.

<a id="buffer.idle_styling"></a>
### `buffer.idle_styling` (number)

The idle styling mode.
  This mode has no effect when `buffer.wrap_mode` is on.

  * `buffer.IDLESTYLING_NONE`
    Style all the currently visible text before displaying it.
  * `buffer.IDLESTYLING_TOVISIBLE`
    Style some text before displaying it and then style the rest
    incrementally in the background as an idle-time task.
  * `buffer.IDLESTYLING_AFTERVISIBLE`
    Style text after the currently visible portion in the background.
  * `buffer.IDLESTYLING_ALL`
    Style text both before and after the visible text in the background.

  The default value is `buffer.IDLESTYLING_NONE`.

<a id="buffer.indent"></a>
### `buffer.indent` (number)

The number of spaces in one level of indentation.
  The default value is `0`, which uses the value of [`buffer.tab_width`](#buffer.tab_width).

<a id="buffer.indentation_guides"></a>
### `buffer.indentation_guides` (number)

The indentation guide drawing mode.
  Indentation guides are dotted vertical lines that appear within indentation
  whitespace at each level of indentation.

  * `buffer.IV_NONE`
    Does not draw any guides.
  * `buffer.IV_REAL`
    Draw guides only within indentation whitespace.
  * `buffer.IV_LOOKFORWARD`
    Draw guides beyond the current line up to the next non-empty line's
    indentation level, but with an additional level if the previous non-empty
    line is a fold point.
  * `buffer.IV_LOOKBOTH`
    Draw guides beyond the current line up to either the indentation level of
    the previous or next non-empty line, whichever is greater.

  The default value is `buffer.IV_NONE`.

<a id="buffer.indic_alpha"></a>
### `buffer.indic_alpha` (table)

Table of fill color alpha values, ranging from `0` (transparent) to `255`
  (opaque), for indicator numbers from `0` to `31` whose styles are either
  `INDIC_ROUNDBOX`, `INDIC_STRAIGHTBOX`, or `INDIC_DOTBOX`.
  The default values are `buffer.ALPHA_NOALPHA`, for no alpha.

<a id="buffer.indic_fore"></a>
### `buffer.indic_fore` (table)

Table of foreground colors, in "0xBBGGRR" format, for indicator numbers
  from `0` to `31`.
  Changing an indicator's foreground color resets that indicator's hover
  foreground color.

<a id="buffer.indic_hover_fore"></a>
### `buffer.indic_hover_fore` (table)

Table of hover foreground colors, in "0xBBGGRR" format, for indicator
  numbers from `0` to `31`.
  The default values are the respective indicator foreground colors.

<a id="buffer.indic_hover_style"></a>
### `buffer.indic_hover_style` (table)

Table of hover styles for indicators numbers from `0` to `31`. An
  indicator's hover style drawn when either the cursor hovers over that
  indicator or the caret is within that indicator.
  The default values are the respective indicator styles.

<a id="buffer.indic_outline_alpha"></a>
### `buffer.indic_outline_alpha` (table)

Table of outline color alpha values, ranging from `0` (transparent) to
  `255` (opaque), for indicator numbers from `0` to `31` whose styles are
  either `INDIC_ROUNDBOX`, `INDIC_STRAIGHTBOX`, or `INDIC_DOTBOX`.
  The default values are `buffer.ALPHA_NOALPHA`, for no alpha.

<a id="buffer.indic_style"></a>
### `buffer.indic_style` (table)

Table of styles for indicator numbers from `0` to `31`.

  * `buffer.INDIC_PLAIN`
    An underline.
  * `buffer.INDIC_SQUIGGLE`
    A squiggly underline 3 pixels in height.
  * `buffer.INDIC_TT`
    An underline of small 'T' shapes.
  * `buffer.INDIC_DIAGONAL`
    An underline of diagonal hatches.
  * `buffer.INDIC_STRIKE`
    Strike out.
  * `buffer.INDIC_HIDDEN`
    Invisible.
  * `buffer.INDIC_BOX`
    A bounding box.
  * `buffer.INDIC_ROUNDBOX`
    A translucent box with rounded corners around the text. Use
    [`buffer.indic_alpha`](#buffer.indic_alpha) and [`buffer.indic_outline_alpha`](#buffer.indic_outline_alpha) to set the
    fill and outline transparency, respectively. Their default values are
    `30` and `50`.
  * `buffer.INDIC_STRAIGHTBOX`
    Similar to `INDIC_ROUNDBOX` but with sharp corners.
  * `buffer.INDIC_DASH`
    A dashed underline.
  * `buffer.INDIC_DOTS`
    A dotted underline.
  * `buffer.INDIC_SQUIGGLELOW`
    A squiggly underline 2 pixels in height.
  * `buffer.INDIC_DOTBOX`
    Similar to `INDIC_STRAIGHTBOX` but with a dotted outline.
    Translucency alternates between [`buffer.indic_alpha`](#buffer.indic_alpha) and
    [`buffer.indic_outline_alpha`](#buffer.indic_outline_alpha) starting with the top-left pixel.
  * `buffer.INDIC_SQUIGGLEPIXMAP`
    Identical to `INDIC_SQUIGGLE` but draws faster by using a pixmap instead
    of multiple line segments.
  * `buffer.INDIC_COMPOSITIONTHICK`
    A 2-pixel thick underline at the bottom of the line inset by 1 pixel on
    on either side. Similar in appearance to the target in Asian language
    input composition.
  * `buffer.INDIC_COMPOSITIONTHIN`
    A 1-pixel thick underline just before the bottom of the line inset by 1
    pixel on either side. Similar in appearance to the non-target ranges in
    Asian language input composition.
  * `buffer.INDIC_FULLBOX`
    Similar to `INDIC_STRAIGHTBOX` but extends to the top of its line,
    potentially touching any similar indicators on the line above.
  * `buffer.INDIC_TEXTFORE`
    Changes the color of text to an indicator's foreground color.
  * `buffer.INDIC_POINT`
    A triangle below the start of the indicator range.
  * `buffer.INDIC_POINTCHARACTER`
    A triangle below the centre of the first character of the indicator
    range.

  Use [`_SCINTILLA.next_indic_number()`](#_SCINTILLA.next_indic_number) for custom indicators.
  Changing an indicator's style resets that indicator's hover style.

<a id="buffer.indic_under"></a>
### `buffer.indic_under` (table)

Table of flags that indicate whether or not to draw indicators behind text
  instead of over the top of it for indicator numbers from `0` to `31`.
  The default values are `false`.

<a id="buffer.indicator_current"></a>
### `buffer.indicator_current` (number)

The indicator number in the range of `0` to `31` used by
  [`buffer.indicator_fill_range()`](#buffer.indicator_fill_range) and
  [`buffer.indicator_clear_range()`](#buffer.indicator_clear_range).

<a id="buffer.length"></a>
### `buffer.length` (number, Read-only)

The number of bytes in the buffer.

<a id="buffer.line_count"></a>
### `buffer.line_count` (number, Read-only)

The number of lines in the buffer.
  There is always at least one.

<a id="buffer.line_end_position"></a>
### `buffer.line_end_position` (table, Read-only)

Table of positions at the ends of lines, but before any end of line
  characters, for line numbers starting from zero.

<a id="buffer.line_indent_position"></a>
### `buffer.line_indent_position` (table, Read-only)

Table of positions at the ends of indentation for line numbers starting
  from zero.

<a id="buffer.line_indentation"></a>
### `buffer.line_indentation` (table)

Table of column indentation amounts, for line numbers starting from zero.

<a id="buffer.line_visible"></a>
### `buffer.line_visible` (table, Read-only)

Table of flags that indicate whether or not lines are visible for line
  numbers starting from zero.

<a id="buffer.lines_on_screen"></a>
### `buffer.lines_on_screen` (number, Read-only)

The number of completely visible lines in the view.
  It is possible to have a partial line visible at the bottom of the view.

<a id="buffer.main_selection"></a>
### `buffer.main_selection` (number)

The number of the main, or most recent, selection.
  Only an existing selection can be made main.

<a id="buffer.margin_back_n"></a>
### `buffer.margin_back_n` (table)

Table of background colors, in "0xBBGGRR" format, of margin numbers from
  `0` to `buffer.margins - 1` (`4` by default).
  Only affects margins of type `buffer.MARGIN_COLOUR`.

<a id="buffer.margin_cursor_n"></a>
### `buffer.margin_cursor_n` (table)

Table of cursor types shown over margin numbers from `0` to
  `buffer.margins - 1` (`4` by default).

  * `buffer.CURSORARROW`
    Normal arrow cursor.
  * `buffer.CURSORREVERSEARROW`
    Reversed arrow cursor.

  The default values are `buffer.CURSORREVERSEARROW`.

<a id="buffer.margin_left"></a>
### `buffer.margin_left` (number)

The pixel size of the left margin of the buffer text.
  The default value is `1`.

<a id="buffer.margin_mask_n"></a>
### `buffer.margin_mask_n` (table)

Table of bit-masks of markers whose symbols marker symbol margins can
  display for margin numbers from `0` to `buffer.margins - 1` (`4` by
  default).
  Bit-masks are 32-bit values whose bits correspond to the 32 available
  markers.
  The default values are `0`, `bit32.bnot(buffer.MASK_FOLDERS)`, `0`, `0`,
  and `0`, for a line margin and logical marker margin.

<a id="buffer.margin_options"></a>
### `buffer.margin_options` (number)

A bit-mask of margin option settings.

  * `buffer.MARGINOPTION_NONE`
    None.
  * `buffer.MARGINOPTION_SUBLINESELECT`
    Select only a wrapped line's sub-line (rather than the entire line) when
    the line number margin is clicked.

  The default value is `buffer.MARGINOPTION_NONE`.

<a id="buffer.margin_right"></a>
### `buffer.margin_right` (number)

The pixel size of the right margin of the buffer text.
  The default value is `1`.

<a id="buffer.margin_sensitive_n"></a>
### `buffer.margin_sensitive_n` (table)

Table of flags that indicate whether or not mouse clicks in margins emit
  `MARGIN_CLICK` events for margin numbers from `0` to `buffer.margins - 1`
  (`4` by default).
  The default values are `false`.

<a id="buffer.margin_style"></a>
### `buffer.margin_style` (table)

Table of style numbers for line numbers starting from zero in the text
  margin.
  Only some style attributes are active in text margins: font, size, bold,
  italics, fore, and back.

<a id="buffer.margin_text"></a>
### `buffer.margin_text` (table)

Table of text displayed in text margins for line numbers starting from
  zero.

<a id="buffer.margin_type_n"></a>
### `buffer.margin_type_n` (table)

Table of margin types for margin numbers from `0` to `buffer.margins - 1`
  (`4` by default).

  * `buffer.MARGIN_SYMBOL`
    A marker symbol margin.
  * `buffer.MARGIN_NUMBER`
    A line number margin.
  * `buffer.MARGIN_BACK`
    A marker symbol margin whose background color matches the default text
    background color.
  * `buffer.MARGIN_FORE`
    A marker symbol margin whose background color matches the default text
    foreground color.
  * `buffer.MARGIN_TEXT`
    A text margin.
  * `buffer.MARGIN_RTEXT`
    A right-justified text margin.
  * `buffer.MARGIN_COLOUR`
    A marker symbol margin whose background color is configurable.

  The default value for the first margin is `buffer.MARGIN_NUMBER`, followed
  by `buffer.MARGIN_SYMBOL` for the rest.

<a id="buffer.margin_width_n"></a>
### `buffer.margin_width_n` (table)

Table of pixel margin widths for margin numbers from `0` to
  `buffer.margins - 1` (`4` by default).

<a id="buffer.margins"></a>
### `buffer.margins` (number)

The number of margins.
  The default value is `5`.

<a id="buffer.marker_alpha"></a>
### `buffer.marker_alpha` (table, Write-only)

Table of alpha values, ranging from `0` (transparent) to `255` (opaque),
  of markers drawn in the text area (not the margin) for markers numbers from
  `0` to `31`.
  The default values are `buffer.ALPHA_NOALPHA`, for no alpha.

<a id="buffer.marker_back"></a>
### `buffer.marker_back` (table, Write-only)

Table of background colors, in "0xBBGGRR" format, of marker numbers from
  `0` to `31`.

<a id="buffer.marker_back_selected"></a>
### `buffer.marker_back_selected` (table, Write-only)

Table of background colors, in "0xBBGGRR" format, of markers whose folding
  blocks are selected for marker numbers from `0` to `31`.

<a id="buffer.marker_fore"></a>
### `buffer.marker_fore` (table, Write-only)

Table of foreground colors, in "0xBBGGRR" format, of marker numbers from
  `0` to `31`.

<a id="buffer.modify"></a>
### `buffer.modify` (bool)

Whether or not the buffer has unsaved changes.

<a id="buffer.mouse_dwell_time"></a>
### `buffer.mouse_dwell_time` (number)

The number of milliseconds the mouse must idle before generating a
  `DWELL_START` event. A time of `buffer.TIME_FOREVER` will never generate
  one.

<a id="buffer.mouse_selection_rectangular_switch"></a>
### `buffer.mouse_selection_rectangular_switch` (bool)

Whether or not pressing [`buffer.rectangular_selection_modifier`](#buffer.rectangular_selection_modifier) when
  selecting text normally with the mouse turns on rectangular selection.
  The default value is `false`.

<a id="buffer.multi_paste"></a>
### `buffer.multi_paste` (number)

The multiple selection paste mode.

  * `buffer.MULTIPASTE_ONCE`
    Paste into only the main selection.
  * `buffer.MULTIPASTE_EACH`
    Paste into all selections.

  The default value is `buffer.MULTIPASTE_ONCE`.

<a id="buffer.multiple_selection"></a>
### `buffer.multiple_selection` (bool)

Enable multiple selection.
  The default value is `false`.

<a id="buffer.overtype"></a>
### `buffer.overtype` (bool)

Enable overtype mode, where typed characters overwrite existing ones.
  The default value is `false`.

<a id="buffer.property"></a>
### `buffer.property` (table)

Map of key-value string pairs used by lexers.

<a id="buffer.property_expanded"></a>
### `buffer.property_expanded` (table, Read-only)

Map of key-value string pairs used by lexers with `%()` variable
  replacement performed in values.

<a id="buffer.property_int"></a>
### `buffer.property_int` (table, Read-only)

Map of key-value pairs used by lexers with values interpreted as numbers,
  or `0` if not found.

<a id="buffer.punctuation_chars"></a>
### `buffer.punctuation_chars` (string)

The string set of characters recognized as punctuation characters.
  Set this only after setting [`buffer.word_chars`](#buffer.word_chars).
  The default value is a string that contains all non-word and non-whitespace
  characters.

<a id="buffer.read_only"></a>
### `buffer.read_only` (bool)

Whether or not the buffer is read-only.
  The default value is `false`.

<a id="buffer.rectangular_selection_anchor"></a>
### `buffer.rectangular_selection_anchor` (number)

The rectangular selection's anchor position.

<a id="buffer.rectangular_selection_anchor_virtual_space"></a>
### `buffer.rectangular_selection_anchor_virtual_space` (number)

The amount of virtual space for the rectangular selection's anchor.

<a id="buffer.rectangular_selection_caret"></a>
### `buffer.rectangular_selection_caret` (number)

The rectangular selection's caret position.

<a id="buffer.rectangular_selection_caret_virtual_space"></a>
### `buffer.rectangular_selection_caret_virtual_space` (number)

The amount of virtual space for the rectangular selection's caret.

<a id="buffer.rectangular_selection_modifier"></a>
### `buffer.rectangular_selection_modifier` (number)

The modifier key used in combination with a mouse drag in order to create a
  rectangular selection.

  * `buffer.MOD_CTRL`
    The "Control" modifier key.
  * `buffer.MOD_ALT`
    The "Alt" modifier key.
  * `buffer.MOD_SUPER`
    The "Super" modifier key, usually defined as the left "Windows" or
    "Command" key.

  The default value is `buffer.MOD_CTRL`.

<a id="buffer.representation"></a>
### `buffer.representation` (table)

The alternative string representations of characters.
  Representations are displayed in the same way control characters are. Use
  the empty string for the '\0' character when assigning its representation.
  Call [`buffer.clear_representation()`](#buffer.clear_representation) to remove a representation.

<a id="buffer.rgba_image_height"></a>
### `buffer.rgba_image_height` (number)

The height of the RGBA image to be defined using
  [`buffer.marker_define_rgba_image()`](#buffer.marker_define_rgba_image).

<a id="buffer.rgba_image_scale"></a>
### `buffer.rgba_image_scale` (number)

The scale factor in percent of the RGBA image to be defined using
  [`buffer.marker_define_rgba_image()`](#buffer.marker_define_rgba_image).
  This is useful on OSX with a retina display where each display unit is 2
  pixels: use a factor of `200` so that each image pixel is displayed using a
  screen pixel. The default scale, `100`, will stretch each image pixel to
  cover 4 screen pixels on a retina display.

<a id="buffer.rgba_image_width"></a>
### `buffer.rgba_image_width` (number)

The width of the RGBA image to be defined using
  [`buffer.marker_define_rgba_image()`](#buffer.marker_define_rgba_image) and
  [`buffer.register_rgba_image()`](#buffer.register_rgba_image).

<a id="buffer.scroll_width"></a>
### `buffer.scroll_width` (number)

The horizontal scrolling pixel width.
  For performance, the view does not measure the display width of the buffer
  to determine the properties of the horizontal scroll bar, but uses an
  assumed width instead. To ensure the width of the currently visible lines
  can be scrolled use [`buffer.scroll_width_tracking`](#buffer.scroll_width_tracking).
  The default value is `2000`.

<a id="buffer.scroll_width_tracking"></a>
### `buffer.scroll_width_tracking` (bool)

Continuously update the horizontal scrolling width to match the maximum
  width of a displayed line beyond [`buffer.scroll_width`](#buffer.scroll_width).
  The default value is `false`.

<a id="buffer.search_flags"></a>
### `buffer.search_flags` (number)

The bit-mask of search flags used by [`buffer.search_in_target()`](#buffer.search_in_target).

  * `buffer.FIND_WHOLEWORD`
    Match search text only when it is surrounded by non-word characters.
  * `buffer.FIND_MATCHCASE`
    Match search text case sensitively.
  * `buffer.FIND_WORDSTART`
    Match search text only when the previous character is a non-word
    character.
  * `buffer.FIND_REGEXP`
    Interpret search text as a regular expression.

  The default value is `0`.

<a id="buffer.sel_alpha"></a>
### `buffer.sel_alpha` (number)

The selection's alpha value, ranging from `0` (transparent) to `255`
  (opaque).
  The default value is `buffer.ALPHA_NOALPHA`, for no alpha.

<a id="buffer.sel_eol_filled"></a>
### `buffer.sel_eol_filled` (bool)

Extend the selection to the view's right margin.
  The default value is `false`.

<a id="buffer.selection_empty"></a>
### `buffer.selection_empty` (bool, Read-only)

Whether or not no text is selected.

<a id="buffer.selection_end"></a>
### `buffer.selection_end` (number)

The position of the end of the selected text.
  When set, becomes the current position, but is not scrolled into view.

<a id="buffer.selection_is_rectangle"></a>
### `buffer.selection_is_rectangle` (bool, Read-only)

Whether or not the selection is a rectangular selection.

<a id="buffer.selection_mode"></a>
### `buffer.selection_mode` (number)

The selection mode.

  * `buffer.SEL_STREAM`
    Character selection.
  * `buffer.SEL_RECTANGLE`
    Rectangular selection.
  * `buffer.SEL_LINES`
    Line selection.
  * `buffer.SEL_THIN`
    Thin rectangular selection. This is the mode after a rectangular
    selection has been typed into and ensures that no characters are
    selected.

  When set, caret movement alters the selected text until this field is set
  again to the same value or until [`buffer.cancel()`](#buffer.cancel) is called.

<a id="buffer.selection_n_anchor"></a>
### `buffer.selection_n_anchor` (table)

Table of positions at the beginning of existing selections numbered from
  zero, the main selection.

<a id="buffer.selection_n_anchor_virtual_space"></a>
### `buffer.selection_n_anchor_virtual_space` (table)

Table of positions at the beginning of virtual space selected in existing
  selections numbered from zero, the main selection.

<a id="buffer.selection_n_caret"></a>
### `buffer.selection_n_caret` (table)

Table of positions at the end of existing selections numbered from zero,
  the main selection.

<a id="buffer.selection_n_caret_virtual_space"></a>
### `buffer.selection_n_caret_virtual_space` (table)

Table of positions at the end of virtual space selected in existing
  selections numbered from zero, the main selection.

<a id="buffer.selection_n_end"></a>
### `buffer.selection_n_end` (table)

Table of positions at the end of existing selections numbered from zero,
  the main selection.

<a id="buffer.selection_n_start"></a>
### `buffer.selection_n_start` (table)

Table of positions at the beginning of existing selections numbered from
  zero, the main selection.

<a id="buffer.selection_start"></a>
### `buffer.selection_start` (number)

The position of the beginning of the selected text.
  When set, becomes the anchor, but is not scrolled into view.

<a id="buffer.selections"></a>
### `buffer.selections` (number, Read-only)

The number of active selections. There is always at least one selection.

<a id="buffer.style_at"></a>
### `buffer.style_at` (table, Read-only)

Table of style numbers at positions starting from zero.

<a id="buffer.style_back"></a>
### `buffer.style_back` (table)

Table of background colors, in "0xBBGGRR" format, of text for style numbers
  from `0` to `255`.

<a id="buffer.style_bold"></a>
### `buffer.style_bold` (table)

Table of flags that indicate whether or not text is bold for style numbers
  from `0` to `255`.
  The default values are `false`.

<a id="buffer.style_case"></a>
### `buffer.style_case` (table)

Table of letter case modes of text for style numbers from `0` to `255`.

  * `buffer.CASE_MIXED`
    Display text in normally.
  * `buffer.CASE_UPPER`
    Display text in upper case.
  * `buffer.CASE_LOWER`
    Display text in lower case.
  * `buffer.CASE_CAMEL`
    Display text in camel case.

  The default values are `buffer.CASE_MIXED`.

<a id="buffer.style_changeable"></a>
### `buffer.style_changeable` (table)

Table of flags that indicate whether or not text is changeable for style
  numbers from `0` to `255`.
  The default values are `true`.
  Read-only styles do not allow the caret into the range of text.

<a id="buffer.style_eol_filled"></a>
### `buffer.style_eol_filled` (table)

Table of flags that indicate whether or not the background colors of styles
  whose characters occur last on lines extend all the way to the view's right
  margin for style numbers from `0` to `255`.
  The default values are `false`.

<a id="buffer.style_font"></a>
### `buffer.style_font` (table)

Table of string font names of text for style numbers from `0` to `255`.

<a id="buffer.style_fore"></a>
### `buffer.style_fore` (table)

Table of foreground colors, in "0xBBGGRR" format, of text for style numbers
  from `0` to `255`.

<a id="buffer.style_italic"></a>
### `buffer.style_italic` (table)

Table of flags that indicate whether or not text is italic for style
  numbers from `0` to `255`.
  The default values are `false`.

<a id="buffer.style_name"></a>
### `buffer.style_name` (table, Read-only)

Table of style names for style numbers from `0` to `255`.

<a id="buffer.style_size"></a>
### `buffer.style_size` (table)

Table of font sizes of text for style numbers from `0` to `255`.

<a id="buffer.style_underline"></a>
### `buffer.style_underline` (table)

Table of flags that indicate whether or not text is underlined for style
  numbers from `0` to `255`.
  The default values are `false`.

<a id="buffer.style_visible"></a>
### `buffer.style_visible` (table)

Table of flags that indicate whether or not text is visible for style
  numbers from `0` to `255`.
  The default values are `true`.

<a id="buffer.tab_draw_mode"></a>
### `buffer.tab_draw_mode` (number)

The draw mode of visible tabs.

  * `buffer.TD_LONGARROW`
    An arrow that stretches until the tabstop.
  * `buffer.TD_STRIKEOUT`
    A horizontal line that stretches until the tabstop.

  The default value is `buffer.TD_LONGARROW`.

<a id="buffer.tab_indents"></a>
### `buffer.tab_indents` (bool)

Indent text when tabbing within indentation.
  The default value is `false`.

<a id="buffer.tab_label"></a>
### `buffer.tab_label` (string)

The buffer's tab label in the tab bar.

<a id="buffer.tab_width"></a>
### `buffer.tab_width` (number)

The number of space characters represented by a tab character.
  The default value is `8`.

<a id="buffer.tag"></a>
### `buffer.tag` (table, Read-only)

List of capture text for capture numbers from a regular expression search.

<a id="buffer.target_end"></a>
### `buffer.target_end` (number)

The position of the end of the target range.
  This is also set by a successful [`buffer.search_in_target()`](#buffer.search_in_target).

<a id="buffer.target_start"></a>
### `buffer.target_start` (number)

The position of the beginning of the target range.
  This is also set by a successful [`buffer.search_in_target()`](#buffer.search_in_target).

<a id="buffer.target_text"></a>
### `buffer.target_text` (string, Read-only)

The text in the target range.

<a id="buffer.text_length"></a>
### `buffer.text_length` (number, Read-only)

The number of bytes in the buffer.

<a id="buffer.use_tabs"></a>
### `buffer.use_tabs` (bool)

Use tabs instead of spaces in indentation. Changing the current setting
  does not convert any of the buffer's existing indentation. Use
  [`textadept.editing.convert_indentation()`](#textadept.editing.convert_indentation) to do so.
  The default value is `true`.

<a id="buffer.v_scroll_bar"></a>
### `buffer.v_scroll_bar` (bool)

Display the vertical scroll bar.
  The default value is `true`.

<a id="buffer.view_eol"></a>
### `buffer.view_eol` (bool)

Display end of line characters.
  The default value is `false`.

<a id="buffer.view_ws"></a>
### `buffer.view_ws` (number)

The whitespace visibility mode.

  * `buffer.WS_INVISIBLE`
    Whitespace is invisible.
  * `buffer.WS_VISIBLEALWAYS`
    Display all space characters as dots and tab characters as arrows.
  * `buffer.WS_VISIBLEAFTERINDENT`
    Display only non-indentation spaces and tabs as dots and arrows.
  * `buffer.WS_VISIBLEONLYININDENT`
    Display only indentation spaces and tabs as dots and arrows.

  The default value is `buffer.WS_INVISIBLE`.

<a id="buffer.virtual_space_options"></a>
### `buffer.virtual_space_options` (number)

The virtual space mode.

  * `buffer.VS_NONE`
    Disable virtual space.
  * `buffer.VS_RECTANGULARSELECTION`
    Enable virtual space only for rectangular selections.
  * `buffer.VS_USERACCESSIBLE`
    Enable virtual space.
  * `buffer.VS_NOWRAPLINESTART`
    Prevent the caret from wrapping to the previous line via
    `buffer:char_left()` and `buffer:char_left_extend()`. This option is not
    restricted to virtual space.

  When virtual space is enabled, the caret may move into the space past end
  of line characters.
  The default value is `buffer.VS_NONE`.

<a id="buffer.whitespace_chars"></a>
### `buffer.whitespace_chars` (string)

The string set of characters recognized as whitespace characters.
  Set this only after setting [`buffer.word_chars`](#buffer.word_chars).
  The default value is a string that contains all non-newline characters less
  than ASCII value 33.

<a id="buffer.whitespace_size"></a>
### `buffer.whitespace_size` (number)

The pixel size of the dots that represent space characters when whitespace
  is visible.
  The default value is `1`.

<a id="buffer.word_chars"></a>
### `buffer.word_chars` (string)

The string set of characters recognized as word characters.
  The default value is a string that contains alphanumeric characters, an
  underscore, and all characters greater than ASCII value 127.

<a id="buffer.wrap_indent_mode"></a>
### `buffer.wrap_indent_mode` (number)

The wrapped line indent mode.

  * `buffer.WRAPINDENT_FIXED`
    Indent wrapped lines by [`buffer.wrap_start_indent`](#buffer.wrap_start_indent).
  * `buffer.WRAPINDENT_SAME`
    Indent wrapped lines the same amount as the first line.
  * `buffer.WRAPINDENT_INDENT`
    Indent wrapped lines one more level than the level of the first line.

  The default value is `buffer.WRAPINDENT_FIXED`.

<a id="buffer.wrap_mode"></a>
### `buffer.wrap_mode` (number)

Long line wrap mode.

  * `buffer.WRAP_NONE`
    Long lines are not wrapped.
  * `buffer.WRAP_WORD`
    Wrap long lines at word (and style) boundaries.
  * `buffer.WRAP_CHAR`
    Wrap long lines at character boundaries.
  * `buffer.WRAP_WHITESPACE`
    Wrap long lines at word boundaries (ignoring style boundaries).

  The default value is `buffer.WRAP_NONE`.

<a id="buffer.wrap_start_indent"></a>
### `buffer.wrap_start_indent` (number)

The number of spaces of indentation to display wrapped lines with if
  [`buffer.wrap_indent_mode`](#buffer.wrap_indent_mode) is `buffer.WRAP_INDENT_FIXED`.
  The default value is `0`.

<a id="buffer.wrap_visual_flags"></a>
### `buffer.wrap_visual_flags` (number)

The wrapped line visual flag display mode.

  * `buffer.WRAPVISUALFLAG_NONE`
    No visual flags.
  * `buffer.WRAPVISUALFLAG_END`
    Show a visual flag at the end of a wrapped line.
  * `buffer.WRAPVISUALFLAG_START`
    Show a visual flag at the beginning of a sub-line.
  * `buffer.WRAPVISUALFLAG_MARGIN`
    Show a visual flag in the sub-line's line number margin.

  The default value is `buffer.WRAPVISUALFLAG_NONE`.

<a id="buffer.wrap_visual_flags_location"></a>
### `buffer.wrap_visual_flags_location` (number)

The wrapped line visual flag drawing mode.

  * `buffer.WRAPVISUALFLAGLOC_DEFAULT`
    Draw a visual flag near the view's right margin.
  * `buffer.WRAPVISUALFLAGLOC_END_BY_TEXT`
    Draw a visual flag near text at the end of a wrapped line.
  * `buffer.WRAPVISUALFLAGLOC_START_BY_TEXT`
    Draw a visual flag near text at the beginning of a subline.

  The default value is `buffer.WRAPVISUALFLAGLOC_DEFAULT`.

<a id="buffer.x_offset"></a>
### `buffer.x_offset` (number)

The horizontal scroll pixel position.
  A value of `0` is the normal position with the first text column visible at
  the left of the view.

<a id="buffer.zoom"></a>
### `buffer.zoom` (number)

The number of points to add to the size of all fonts.
  Negative values are allowed.
  The default value is `0`.


## Functions defined by `buffer`

<a id="buffer.add_selection"></a>
### `buffer.add_selection`(*buffer, end\_pos, start\_pos*)

Selects the range of text between positions *start_pos* to *end_pos* as the
main selection, retaining all other selections as additional selections.
Since an empty selection still counts as a selection, use
`buffer.set_selection()` first when setting a list of selections.

Parameters:

* *`buffer`*: A buffer.
* *`end_pos`*: The caret position of the range of text to select in *buffer*.
* *`start_pos`*: The anchor position of the range of text to select in
  *buffer*.

See also:

* [`buffer.set_selection`](#buffer.set_selection)

<a id="buffer.add_text"></a>
### `buffer.add_text`(*buffer, text*)

Adds string *text* to the buffer at the caret position and moves the caret to
the end of the added text without scrolling it into view.

Parameters:

* *`buffer`*: A buffer.
* *`text`*: The text to add.

<a id="buffer.annotation_clear_all"></a>
### `buffer.annotation_clear_all`(*buffer*)

Clears annotations from all lines.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.append_text"></a>
### `buffer.append_text`(*buffer, text*)

Appends string *text* to the end of the buffer without modifying any existing
selections or scrolling the text into view.

Parameters:

* *`buffer`*: A buffer.
* *`text`*: The text to append.

<a id="buffer.auto_c_active"></a>
### `buffer.auto_c_active`(*buffer*)

Returns whether or not an autocompletion or user list is visible.

Parameters:

* *`buffer`*: A buffer.

Return:

* bool

<a id="buffer.auto_c_cancel"></a>
### `buffer.auto_c_cancel`(*buffer*)

Cancels an autocompletion or user list.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.auto_c_complete"></a>
### `buffer.auto_c_complete`(*buffer*)

Completes the current word with the one selected in an autocompletion list.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.auto_c_pos_start"></a>
### `buffer.auto_c_pos_start`(*buffer*)

Returns the position where autocompletion started or where a user list was
shown.

Parameters:

* *`buffer`*: A buffer.

Return:

* number

<a id="buffer.auto_c_select"></a>
### `buffer.auto_c_select`(*buffer, prefix*)

Selects the first item that starts with string *prefix* in an autocompletion
or user list, using the case sensitivity setting `buffer.auto_c_ignore_case`.

Parameters:

* *`buffer`*: A buffer.
* *`prefix`*: The item in the list to select.

<a id="buffer.auto_c_show"></a>
### `buffer.auto_c_show`(*buffer, len\_entered, items*)

Displays an autocompletion list constructed from string *items* (whose items
are delimited by `buffer.auto_c_separator` characters) using *len_entered*
number of characters behind the caret as the prefix of the word to be
autocompleted.
The sorted order of *items* (`buffer.auto_c_order`) must have already been
defined.

Parameters:

* *`buffer`*: A buffer.
* *`len_entered`*: The number of characters before the caret used to provide
  the context.
* *`items`*: The sorted string of words to show, separated by
  `buffer.auto_c_separator` characters (initially spaces).

See also:

* [`buffer.auto_c_separator`](#buffer.auto_c_separator)
* [`buffer.auto_c_order`](#buffer.auto_c_order)

<a id="buffer.auto_c_stops"></a>
### `buffer.auto_c_stops`(*buffer, chars*)

Allows the user to type any character in string set *chars* in order to
cancel an autocompletion or user list.
The default set is empty.

Parameters:

* *`buffer`*: A buffer.
* *`chars`*: The string of characters that cancel autocompletion. This string
  is empty by default.

<a id="buffer.back_tab"></a>
### `buffer.back_tab`(*buffer*)

Un-indents the text on the selected lines.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.begin_undo_action"></a>
### `buffer.begin_undo_action`(*buffer*)

Starts a sequence of actions to be undone or redone as a single action.
May be nested.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.brace_bad_light"></a>
### `buffer.brace_bad_light`(*buffer, pos*)

Highlights the character at position *pos* as an unmatched brace character
using the `'style.bracebad'` style.
Removes highlighting when *pos* is `-1`.

Parameters:

* *`buffer`*: A buffer.
* *`pos`*: The position in *buffer* to highlight, or `-1` to remove the
  highlight.

<a id="buffer.brace_bad_light_indicator"></a>
### `buffer.brace_bad_light_indicator`(*buffer, use\_indicator, indicator*)

Highlights unmatched brace characters with indicator number *indicator*, in
the range of `0` to `31`, instead of the
`buffer.STYLE_BRACEBAD` style if *use_indicator* is `true`.

Parameters:

* *`buffer`*: A buffer.
* *`use_indicator`*: Whether or not to use an indicator.
* *`indicator`*: The indicator number to use.

<a id="buffer.brace_highlight"></a>
### `buffer.brace_highlight`(*buffer, pos1, pos2*)

Highlights the characters at positions *pos1* and *pos2* as matching braces
using the `'style.bracelight'` style.
If indent guides are enabled, locates the column with `buffer.column` and
sets `buffer.highlight_guide` in order to highlight the indent guide.

Parameters:

* *`buffer`*: A buffer.
* *`pos1`*: The first position in *buffer* to highlight.
* *`pos2`*: The second position in *buffer* to highlight.

<a id="buffer.brace_highlight_indicator"></a>
### `buffer.brace_highlight_indicator`(*buffer, use\_indicator, indicator*)

Highlights matching brace characters with indicator number *indicator*, in
the range of `0` to `31`, instead of the
`buffer.STYLE_BRACELIGHT` style if *use_indicator* is `true`.

Parameters:

* *`buffer`*: A buffer.
* *`use_indicator`*: Whether or not to use an indicator.
* *`indicator`*: The indicator number to use.

<a id="buffer.brace_match"></a>
### `buffer.brace_match`(*buffer, pos*)

Returns the position of the matching brace for the brace character at
position *pos*, taking nested braces into account, or `-1`.
The brace characters recognized are '(', ')', '[', ']', '{', '}', '<', and
'>' and must have the same style.

Parameters:

* *`buffer`*: A buffer.
* *`pos`*: The position of the brace in *buffer* to match.

Return:

* number

<a id="buffer.call_tip_active"></a>
### `buffer.call_tip_active`(*buffer*)

Returns whether or not a call tip is visible.

Parameters:

* *`buffer`*: A buffer.

Return:

* bool

<a id="buffer.call_tip_cancel"></a>
### `buffer.call_tip_cancel`(*buffer*)

Removes a call tip from view.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.call_tip_pos_start"></a>
### `buffer.call_tip_pos_start`(*buffer*)

Returns a call tip's display position.

Parameters:

* *`buffer`*: A buffer.

Return:

* number

<a id="buffer.call_tip_set_hlt"></a>
### `buffer.call_tip_set_hlt`(*buffer, start\_pos, end\_pos*)

Highlights a call tip's text between positions *start_pos*, starting from
zero, to *end_pos* with the color `buffer.call_tip_fore_hlt`.

Parameters:

* *`buffer`*: A buffer.
* *`start_pos`*: The start position in a call tip text to highlight.
* *`end_pos`*: The end position in a call tip text to highlight.

<a id="buffer.call_tip_show"></a>
### `buffer.call_tip_show`(*buffer, pos, text*)

Displays a call tip at position *pos* with string *text* as the call tip's
contents.
Any "\001" or "\002" bytes in *text* are replaced by clickable up or down
arrow visuals, respectively. These may be used to indicate that a symbol has
more than one call tip, for example.

Parameters:

* *`buffer`*: A buffer.
* *`pos`*: The position in *buffer* to show a call tip at.
* *`text`*: The call tip text to show.

<a id="buffer.can_redo"></a>
### `buffer.can_redo`(*buffer*)

Returns whether or not there is an action to be redone.

Parameters:

* *`buffer`*: A buffer.

Return:

* bool

<a id="buffer.can_undo"></a>
### `buffer.can_undo`(*buffer*)

Returns whether or not there is an action to be undone.

Parameters:

* *`buffer`*: A buffer.

Return:

* bool

<a id="buffer.cancel"></a>
### `buffer.cancel`(*buffer*)

Cancels the active selection mode, autocompletion or user list, call tip,
etc.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.char_left"></a>
### `buffer.char_left`(*buffer*)

Moves the caret left one character.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.char_left_extend"></a>
### `buffer.char_left_extend`(*buffer*)

Moves the caret left one character, extending the selected text to the new
position.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.char_left_rect_extend"></a>
### `buffer.char_left_rect_extend`(*buffer*)

Moves the caret left one character, extending the rectangular selection to
the new position.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.char_right"></a>
### `buffer.char_right`(*buffer*)

Moves the caret right one character.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.char_right_extend"></a>
### `buffer.char_right_extend`(*buffer*)

Moves the caret right one character, extending the selected text to the new
position.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.char_right_rect_extend"></a>
### `buffer.char_right_rect_extend`(*buffer*)

Moves the caret right one character, extending the rectangular selection to
the new position.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.choose_caret_x"></a>
### `buffer.choose_caret_x`(*buffer*)

Identifies the current horizontal caret position as the caret's preferred
horizontal position when moving between lines.

Parameters:

* *`buffer`*: A buffer.

See also:

* [`buffer.caret_sticky`](#buffer.caret_sticky)

<a id="buffer.clear"></a>
### `buffer.clear`(*buffer*)

Deletes the selected text or the character at the caret.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.clear_all"></a>
### `buffer.clear_all`(*buffer*)

Deletes the buffer's text.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.clear_document_style"></a>
### `buffer.clear_document_style`(*buffer*)

Clears all styling and folding information.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.clear_registered_images"></a>
### `buffer.clear_registered_images`(*buffer*)

Clears all images registered using `buffer.register_image()` and
`buffer.register_rgba_image()`.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.clear_representation"></a>
### `buffer.clear_representation`(*buffer, char*)

Removes the alternate string representation for character *char*.

Parameters:

* *`buffer`*: A buffer.
* *`char`*: The character in `buffer.representations` to remove the alternate
  string representation for.

<a id="buffer.clear_selections"></a>
### `buffer.clear_selections`(*buffer*)

Removes all selections and moves the caret to the beginning of the buffer.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.colourise"></a>
### `buffer.colourise`(*buffer, start\_pos, end\_pos*)

Instructs the lexer to style and mark fold points in the range of text
between *start_pos* and *end_pos*.
If *end_pos* is `-1`, styles and marks to the end of the buffer.

Parameters:

* *`buffer`*: A buffer.
* *`start_pos`*: The start position of the range of text in *buffer* to
  process.
* *`end_pos`*: The end position of the range of text in *buffer* to process,
  or `-1` to process from *start_pos* to the end of *buffer*.

<a id="buffer.contracted_fold_next"></a>
### `buffer.contracted_fold_next`(*buffer, line*)

Returns the line number of the next contracted fold point starting from line
number *line*, or `-1` if none exists.

Parameters:

* *`buffer`*: A buffer.
* *`line`*: The line number in *buffer* to start at.

Return:

* number

<a id="buffer.convert_eols"></a>
### `buffer.convert_eols`(*buffer, mode*)

Converts all end of line characters to those in end of line mode *mode*.

Parameters:

* *`buffer`*: A buffer.
* *`mode`*: The end of line mode to convert to. Valid values are:
  * `buffer.EOL_CRLF`
  * `buffer.EOL_CR`
  * `buffer.EOL_LF`

<a id="buffer.copy"></a>
### `buffer.copy`(*buffer*)

Copies the selected text to the clipboard.
Multiple selections are copied in order with no delimiters. Rectangular
selections are copied from top to bottom with end of line characters. Virtual
space is not copied.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.copy_allow_line"></a>
### `buffer.copy_allow_line`(*buffer*)

Copies the selected text or the current line to the clipboard.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.copy_range"></a>
### `buffer.copy_range`(*buffer, start\_pos, end\_pos*)

Copies the range of text between positions *start_pos* and *end_pos* to the
clipboard.

Parameters:

* *`buffer`*: A buffer.
* *`start_pos`*: The start position of the range of text in *buffer* to copy.
* *`end_pos`*: The end position of the range of text in *buffer* to copy.

<a id="buffer.copy_text"></a>
### `buffer.copy_text`(*buffer, text*)

Copies string *text* to the clipboard.

Parameters:

* *`buffer`*: A buffer.
* *`text`*: The text to copy.

<a id="buffer.count_characters"></a>
### `buffer.count_characters`(*buffer, start\_pos, end\_pos*)

Returns the number of whole characters (taking multi-byte characters into
account) between positions *start_pos* and *end_pos*.

Parameters:

* *`buffer`*: A buffer.
* *`start_pos`*: The start position of the range of text in *buffer* to start
  counting at.
* *`end_pos`*: The end position of the range of text in *buffer* to stop
  counting at.

Return:

* number

<a id="buffer.cut"></a>
### `buffer.cut`(*buffer*)

Cuts the selected text to the clipboard.
Multiple selections are copied in order with no delimiters. Rectangular
selections are copied from top to bottom with end of line characters. Virtual
space is not copied.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.del_line_left"></a>
### `buffer.del_line_left`(*buffer*)

Deletes the range of text from the caret to the beginning of the current
line.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.del_line_right"></a>
### `buffer.del_line_right`(*buffer*)

Deletes the range of text from the caret to the end of the current line.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.del_word_left"></a>
### `buffer.del_word_left`(*buffer*)

Deletes the word to the left of the caret, including any leading non-word
characters.
`buffer.word_chars` contains the set of characters that constitute words.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.del_word_right"></a>
### `buffer.del_word_right`(*buffer*)

Deletes the word to the right of the caret, including any trailing non-word
characters.
`buffer.word_chars` contains the set of characters that constitute words.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.del_word_right_end"></a>
### `buffer.del_word_right_end`(*buffer*)

Deletes the word to the right of the caret, excluding any trailing non-word
characters.
`buffer.word_chars` contains the set of characters that constitute words.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.delete"></a>
### `buffer.delete`(*buffer*)

Deletes the buffer.
**Do not call this function.** Call `io.close_buffer()` instead. Emits a
`BUFFER_DELETED` event.

Parameters:

* *`buffer`*: A buffer.

See also:

* [`events.BUFFER_DELETED`](#events.BUFFER_DELETED)

<a id="buffer.delete_back"></a>
### `buffer.delete_back`(*buffer*)

Deletes the character behind the caret if no text is selected.
Otherwise, deletes the selected text.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.delete_back_not_line"></a>
### `buffer.delete_back_not_line`(*buffer*)

Deletes the character behind the caret unless either the caret is at the
beginning of a line or text is selected.
If text is selected, deletes it.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.delete_range"></a>
### `buffer.delete_range`(*buffer, pos, length*)

Deletes the range of text from position *pos* to *pos* + *length*.

Parameters:

* *`buffer`*: A buffer.
* *`pos`*: The start position of the range of text in *buffer* to delete.
* *`length`*: The number of characters in the range of text to delete.

<a id="buffer.doc_line_from_visible"></a>
### `buffer.doc_line_from_visible`(*buffer, display\_line*)

Returns the actual line number of displayed line number *display_line*,
taking hidden lines into account.
If *display_line* is less than or equal to zero, returns `0`. If
*display_line* is greater than or equal to the number of displayed lines,
returns `buffer.line_count`.

Parameters:

* *`buffer`*: A buffer.
* *`display_line`*: The display line number to use.

Return:

* number

<a id="buffer.document_end"></a>
### `buffer.document_end`(*buffer*)

Moves the caret to the end of the buffer.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.document_end_extend"></a>
### `buffer.document_end_extend`(*buffer*)

Moves the caret to the end of the buffer, extending the selected text to the
new position.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.document_start"></a>
### `buffer.document_start`(*buffer*)

Moves the caret to the beginning of the buffer.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.document_start_extend"></a>
### `buffer.document_start_extend`(*buffer*)

Moves the caret to the beginning of the buffer, extending the selected text
to the new position.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.drop_selection_n"></a>
### `buffer.drop_selection_n`(*buffer, n*)

Drops existing selection number *n*.

Parameters:

* *`buffer`*: A buffer.
* *`n`*: The number of the existing selection.

<a id="buffer.edit_toggle_overtype"></a>
### `buffer.edit_toggle_overtype`(*buffer*)

Toggles `buffer.overtype`.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.empty_undo_buffer"></a>
### `buffer.empty_undo_buffer`(*buffer*)

Deletes the undo and redo history.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.end_undo_action"></a>
### `buffer.end_undo_action`(*buffer*)

Ends a sequence of actions to be undone or redone as a single action.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.ensure_visible"></a>
### `buffer.ensure_visible`(*buffer, line*)

Ensures line number *line* is visible by expanding any fold points hiding it.

Parameters:

* *`buffer`*: A buffer.
* *`line`*: The line number in *buffer* to ensure visible.

<a id="buffer.ensure_visible_enforce_policy"></a>
### `buffer.ensure_visible_enforce_policy`(*buffer, line*)

Ensures line number *line* is visible by expanding any fold points hiding it
based on the vertical caret policy previously defined in
`buffer.set_visible_policy()`.

Parameters:

* *`buffer`*: A buffer.
* *`line`*: The line number in *buffer* to ensure visible.

<a id="buffer.find_column"></a>
### `buffer.find_column`(*buffer, line, column*)

Returns the position of column number *column* on line number *line* (taking
tab and multi-byte characters into account), or the position at the end of
line *line*.

Parameters:

* *`buffer`*: A buffer.
* *`line`*: The line number in *buffer* to use.
* *`column`*: The column number to use.

<a id="buffer.fold_all"></a>
### `buffer.fold_all`(*buffer, action*)

Contracts, expands, or toggles all fold points, depending on *action*.
When toggling, the state of the first fold point determines whether to
expand or contract.

Parameters:

* *`buffer`*: A buffer.
* *`action`*: The fold action to perform. Valid values are:
  * `buffer.FOLDACTION_CONTRACT`
  * `buffer.FOLDACTION_EXPAND`
  * `buffer.FOLDACTION_TOGGLE`

<a id="buffer.fold_children"></a>
### `buffer.fold_children`(*buffer, line, action*)

Contracts, expands, or toggles the fold point on line number *line*, as well
as all of its children, depending on *action*.

Parameters:

* *`buffer`*: A buffer.
* *`line`*: The line number in *buffer* to set the fold states for.
* *`action`*: The fold action to perform. Valid values are:
  * `buffer.FOLDACTION_CONTRACT`
  * `buffer.FOLDACTION_EXPAND`
  * `buffer.FOLDACTION_TOGGLE`

<a id="buffer.fold_line"></a>
### `buffer.fold_line`(*buffer, line, action*)

Contracts, expands, or toggles the fold point on line number *line*,
depending on *action*.

Parameters:

* *`buffer`*: A buffer.
* *`line`*: The line number in *buffer* to set the fold state for.
* *`action`*: The fold action to perform. Valid values are:
  * `buffer.FOLDACTION_CONTRACT`
  * `buffer.FOLDACTION_EXPAND`
  * `buffer.FOLDACTION_TOGGLE`

<a id="buffer.get_cur_line"></a>
### `buffer.get_cur_line`(*buffer*)

Returns the current line's text and the caret's position on that line,
starting from zero.

Parameters:

* *`buffer`*: A buffer.

Return:

* string, number

<a id="buffer.get_last_child"></a>
### `buffer.get_last_child`(*buffer, line, level*)

Returns the line number of the last line after line number *line* whose fold
level is greater than *level*.
If *level* is `-1`, returns the level of *line*.

Parameters:

* *`buffer`*: A buffer.
* *`line`*: The line number in *buffer* of a header line.
* *`level`*: The fold level, or `-1` for the level of *line*.

<a id="buffer.get_lexer"></a>
### `buffer.get_lexer`(*buffer, current*)

Returns the buffer's lexer name.
If *current* is `true`, returns the name of the lexer under the caret in
a multiple-language lexer.

Parameters:

* *`buffer`*: A buffer.
* *`current`*: Whether or not to get the lexer at the current caret position
  in multi-language lexers. The default is `false` and returns the parent
  lexer.

<a id="buffer.get_line"></a>
### `buffer.get_line`(*buffer, line*)

Returns the text on line number *line*, including end of line characters.

Parameters:

* *`buffer`*: A buffer.
* *`line`*: The line number in *buffer* to use.

Return:

* string, number

<a id="buffer.get_line_sel_end_position"></a>
### `buffer.get_line_sel_end_position`(*buffer, line*)

Returns the position of the end of the selected text on line number *line*,
or `-1` if *line* has no selection.

Parameters:

* *`buffer`*: A buffer.
* *`line`*: The line number in *buffer* to use.

<a id="buffer.get_line_sel_start_position"></a>
### `buffer.get_line_sel_start_position`(*buffer, line*)

Returns the position of the beginning of the selected text on line number
*line*, or `-1` if *line* has no selection.

Parameters:

* *`buffer`*: A buffer.
* *`line`*: The line number in *buffer* to use.

<a id="buffer.get_sel_text"></a>
### `buffer.get_sel_text`(*buffer*)

Returns the selected text.
Multiple selections are included in order with no delimiters. Rectangular
selections are included from top to bottom with end of line characters.
Virtual space is not included.

Parameters:

* *`buffer`*: A buffer.

Return:

* string, number

<a id="buffer.get_text"></a>
### `buffer.get_text`(*buffer*)

Returns the buffer's text.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.goto_line"></a>
### `buffer.goto_line`(*buffer, line*)

Moves the caret to the beginning of line number *line* and scrolls it into
view, regardless of whether *line* is hidden or not.

Parameters:

* *`buffer`*: A buffer.
* *`line`*: The line number in *buffer* to go to.

<a id="buffer.goto_pos"></a>
### `buffer.goto_pos`(*buffer, pos*)

Moves the caret to position *pos* and scrolls it into view.

Parameters:

* *`buffer`*: A buffer.
* *`pos`*: The position in *buffer* to go to.

<a id="buffer.hide_lines"></a>
### `buffer.hide_lines`(*buffer, start\_line, end\_line*)

Hides the range of lines between line numbers *start_line* to *end_line*.
This has no effect on fold levels or fold flags and the first line cannot be
hidden.

Parameters:

* *`buffer`*: A buffer.
* *`start_line`*: The start line of the range of lines in *buffer* to hide.
* *`end_line`*: The end line of the range of lines in *buffer* to hide.

<a id="buffer.home"></a>
### `buffer.home`(*buffer*)

Moves the caret to the beginning of the current line.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.home_display"></a>
### `buffer.home_display`(*buffer*)

Moves the caret to the beginning of the current wrapped line.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.home_display_extend"></a>
### `buffer.home_display_extend`(*buffer*)

Moves the caret to the beginning of the current wrapped line, extending the
selected text to the new position.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.home_extend"></a>
### `buffer.home_extend`(*buffer*)

Moves the caret to the beginning of the current line, extending the selected
text to the new position.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.home_rect_extend"></a>
### `buffer.home_rect_extend`(*buffer*)

Moves the caret to the beginning of the current line, extending the
rectangular selection to the new position.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.home_wrap"></a>
### `buffer.home_wrap`(*buffer*)

Moves the caret to the beginning of the current wrapped line or, if already
there, to the beginning of the actual line.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.home_wrap_extend"></a>
### `buffer.home_wrap_extend`(*buffer*)

Like `buffer.home_wrap()`, but extends the selected text to the new position.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.indicator_all_on_for"></a>
### `buffer.indicator_all_on_for`(*buffer, pos*)

Returns a bit-mask that represents which indicators are on at position *pos*.
Bit 0 is set if indicator 0 is on, bit 1 for indicator 1, etc.

Parameters:

* *`buffer`*: A buffer.
* *`pos`*: The position in *buffer* to get indicators at.

Return:

* number

<a id="buffer.indicator_clear_range"></a>
### `buffer.indicator_clear_range`(*buffer, pos, length*)

Clears indicator number `buffer.indicator_current` over the range of text
from position *pos* to *pos* + *length*.

Parameters:

* *`buffer`*: A buffer.
* *`pos`*: The start position of the range of text in *buffer* to clear
  indicators over.
* *`length`*: The number of characters in the range of text to clear
  indicators over.

<a id="buffer.indicator_end"></a>
### `buffer.indicator_end`(*buffer, indicator, pos*)

Returns the next boundary position, starting from position *pos*, of
indicator number *indicator*, in the range of `0` to `31`.
Returns `0` if *indicator* was not found.

Parameters:

* *`buffer`*: A buffer.
* *`indicator`*: An indicator number in the range of `0` to `31`.
* *`pos`*: The position in *buffer* of the indicator.

<a id="buffer.indicator_fill_range"></a>
### `buffer.indicator_fill_range`(*buffer, pos, length*)

Fills the range of text from position *pos* to *pos* + *length* with
indicator number `buffer.indicator_current`.

Parameters:

* *`buffer`*: A buffer.
* *`pos`*: The start position of the range of text in *buffer* to set
  indicators over.
* *`length`*: The number of characters in the range of text to set indicators
  over.

<a id="buffer.indicator_start"></a>
### `buffer.indicator_start`(*buffer, indicator, pos*)

Returns the previous boundary position, starting from position *pos*, of
indicator number *indicator*, in the range of `0` to `31`.
Returns `0` if *indicator* was not found.

Parameters:

* *`buffer`*: A buffer.
* *`indicator`*: An indicator number in the range of `0` to `31`.
* *`pos`*: The position in *buffer* of the indicator.

<a id="buffer.insert_text"></a>
### `buffer.insert_text`(*buffer, pos, text*)

Inserts string *text* at position *pos*, removing any selections.
If *pos* is `-1`, inserts *text* at the caret position.
If the caret is after the *pos*, it is moved appropriately, but not scrolled
into view.

Parameters:

* *`buffer`*: A buffer.
* *`pos`*: The position in *buffer* to insert text at, or `-1` for the
  current position.
* *`text`*: The text to insert.

<a id="buffer.is_range_word"></a>
### `buffer.is_range_word`(*buffer, start\_pos, end\_pos*)

Returns whether or not the the positions *start_pos* and *end_pos* are at
word boundaries.

Parameters:

* *`buffer`*: A buffer.
* *`start_pos`*: The start position of the range of text in *buffer* to
  check for a word boundary at.
* *`end_pos`*: The end position of the range of text in *buffer* to check for
  a word boundary at.

<a id="buffer.line_copy"></a>
### `buffer.line_copy`(*buffer*)

Copies the current line to the clipboard.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.line_cut"></a>
### `buffer.line_cut`(*buffer*)

Cuts the current line to the clipboard.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.line_delete"></a>
### `buffer.line_delete`(*buffer*)

Deletes the current line.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.line_down"></a>
### `buffer.line_down`(*buffer*)

Moves the caret down one line.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.line_down_extend"></a>
### `buffer.line_down_extend`(*buffer*)

Moves the caret down one line, extending the selected text to the new
position.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.line_down_rect_extend"></a>
### `buffer.line_down_rect_extend`(*buffer*)

Moves the caret down one line, extending the rectangular selection to the new
position.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.line_duplicate"></a>
### `buffer.line_duplicate`(*buffer*)

Duplicates the current line on a new line below.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.line_end"></a>
### `buffer.line_end`(*buffer*)

Moves the caret to the end of the current line.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.line_end_display"></a>
### `buffer.line_end_display`(*buffer*)

Moves the caret to the end of the current wrapped line.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.line_end_display_extend"></a>
### `buffer.line_end_display_extend`(*buffer*)

Moves the caret to the end of the current wrapped line, extending the
selected text to the new position.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.line_end_extend"></a>
### `buffer.line_end_extend`(*buffer*)

Moves the caret to the end of the current line, extending the selected text
to the new position.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.line_end_rect_extend"></a>
### `buffer.line_end_rect_extend`(*buffer*)

Moves the caret to the end of the current line, extending the rectangular
selection to the new position.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.line_end_wrap"></a>
### `buffer.line_end_wrap`(*buffer*)

Moves the caret to the end of the current wrapped line or, if already there,
to the end of the actual line.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.line_end_wrap_extend"></a>
### `buffer.line_end_wrap_extend`(*buffer*)

Like `buffer.line_end_wrap()`, but extends the selected text to the new
position.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.line_from_position"></a>
### `buffer.line_from_position`(*buffer, pos*)

Returns the line number of the line that contains position *pos*.
Returns `0` if *pos* is less than 0 or `buffer.line_count` if *pos* is
greater than `buffer.length`.

Parameters:

* *`buffer`*: A buffer.
* *`pos`*: The position in *buffer* to get the line number of.

Return:

* number

<a id="buffer.line_length"></a>
### `buffer.line_length`(*buffer, line*)

Returns the number of bytes on line number *line*, including end of line
characters.
To get line length excluding end of line characters, use
`buffer.line_end_position[line] - buffer.position_from_line(line)`.

Parameters:

* *`buffer`*: A buffer.
* *`line`*: The line number in *buffer* to get the length of.

Return:

* number

<a id="buffer.line_reverse"></a>
### `buffer.line_reverse`(*buffer*)

Reverses the order of the selected lines.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.line_scroll"></a>
### `buffer.line_scroll`(*buffer, columns, lines*)

Scrolls the buffer right *columns* columns and down *lines* lines.
Negative values are allowed.

Parameters:

* *`buffer`*: A buffer.
* *`columns`*: The number of columns to scroll horizontally.
* *`lines`*: The number of lines to scroll vertically.

<a id="buffer.line_scroll_down"></a>
### `buffer.line_scroll_down`(*buffer*)

Scrolls the buffer down one line, keeping the caret visible.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.line_scroll_up"></a>
### `buffer.line_scroll_up`(*buffer*)

Scrolls the buffer up one line, keeping the caret visible.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.line_transpose"></a>
### `buffer.line_transpose`(*buffer*)

Swaps the current line with the previous one.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.line_up"></a>
### `buffer.line_up`(*buffer*)

Moves the caret up one line.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.line_up_extend"></a>
### `buffer.line_up_extend`(*buffer*)

Moves the caret up one line, extending the selected text to the new position.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.line_up_rect_extend"></a>
### `buffer.line_up_rect_extend`(*buffer*)

Moves the caret up one line, extending the rectangular selection to the new
position.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.lines_join"></a>
### `buffer.lines_join`(*buffer*)

Joins the lines in the target range, inserting spaces between the words
joined at line boundaries.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.lines_split"></a>
### `buffer.lines_split`(*buffer, pixel\_width, width*)

Splits the lines in the target range into lines *width* pixels wide.
If *width* is `0`, splits the lines in the target range into lines as wide as
the view.

Parameters:

* *`buffer`*: A buffer.
* *`pixel_width`*: 
* *`width`*: The pixel width to split lines at. When `0`, uses the width of
  the view.

<a id="buffer.lower_case"></a>
### `buffer.lower_case`(*buffer*)

Converts the selected text to lower case letters.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.margin_text_clear_all"></a>
### `buffer.margin_text_clear_all`(*buffer*)

Clears all text in text margins.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.marker_add"></a>
### `buffer.marker_add`(*buffer, line, marker*)

Adds marker number *marker*, in the range of `0` to `31`, to line number
*line*, returning the added marker's handle which can be used in
`buffer.marker_delete_handle()` and `buffer.marker_line_from_handle()`, or
`0` if *line* is invalid.

Parameters:

* *`buffer`*: A buffer.
* *`line`*: The line number to add the marker on.
* *`marker`*: The marker number in the range of `0` to `31` to add.

Return:

* number

<a id="buffer.marker_add_set"></a>
### `buffer.marker_add_set`(*buffer, line, marker\_mask*)

Adds the markers specified in marker bit-mask *marker_mask* to line number
*line*.
The first bit is set to add marker number 0, the second bit for marker number
1, and so on up to marker number 31.

Parameters:

* *`buffer`*: A buffer.
* *`line`*: The line number to add the markers on.
* *`marker_mask`*: The mask of markers to set. Set bit 0 to set marker 0, bit
  1 for marker 1 and so on.

<a id="buffer.marker_define"></a>
### `buffer.marker_define`(*buffer, marker, symbol*)

Assigns marker symbol *symbol* to marker number *marker*, in the range of `0`
to `31`.
*symbol* is shown in marker symbol margins next to lines marked with
*marker*.

Parameters:

* *`buffer`*: A buffer.
* *`marker`*: The marker number in the range of `0` to `31` to set *symbol*
  for.
* *`symbol`*: The marker symbol: `buffer.MARK_*`.

See also:

* [`_SCINTILLA.next_marker_number`](#_SCINTILLA.next_marker_number)

<a id="buffer.marker_define_pixmap"></a>
### `buffer.marker_define_pixmap`(*buffer, marker, pixmap*)

Associates marker number *marker*, in the range of `0` to `31`, with XPM
image *pixmap*.
The `buffer.MARK_PIXMAP` marker symbol must be assigned to *marker*.
*pixmap* is shown in marker symbol margins next to lines marked with
*marker*.

Parameters:

* *`buffer`*: A buffer.
* *`marker`*: The marker number in the range of `0` to `31` to define
  pixmap *pixmap* for.
* *`pixmap`*: The string pixmap data.

<a id="buffer.marker_define_rgba_image"></a>
### `buffer.marker_define_rgba_image`(*buffer, marker, pixels*)

Associates marker number *marker*, in the range of `0` to `31`, with RGBA
image *pixels*.
The dimensions for *pixels* (`buffer.rgba_image_width` and
`buffer.rgba_image_height`) must have already been defined. *pixels* is a
sequence of 4 byte pixel values (red, blue, green, and alpha) defining the
image line by line starting at the top-left pixel.
The `buffer.MARK_RGBAIMAGE` marker symbol must be assigned to *marker*.
*pixels* is shown in symbol margins next to lines marked with *marker*.

Parameters:

* *`buffer`*: A buffer.
* *`marker`*: The marker number in the range of `0` to `31` to define RGBA
  data *pixels* for.
* *`pixels`*: The string sequence of 4 byte pixel values starting with the
  pixels for the top line, with the leftmost pixel first, then continuing
  with the pixels for subsequent lines. There is no gap between lines for
  alignment reasons. Each pixel consists of, in order, a red byte, a green
  byte, a blue byte and an alpha byte. The colour bytes are not premultiplied
  by the alpha value. That is, a fully red pixel that is 25% opaque will be
  `[FF, 00, 00, 3F]`.

<a id="buffer.marker_delete"></a>
### `buffer.marker_delete`(*buffer, line, marker*)

Deletes marker number *marker*, in the range of `0` to `31`, from line number
*line*. If *marker* is `-1`, deletes all markers from *line*.

Parameters:

* *`buffer`*: A buffer.
* *`line`*: The line number to delete the marker on.
* *`marker`*: The marker number in the range of `0` to `31` to delete from
  *line*, or `-1` to delete all markers from the line.

<a id="buffer.marker_delete_all"></a>
### `buffer.marker_delete_all`(*buffer, marker*)

Deletes marker number *marker*, in the range of `0` to `31`, from any line
that has it.
If *marker* is `-1`, deletes all markers from all lines.

Parameters:

* *`buffer`*: A buffer.
* *`marker`*: The marker number in the range of `0` to `31` to delete from
  all lines, or `-1` to delete all markers from all lines.

<a id="buffer.marker_delete_handle"></a>
### `buffer.marker_delete_handle`(*buffer, handle*)

Deletes the marker with handle *handle* returned by `buffer.marker_add()`.

Parameters:

* *`buffer`*: A buffer.
* *`handle`*: The identifier of a marker returned by `buffer.marker_add()`.

<a id="buffer.marker_enable_highlight"></a>
### `buffer.marker_enable_highlight`(*buffer, enabled*)

Highlights the margin fold markers for the current fold block if *enabled* is
`true`.

Parameters:

* *`buffer`*: A buffer.
* *`enabled`*: Whether or not to enable highlight.

<a id="buffer.marker_get"></a>
### `buffer.marker_get`(*buffer, line*)

Returns a bit-mask that represents the markers that were added to line number
*line*.
The first bit is set if marker number 0 is present, the second bit for marker
number 1, and so on.

Parameters:

* *`buffer`*: A buffer.
* *`line`*: The line number to get markers on.

Return:

* number

<a id="buffer.marker_line_from_handle"></a>
### `buffer.marker_line_from_handle`(*buffer, handle*)

Returns the line number that marker handle *handle*, returned by
`buffer.marker_add()`, was added to, or `-1` if the line was not found.

Parameters:

* *`buffer`*: A buffer.
* *`handle`*: The identifier of a marker returned by `buffer.marker_add()`.

Return:

* number

<a id="buffer.marker_next"></a>
### `buffer.marker_next`(*buffer, line, marker\_mask*)

Returns the first line number, starting at line number *line*, that has had
all of the markers represented by marker bit-mask *marker_mask* added to it.
Returns `-1` if no line was found.
Bit 0 is set if marker 0 is set, bit 1 for marker 1, etc., up to marker 31.

Parameters:

* *`buffer`*: A buffer.
* *`line`*: The start line to search from.
* *`marker_mask`*: The mask of markers to find. Set bit 0 to find marker 0,
  bit 1 for marker 1 and so on.

Return:

* number

<a id="buffer.marker_previous"></a>
### `buffer.marker_previous`(*buffer, line, marker\_mask*)

Returns the last line number, before or on line number *line*, that has had
all of the markers represented by marker bit-mask *marker_mask* added to it.
Returns `-1` if no line was found.
Bit 0 is set if marker 0 is set, bit 1 for marker 1, etc., up to marker 31.

Parameters:

* *`buffer`*: A buffer.
* *`line`*: The start line to search from.
* *`marker_mask`*: The mask of markers to find. Set bit 0 to find marker 0,
  bit 1 for marker 1 and so on.

Return:

* number

<a id="buffer.marker_symbol_defined"></a>
### `buffer.marker_symbol_defined`(*buffer, marker*)

Returns the symbol assigned to marker number *marker*, in the range of `0` to
`31`, used in `buffer.marker_define()`,
`buffer.marker_define_pixmap()`, or `buffer.marker_define_rgba_image()`.

Parameters:

* *`buffer`*: A buffer.
* *`marker`*: The marker number in the range of `0` to `31` to get the symbol
  of.

Return:

* number

<a id="buffer.move_caret_inside_view"></a>
### `buffer.move_caret_inside_view`(*buffer*)

Moves the caret into view if it is not already, removing any selections.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.move_selected_lines_down"></a>
### `buffer.move_selected_lines_down`(*buffer*)

Shifts the selected lines down one line.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.move_selected_lines_up"></a>
### `buffer.move_selected_lines_up`(*buffer*)

Shifts the selected lines up one line.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.multi_edge_add_line"></a>
### `buffer.multi_edge_add_line`(*buffer, column, color*)

Adds a new vertical line at column number *column* with color *color*, in
"0xBBGGRR" format.

Parameters:

* *`buffer`*: A buffer.
* *`column`*: The column number to add a vertical line at.
* *`color`*: The color in "0xBBGGRR" format.

<a id="buffer.multi_edge_clear_all"></a>
### `buffer.multi_edge_clear_all`(*buffer*)

Clears all vertical lines created by `buffer:multi_edge_add_line()`.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.multiple_select_add_each"></a>
### `buffer.multiple_select_add_each`(*buffer*)

Adds to the set of selections each occurrence of the main selection within
the target range.
If there is no selected text, the current word is used.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.multiple_select_add_next"></a>
### `buffer.multiple_select_add_next`(*buffer*)

Adds to the set of selections the next occurrence of the main selection
within the target range, makes that occurrence the new main selection, and
scrolls it into view.
If there is no selected text, the current word is used.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.new"></a>
### `buffer.new`()

Creates and returns a new buffer.
Emits a `BUFFER_NEW` event.

Return:

* the new buffer.

See also:

* [`events.BUFFER_NEW`](#events.BUFFER_NEW)

<a id="buffer.new_line"></a>
### `buffer.new_line`(*buffer*)

Types a new line at the caret position according to [`buffer.eol_mode`](#buffer.eol_mode).

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.page_down"></a>
### `buffer.page_down`(*buffer*)

Moves the caret down one page.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.page_down_extend"></a>
### `buffer.page_down_extend`(*buffer*)

Moves the caret down one page, extending the selected text to the new
position.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.page_down_rect_extend"></a>
### `buffer.page_down_rect_extend`(*buffer*)

Moves the caret down one page, extending the rectangular selection to the new
position.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.page_up"></a>
### `buffer.page_up`(*buffer*)

Moves the caret up one page.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.page_up_extend"></a>
### `buffer.page_up_extend`(*buffer*)

Moves the caret up one page, extending the selected text to the new position.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.page_up_rect_extend"></a>
### `buffer.page_up_rect_extend`(*buffer*)

Moves the caret up one page, extending the rectangular selection to the new
position.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.para_down"></a>
### `buffer.para_down`(*buffer*)

Moves the caret down one paragraph.
Paragraphs are surrounded by one or more blank lines.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.para_down_extend"></a>
### `buffer.para_down_extend`(*buffer*)

Moves the caret down one paragraph, extending the selected text to the new
position.
Paragraphs are surrounded by one or more blank lines.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.para_up"></a>
### `buffer.para_up`(*buffer*)

Moves the caret up one paragraph.
Paragraphs are surrounded by one or more blank lines.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.para_up_extend"></a>
### `buffer.para_up_extend`(*buffer*)

Moves the caret up one paragraph, extending the selected text to the new
position.
Paragraphs are surrounded by one or more blank lines.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.paste"></a>
### `buffer.paste`(*buffer*)

Pastes the clipboard's contents into the buffer, replacing any selected text
according to `buffer.multi_paste`.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.position_after"></a>
### `buffer.position_after`(*buffer, pos*)

Returns the position of the character after position *pos* (taking multi-byte
characters into account), or `buffer.length` if there is no character after
*pos*.

Parameters:

* *`buffer`*: A buffer.
* *`pos`*: The position in *buffer* to get the position after from.

<a id="buffer.position_before"></a>
### `buffer.position_before`(*buffer, pos*)

Returns the position of the character before position *pos* (taking
multi-byte characters into account), or `0` if there is no character before
*pos*.

Parameters:

* *`buffer`*: A buffer.
* *`pos`*: The position in *buffer* to get the position before from.

Return:

* number

<a id="buffer.position_from_line"></a>
### `buffer.position_from_line`(*buffer, line*)

Returns the position at the beginning of line number *line*.
Returns `-1` if *line* is greater than `buffer.line_count`.

Parameters:

* *`buffer`*: A buffer.
* *`line`*: The line number in *buffer* to get the beginning position for.

Return:

* number

<a id="buffer.position_relative"></a>
### `buffer.position_relative`(*buffer, pos, n*)

Returns the position *n* characters before or after position *pos* (taking
multi-byte characters into account).
Returns `0` if the position is less than 0 or greater than `buffer.length`.

Parameters:

* *`buffer`*: A buffer.
* *`pos`*: The position in *buffer* to get the relative position from.
* *`n`*: The relative number of characters to get the position for. A
  negative number indicates a position before while a positive number
  indicates a position after.

Return:

* number

<a id="buffer.redo"></a>
### `buffer.redo`(*buffer*)

Redoes the next undone action.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.register_image"></a>
### `buffer.register_image`(*buffer, type, xpm\_data*)

Registers XPM image *xpm_data* to type number *type* for use in
autocompletion and user lists.

Parameters:

* *`buffer`*: A buffer.
* *`type`*: Integer type to register the image with.
* *`xpm_data`*: The XPM data as described in `buffer.marker_define_pixmap()`.

<a id="buffer.register_rgba_image"></a>
### `buffer.register_rgba_image`(*buffer, type, pixels*)

Registers RGBA image *pixels* to type number *type* for use in autocompletion
and user lists.
The dimensions for *pixels* (`buffer.rgba_image_width` and
`buffer.rgba_image_height`) must have already been defined. *pixels* is a
sequence of 4 byte pixel values (red, blue, green, and alpha) defining the
image line by line starting at the top-left pixel.

Parameters:

* *`buffer`*: A buffer.
* *`type`*: Integer type to register the image with.
* *`pixels`*: The RGBA data as described in
  `buffer.marker_define_rgba_image()`.

<a id="buffer.replace_sel"></a>
### `buffer.replace_sel`(*buffer, text*)

Replaces the selected text with string *text*, scrolling the caret into view.

Parameters:

* *`buffer`*: A buffer.
* *`text`*: The text to replace the selected text with.

<a id="buffer.replace_target"></a>
### `buffer.replace_target`(*buffer, text*)

Replaces the text in the target range with string *text* sans modifying any
selections or scrolling the view.
Setting the target and calling this function with an empty string is another
way to delete text.

Parameters:

* *`buffer`*: A buffer.
* *`text`*: The text to replace the target range with.

Return:

* number

<a id="buffer.replace_target_re"></a>
### `buffer.replace_target_re`(*buffer, text*)

Replaces the text in the target range with string *text* but first replaces
any "\d" sequences with the text of capture number *d* from the regular
expression (or the entire match for *d* = 0), and then returns the
replacement text's length.

Parameters:

* *`buffer`*: A buffer.
* *`text`*: The text to replace the target range with.

Return:

* number

<a id="buffer.rotate_selection"></a>
### `buffer.rotate_selection`(*buffer*)

Designates the next additional selection to be the main selection.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.scroll_caret"></a>
### `buffer.scroll_caret`(*buffer*)

Scrolls the caret into view based on the policies previously defined in
`buffer.set_x_caret_policy()` and `buffer.set_y_caret_policy()`.

Parameters:

* *`buffer`*: A buffer.

See also:

* [`buffer.set_x_caret_policy`](#buffer.set_x_caret_policy)
* [`buffer.set_y_caret_policy`](#buffer.set_y_caret_policy)

<a id="buffer.scroll_range"></a>
### `buffer.scroll_range`(*buffer, secondary\_pos, primary\_pos*)

Scrolls into view the range of text between positions *primary_pos* and
*secondary_pos*, with priority given to *primary_pos*.
Similar to `buffer.scroll_caret()`, but with *primary_pos* instead of
`buffer.current_pos`.
This is useful for scrolling search results into view.

Parameters:

* *`buffer`*: A buffer.
* *`secondary_pos`*: The secondary range position to scroll into view.
* *`primary_pos`*: The primary range position to scroll into view.

<a id="buffer.scroll_to_end"></a>
### `buffer.scroll_to_end`(*buffer*)

Scrolls to the end of the buffer without moving the caret.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.scroll_to_start"></a>
### `buffer.scroll_to_start`(*buffer*)

Scrolls to the beginning of the buffer without moving the caret.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.search_anchor"></a>
### `buffer.search_anchor`(*buffer*)

Anchors the position that `buffer.search_next()` and `buffer.search_prev()`
start at to the beginning of the current selection or caret position.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.search_in_target"></a>
### `buffer.search_in_target`(*buffer, text*)

Searches for the first occurrence of string *text* in the target range
bounded by `buffer.target_start` and `buffer.target_end` using search flags
`buffer.search_flags` and, if found, sets the new target range to that
occurrence, returning its position or `-1` if *text* was not found.

Parameters:

* *`buffer`*: A buffer.
* *`text`*: The text to search the target range for.

Return:

* number

See also:

* [`buffer.search_flags`](#buffer.search_flags)

<a id="buffer.search_next"></a>
### `buffer.search_next`(*buffer, flags, text*)

Searches for and selects the first occurrence of string *text* starting at
the search anchor using search flags *flags*, returning that occurrence's
position or `-1` if *text* was not found.
Selected text is not scrolled into view.

Parameters:

* *`buffer`*: A buffer.
* *`flags`*: The search flags to use. See `buffer.search_flags`.
* *`text`*: The text to search for.

Return:

* number

See also:

* [`buffer.search_flags`](#buffer.search_flags)

<a id="buffer.search_prev"></a>
### `buffer.search_prev`(*buffer, flags, text*)

Searches for and selects the last occurrence of string *text* before the
search anchor using search flags *flags*, returning that occurrence's
position or `-1` if *text* was not found.

Parameters:

* *`buffer`*: A buffer.
* *`flags`*: The search flags to use. See `buffer.search_flags`.
* *`text`*: The text to search for.

Return:

* number

See also:

* [`buffer.search_flags`](#buffer.search_flags)

<a id="buffer.select_all"></a>
### `buffer.select_all`(*buffer*)

Selects all of the buffer's text without scrolling the view.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.selection_duplicate"></a>
### `buffer.selection_duplicate`(*buffer*)

Duplicates the selected text to its right.
If no text is selected, duplicates the current line on a new line below.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.set_chars_default"></a>
### `buffer.set_chars_default`(*buffer*)

Resets `buffer.word_chars`, `buffer.whitespace_chars`, and
`buffer.punctuation_chars` to their respective defaults.

Parameters:

* *`buffer`*: A buffer.

See also:

* [`buffer.word_chars`](#buffer.word_chars)
* [`buffer.whitespace_chars`](#buffer.whitespace_chars)
* [`buffer.punctuation_chars`](#buffer.punctuation_chars)

<a id="buffer.set_empty_selection"></a>
### `buffer.set_empty_selection`(*buffer, pos*)

Moves the caret to position *pos* without scrolling the view and removes any
selections.

Parameters:

* *`buffer`*: A buffer
* *`pos`*: The position in *buffer* to move to.

<a id="buffer.set_encoding"></a>
### `buffer.set_encoding`(*buffer, encoding*)

Converts the current buffer's contents to encoding *encoding*.

Parameters:

* *`buffer`*: A buffer.
* *`encoding`*: The string encoding to set. Valid encodings are ones that GNU
  iconv accepts.

Usage:

* `buffer:set_encoding('ASCII')`

<a id="buffer.set_fold_margin_colour"></a>
### `buffer.set_fold_margin_colour`(*buffer, use\_setting, color*)

Overrides the fold margin's default color with color *color*, in "0xBBGGRR"
format,
if *use_setting* is `true`.

Parameters:

* *`buffer`*: A buffer.
* *`use_setting`*: Whether or not to use *color*.
* *`color`*: The color in "0xBBGGRR" format.

<a id="buffer.set_fold_margin_hi_colour"></a>
### `buffer.set_fold_margin_hi_colour`(*buffer, use\_setting, color*)

Overrides the fold margin's default highlight color with color *color*, in
"0xBBGGRR" format, if *use_setting* is `true`.

Parameters:

* *`buffer`*: A buffer.
* *`use_setting`*: Whether or not to use *color*.
* *`color`*: The color in "0xBBGGRR" format.

<a id="buffer.set_lexer"></a>
### `buffer.set_lexer`(*buffer, lexer*)

Associates lexer name *lexer* or the auto-detected lexer name with the buffer
and then loads the appropriate language module if that module exists.

Parameters:

* *`buffer`*: A buffer.
* *`lexer`*: Optional string lexer name to set. If `nil`, attempts to
  auto-detect the buffer's lexer.

Usage:

* `buffer:set_lexer('lexer_name')`

<a id="buffer.set_save_point"></a>
### `buffer.set_save_point`(*buffer*)

Indicates the buffer has no unsaved changes.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.set_sel"></a>
### `buffer.set_sel`(*buffer, start\_pos, end\_pos*)

Selects the range of text between positions *start_pos* and *end_pos*,
scrolling the selected text into view.

Parameters:

* *`buffer`*: A buffer.
* *`start_pos`*: The start position of the range of text in *buffer* to
  select. If negative, it means the end of the buffer.
* *`end_pos`*: The end position of the range of text in *buffer* to select.
  If negative, it means remove any selection (i.e. set the `anchor` to the
  same position as `current_pos`).

<a id="buffer.set_sel_back"></a>
### `buffer.set_sel_back`(*buffer, use\_setting, color*)

Overrides the selection's default background color with color *color*, in
"0xBBGGRR" format, if *use_setting* is `true`.
Overwrites any existing `buffer.additional_sel_back` color.

Parameters:

* *`buffer`*: A buffer.
* *`use_setting`*: Whether or not to use *color*.
* *`color`*: The color in "0xBBGGRR" format.

<a id="buffer.set_sel_fore"></a>
### `buffer.set_sel_fore`(*buffer, use\_setting, color*)

Overrides the selection's default foreground color with color *color*, in
"0xBBGGRR" format, if *use_setting* is `true`.
Overwrites any existing `buffer.additional_sel_fore` color.

Parameters:

* *`buffer`*: A buffer.
* *`use_setting`*: Whether or not to use *color*.
* *`color`*: The color in "0xBBGGRR" format.

<a id="buffer.set_selection"></a>
### `buffer.set_selection`(*buffer, end\_pos, start\_pos*)

Selects the range of text between positions *start_pos* to *end_pos*,
removing all other selections.

Parameters:

* *`buffer`*: A buffer.
* *`end_pos`*: The caret position of the range of text to select in *buffer*.
* *`start_pos`*: The anchor position of the range of text to select in
  *buffer*.

<a id="buffer.set_styling"></a>
### `buffer.set_styling`(*buffer, length, style*)

Assigns style number *style*, in the range from `0` to `255`, to the next
*length* characters, starting from the current styling position, and
increments the styling position by *length*.
[`buffer:start_styling`](#buffer.start_styling) should be called before `buffer:set_styling()`.

Parameters:

* *`buffer`*: A buffer.
* *`length`*: The number of characters to style.
* *`style`*: The style number to set.

<a id="buffer.set_target_range"></a>
### `buffer.set_target_range`(*buffer, start\_pos, end\_pos*)

Defines the target range's beginning and end positions as *start_pos* and
*end_pos*, respectively.

Parameters:

* *`buffer`*: A buffer.
* *`start_pos`*: The position of the beginning of the target range.
* *`end_pos`*: The position of the end of the target range.

<a id="buffer.set_text"></a>
### `buffer.set_text`(*buffer, text*)

Replaces the buffer's text with string *text*.

Parameters:

* *`buffer`*: A buffer.
* *`text`*: The text to set.

<a id="buffer.set_visible_policy"></a>
### `buffer.set_visible_policy`(*buffer, policy, y*)

Defines scrolling policy bit-mask *policy* as the policy for keeping the
caret *y* number of lines away from the vertical margins as
`buffer.ensure_visible_enforce_policy()` redisplays hidden or folded lines.
It is similar in operation to `buffer.set_y_caret_policy()`.

Parameters:

* *`buffer`*: A buffer.
* *`policy`*: The combination of `buffer.VISIBLE_SLOP` and
  `buffer.VISIBLE_STRICT` policy flags to set.
* *`y`*: The number of lines from the vertical margins to keep the caret.

<a id="buffer.set_whitespace_back"></a>
### `buffer.set_whitespace_back`(*buffer, use\_setting, color*)

Overrides the background color of whitespace with color *color*, in
"0xBBGGRR" format, if *use_setting* is `true`.

Parameters:

* *`buffer`*: A buffer.
* *`use_setting`*: Whether or not to use *color*.
* *`color`*: The color in "0xBBGGRR" format.

<a id="buffer.set_whitespace_fore"></a>
### `buffer.set_whitespace_fore`(*buffer, use\_setting, color*)

Overrides the foreground color of whitespace with color *color*, in
"0xBBGGRR" format, if *use_setting* is `true`.

Parameters:

* *`buffer`*: 
* *`use_setting`*: Whether or not to use *color*.
* *`color`*: The color in "0xBBGGRR" format.

<a id="buffer.set_x_caret_policy"></a>
### `buffer.set_x_caret_policy`(*buffer, policy, x*)

Defines scrolling policy bit-mask *policy* as the policy for keeping the
caret *x* number of pixels away from the horizontal margins.

Parameters:

* *`buffer`*: A buffer.
* *`policy`*: The combination of `buffer.CARET_SLOP`, `buffer.CARET_STRICT`,
  `buffer.CARET_EVEN`, and `buffer.CARET_JUMPS` policy flags to set.
* *`x`*: The number of pixels from the horizontal margins to keep the caret.

<a id="buffer.set_y_caret_policy"></a>
### `buffer.set_y_caret_policy`(*buffer, policy, y*)

Defines scrolling policy bit-mask *policy* as the policy for keeping the
caret *y* number of lines away from the vertical margins.

Parameters:

* *`buffer`*: A buffer.
* *`policy`*: The combination of `buffer.CARET_SLOP`, `buffer.CARET_STRICT`,
  `buffer.CARET_EVEN`, and `buffer.CARET_JUMPS` policy flags to set.
* *`y`*: The number of lines from the vertical margins to keep the caret.

<a id="buffer.show_lines"></a>
### `buffer.show_lines`(*buffer, start\_line, end\_line*)

Shows the range of lines between line numbers *start_line* to *end_line*.
This has no effect on fold levels or fold flags and the first line cannot be
hidden.

Parameters:

* *`buffer`*: A buffer.
* *`start_line`*: The start line of the range of lines in *buffer* to show.
* *`end_line`*: The end line of the range of lines in *buffer* to show.

<a id="buffer.start_styling"></a>
### `buffer.start_styling`(*buffer, position, style\_mask*)

Begins styling at position *position* with styling bit-mask *style_mask*.
*style_mask* specifies which style bits can be set with
`buffer.set_styling()`.

Parameters:

* *`buffer`*: A buffer.
* *`position`*: The position in *buffer* to start styling at.
* *`style_mask`*: The bit mask of style bits that can be set when styling.

Usage:

* `buffer:start_styling(0, 0xFF)`

See also:

* [`buffer.set_styling`](#buffer.set_styling)

<a id="buffer.stuttered_page_down"></a>
### `buffer.stuttered_page_down`(*buffer*)

Moves the caret to the bottom of the page or, if already there, down one
page.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.stuttered_page_down_extend"></a>
### `buffer.stuttered_page_down_extend`(*buffer*)

Like `buffer.stuttered_page_down()`, but extends the selected text to the new
position.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.stuttered_page_up"></a>
### `buffer.stuttered_page_up`(*buffer*)

Moves the caret to the top of the page or, if already there, up one page.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.stuttered_page_up_extend"></a>
### `buffer.stuttered_page_up_extend`(*buffer*)

Like `buffer.stuttered_page_up()`, but extends the selected text to the new
position.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.style_clear_all"></a>
### `buffer.style_clear_all`(*buffer*)

Reverts all styles to having the same properties as `buffer.STYLE_DEFAULT`.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.style_reset_default"></a>
### `buffer.style_reset_default`(*buffer*)

Resets `buffer.STYLE_DEFAULT` to its initial state.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.swap_main_anchor_caret"></a>
### `buffer.swap_main_anchor_caret`(*buffer*)

Swaps the main selection's beginning and end positions.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.tab"></a>
### `buffer.tab`(*buffer*)

Indents the text on the selected lines or types a Tab character ("\t") at
the caret position.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.target_from_selection"></a>
### `buffer.target_from_selection`(*buffer*)

Defines the target range's beginning and end positions as the beginning and
end positions of the main selection, respectively.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.target_whole_document"></a>
### `buffer.target_whole_document`(*buffer*)

Defines the target range's beginning and end positions as the beginning and
end positions of the document, respectively.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.text_height"></a>
### `buffer.text_height`(*buffer, line*)

Returns the pixel height of line number *line*.

Parameters:

* *`buffer`*: A buffer.
* *`line`*: The line number in *buffer* to get the pixel height of.

Return:

* number

<a id="buffer.text_range"></a>
### `buffer.text_range`(*buffer, start\_pos, end\_pos*)

Returns the range of text between positions *start_pos* and *end_pos*.

Parameters:

* *`buffer`*: A buffer.
* *`start_pos`*: The start position of the range of text to get in *buffer*.
* *`end_pos`*: The end position of the range of text to get in *buffer*.

<a id="buffer.text_width"></a>
### `buffer.text_width`(*buffer, style\_num, text*)

Returns the pixel width string *text* would have when styled with style
number *style_num*, in the range of `0` to `255`.

Parameters:

* *`buffer`*: A buffer.
* *`style_num`*: The style number between `0` and `255` to use.
* *`text`*: The text to measure the width of.

Return:

* number

<a id="buffer.toggle_caret_sticky"></a>
### `buffer.toggle_caret_sticky`(*buffer*)

Cycles between `buffer.caret_sticky` option settings `buffer.CARETSTICKY_ON`
and `buffer.CARETSTICKY_OFF`.

Parameters:

* *`buffer`*: A buffer.

See also:

* [`buffer.caret_sticky`](#buffer.caret_sticky)

<a id="buffer.toggle_fold"></a>
### `buffer.toggle_fold`(*buffer, line*)

Toggles the fold point on line number *line* between expanded (where all of
its child lines are displayed) and contracted (where all of its child lines
are hidden).

Parameters:

* *`buffer`*: A buffer.
* *`line`*: The line number in *buffer* to toggle the fold on.

<a id="buffer.toggle_fold_display_text"></a>
### `buffer.toggle_fold_display_text`(*buffer, line, text*)

Toggles a fold point on line number *line* between expanded (where all of
its child lines are displayed) and contracted (where all of its child lines
are hidden), and shows string *text* after the line.
*text* is drawn with style number `buffer.STYLE_FOLDDISPLAYTEXT`.

Parameters:

* *`buffer`*: A buffer.
* *`line`*: The line number in *buffer* to toggle the fold on and display
  *text* after.
* *`text`*: The text to display after the line.

<a id="buffer.undo"></a>
### `buffer.undo`(*buffer*)

Undoes the most recent action.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.upper_case"></a>
### `buffer.upper_case`(*buffer*)

Converts the selected text to upper case letters.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.user_list_show"></a>
### `buffer.user_list_show`(*buffer, id, items*)

Displays a user list identified by list identifier number *id* and
constructed from string *items* (whose items are delimited by
`buffer.auto_c_separator` characters).
The sorted order of *items* (`buffer.auto_c_order`) must have already been
defined. When the user selects an item, *id* is sent in a
`USER_LIST_SELECTION` event along with the selection.

Parameters:

* *`buffer`*: A buffer.
* *`id`*: The list identifier number greater than zero to use.
* *`items`*: The sorted string of words to show, separated by
  `buffer.auto_c_separator` characters (initially spaces).

See also:

* [`_SCINTILLA.next_user_list_type`](#_SCINTILLA.next_user_list_type)
* [`events.USER_LIST_SELECTION`](#events.USER_LIST_SELECTION)

<a id="buffer.vc_home"></a>
### `buffer.vc_home`(*buffer*)

Moves the caret to the first visible character on the current line or, if
already there, to the beginning of the current line.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.vc_home_display"></a>
### `buffer.vc_home_display`(*buffer*)

Moves the caret to the first visible character on the current wrapped line
or, if already there, to the beginning of the current wrapped line.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.vc_home_display_extend"></a>
### `buffer.vc_home_display_extend`(*buffer*)

Like `buffer.vc_home_display()`, but extends the selected text to the new
position.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.vc_home_extend"></a>
### `buffer.vc_home_extend`(*buffer*)

Like `buffer.vc_home()`, but extends the selected text to the new position.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.vc_home_rect_extend"></a>
### `buffer.vc_home_rect_extend`(*buffer*)

Like `buffer.vc_home()`, but extends the rectangular selection to the new
position.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.vc_home_wrap"></a>
### `buffer.vc_home_wrap`(*buffer*)

Moves the caret to the first visible character on the current wrapped line
or, if already there, to the beginning of the actual line.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.vc_home_wrap_extend"></a>
### `buffer.vc_home_wrap_extend`(*buffer*)

Like `buffer.vc_home_wrap()`, but extends the selected text to the new
position.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.vertical_centre_caret"></a>
### `buffer.vertical_centre_caret`(*buffer*)

Centers current line in the view.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.visible_from_doc_line"></a>
### `buffer.visible_from_doc_line`(*buffer, line*)

Returns the displayed line number of actual line number *line*, taking hidden
lines into account, or `-1` if *line* is outside the range of lines in the
buffer.
Lines can occupy more than one display line if they wrap.

Parameters:

* *`buffer`*: A buffer.
* *`line`*: The line number in *buffer* to use.

Return:

* number

<a id="buffer.word_end_position"></a>
### `buffer.word_end_position`(*buffer, pos, only\_word\_chars*)

Returns the position of the end of the word at position *pos*.
`buffer.word_chars` contains the set of characters that constitute words. If
*pos* has a non-word character to its right and *only_word_chars* is `false`,
returns the first word character's position.

Parameters:

* *`buffer`*: A buffer.
* *`pos`*: The position in *buffer* of the word.
* *`only_word_chars`*: If `true`, stops searching at the first non-word
  character in the search direction. Otherwise, the first character in the
  search direction sets the type of the search as word or non-word and the
  search stops at the first non-matching character. Searches are also
  terminated by the start or end of the buffer.

<a id="buffer.word_left"></a>
### `buffer.word_left`(*buffer*)

Moves the caret left one word.
`buffer.word_chars` contains the set of characters that constitute words.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.word_left_end"></a>
### `buffer.word_left_end`(*buffer*)

Moves the caret left one word, positioning it at the end of the previous
word.
`buffer.word_chars` contains the set of characters that constitute words.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.word_left_end_extend"></a>
### `buffer.word_left_end_extend`(*buffer*)

Like `buffer.word_left_end()`, but extends the selected text to the new
position.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.word_left_extend"></a>
### `buffer.word_left_extend`(*buffer*)

Moves the caret left one word, extending the selected text to the new
position.
`buffer.word_chars` contains the set of characters that constitute words.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.word_part_left"></a>
### `buffer.word_part_left`(*buffer*)

Moves the caret to the previous part of the current word.
Word parts are delimited by underscore characters or changes in
capitalization.
`buffer.word_chars` contains the set of characters that constitute words.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.word_part_left_extend"></a>
### `buffer.word_part_left_extend`(*buffer*)

Moves the caret to the previous part of the current word, extending the
selected text to the new position.
Word parts are delimited by underscore characters or changes in
capitalization.
`buffer.word_chars` contains the set of characters that constitute words.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.word_part_right"></a>
### `buffer.word_part_right`(*buffer*)

Moves the caret to the next part of the current word.
Word parts are delimited by underscore characters or changes in
capitalization.
`buffer.word_chars` contains the set of characters that constitute words.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.word_part_right_extend"></a>
### `buffer.word_part_right_extend`(*buffer*)

Moves the caret to the next part of the current word, extending the selected
text to the new position.
Word parts are delimited by underscore characters or changes in
capitalization.
`buffer.word_chars` contains the set of characters that constitute words.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.word_right"></a>
### `buffer.word_right`(*buffer*)

Moves the caret right one word.
`buffer.word_chars` contains the set of characters that constitute words.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.word_right_end"></a>
### `buffer.word_right_end`(*buffer*)

Moves the caret right one word, positioning it at the end of the current
word.
`buffer.word_chars` contains the set of characters that constitute words.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.word_right_end_extend"></a>
### `buffer.word_right_end_extend`(*buffer*)

Like `buffer.word_right_end()`, but extends the selected text to the new
position.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.word_right_extend"></a>
### `buffer.word_right_extend`(*buffer*)

Moves the caret right one word, extending the selected text to the new
position.
`buffer.word_chars` contains the set of characters that constitute words.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.word_start_position"></a>
### `buffer.word_start_position`(*buffer, pos, only\_word\_chars*)

Returns the position of the beginning of the word at position *pos*.
`buffer.word_chars` contains the set of characters that constitute words. If
*pos* has a non-word character to its left and *only_word_chars* is `false`,
returns the last word character's position.

Parameters:

* *`buffer`*: A buffer.
* *`pos`*: The position in *buffer* of the word.
* *`only_word_chars`*: If `true`, stops searching at the first non-word
  character in the search direction. Otherwise, the first character in the
  search direction sets the type of the search as word or non-word and the
  search stops at the first non-matching character. Searches are also
  terminated by the start or end of the buffer.

<a id="buffer.wrap_count"></a>
### `buffer.wrap_count`(*buffer, line*)

Returns the number of wrapped lines needed to fully display line number
*line*.

Parameters:

* *`buffer`*: A buffer.
* *`line`*: The line number in *buffer* to use.

Return:

* number

<a id="buffer.zoom_in"></a>
### `buffer.zoom_in`(*buffer*)

Increases the size of all fonts by one point, up to 20.

Parameters:

* *`buffer`*: A buffer.

<a id="buffer.zoom_out"></a>
### `buffer.zoom_out`(*buffer*)

Decreases the size of all fonts by one point, down to -10.

Parameters:

* *`buffer`*: A buffer.


- - -

<a id="events"></a>
# The `events` Module

- - -

Textadept's core event structure and handlers.

Textadept emits events when you do things like create a new buffer, press a
key, click on a menu, etc. You can even emit events yourself using Lua. Each
event has a set of event handlers, which are simply Lua functions called in
the order they were connected to an event. For example, if you created a
module that needs to do something each time Textadept creates a new buffer,
connect a Lua function to the [`events.BUFFER_NEW`](#events.BUFFER_NEW) event:

    events.connect(events.BUFFER_NEW, function()
      -- Do something here.
    end)

Events themselves are nothing special. You do not have to declare one before
using it. Events are simply strings containing arbitrary event names. When
either you or Textadept emits an event, Textadept runs all event handlers
connected to the event, passing any given arguments to the event's handler
functions. If an event handler explicitly returns a `true` or `false` boolean
value, Textadept will not call subsequent handlers. This is useful if you
want to stop the propagation of an event like a keypress if your event
handler handled it.


## Fields defined by `events`

<a id="events.APPLEEVENT_ODOC"></a>
### `events.APPLEEVENT_ODOC` (string)

Emitted when Mac OSX tells Textadept to open a file.
  Arguments:

  * _`uri`_: The UTF-8-encoded URI to open.

<a id="events.AUTO_C_CANCELED"></a>
### `events.AUTO_C_CANCELED` (string)

Emitted when canceling an autocompletion or user list.

<a id="events.AUTO_C_CHAR_DELETED"></a>
### `events.AUTO_C_CHAR_DELETED` (string)

Emitted after deleting a character while an autocompletion or user list is
  active.

<a id="events.AUTO_C_COMPLETED"></a>
### `events.AUTO_C_COMPLETED` (string)

Emitted after inserting an item from an autocompletion list into the
  buffer.
  Arguments:

  * _`text`_: The selection's text.
  * _`position`_: The autocompleted word's beginning position.

<a id="events.AUTO_C_SELECTION"></a>
### `events.AUTO_C_SELECTION` (string)

Emitted after selecting an item from an autocompletion list, but before
  inserting that item into the buffer.
  Automatic insertion can be cancelled by calling
  [`buffer:auto_c_cancel()`](#buffer.auto_c_cancel) before returning from the event handler.
  Arguments:

  * _`text`_: The selection's text.
  * _`position`_: The autocompleted word's beginning position.

<a id="events.BUFFER_AFTER_SWITCH"></a>
### `events.BUFFER_AFTER_SWITCH` (string)

Emitted right after switching to another buffer.
  Emitted by [`view.goto_buffer()`](#view.goto_buffer).

<a id="events.BUFFER_BEFORE_SWITCH"></a>
### `events.BUFFER_BEFORE_SWITCH` (string)

Emitted right before switching to another buffer.
  Emitted by [`view.goto_buffer()`](#view.goto_buffer).

<a id="events.BUFFER_DELETED"></a>
### `events.BUFFER_DELETED` (string)

Emitted after deleting a buffer.
  Emitted by [`buffer.delete()`](#buffer.delete).

<a id="events.BUFFER_NEW"></a>
### `events.BUFFER_NEW` (string)

Emitted after creating a new buffer.
  Emitted on startup and by [`buffer.new()`](#buffer.new).

<a id="events.CALL_TIP_CLICK"></a>
### `events.CALL_TIP_CLICK` (string)

Emitted when clicking on a calltip.
  Arguments:

  * _`position`_: `1` if the up arrow was clicked, 2 if the down arrow was
    clicked, and 0 otherwise.

<a id="events.CHAR_ADDED"></a>
### `events.CHAR_ADDED` (string)

Emitted after the user types a text character into the buffer.
  Arguments:

  * _`code`_: The text character's character code.

<a id="events.CSI"></a>
### `events.CSI` (string)

Emitted when the terminal version receives an unrecognized CSI sequence.
  Arguments:

  * _`cmd`_: The 24-bit CSI command value. The lowest byte contains the
    command byte. The second lowest byte contains the leading byte, if any
    (e.g. '?'). The third lowest byte contains the intermediate byte, if any
    (e.g. '$').
  * _`args`_: Table of numeric arguments of the CSI sequence.

<a id="events.DOUBLE_CLICK"></a>
### `events.DOUBLE_CLICK` (string)

Emitted after double-clicking the mouse button.
  Arguments:

  * _`position`_: The position double-clicked.
  * _`line`_: The line number of the position double-clicked.
  * _`modifiers`_: A bit-mask of any modifier keys used: `buffer.MOD_CTRL`,
    `buffer.MOD_SHIFT`, `buffer.MOD_ALT`, and `buffer.MOD_META`.
    Note: If you set `buffer.rectangular_selection_modifier` to
    `buffer.MOD_CTRL`, the "Control" modifier is reported as *both* "Control"
    and "Alt" due to a Scintilla limitation with GTK+.

<a id="events.DWELL_END"></a>
### `events.DWELL_END` (string)

Emitted after `DWELL_START` when the user moves the mouse, presses a key,
  or scrolls the view.
  Arguments:

  * _`position`_: The position closest to *x* and *y*.
  * _`x`_: The x-coordinate of the mouse in the view.
  * _`y`_: The y-coordinate of the mouse in the view.

<a id="events.DWELL_START"></a>
### `events.DWELL_START` (string)

Emitted when the mouse is stationary for [`buffer.mouse_dwell_time`](#buffer.mouse_dwell_time)
  milliseconds.
  Arguments:

  * _`position`_: The position closest to *x* and *y*.
  * _`x`_: The x-coordinate of the mouse in the view.
  * _`y`_: The y-coordinate of the mouse in the view.

<a id="events.ERROR"></a>
### `events.ERROR` (string)

Emitted when an error occurs.
  Arguments:

  * _`text`_: The error message text.

<a id="events.FIND"></a>
### `events.FIND` (string)

Emitted to find text via the Find & Replace Pane.
  Arguments:

  * _`text`_: The text to search for.
  * _`next`_: Whether or not to search forward.

<a id="events.FOCUS"></a>
### `events.FOCUS` (string)

Emitted when Textadept receives focus.
  This event is never emitted when Textadept is running in the terminal.

<a id="events.INDICATOR_CLICK"></a>
### `events.INDICATOR_CLICK` (string)

Emitted when clicking the mouse on text that has an indicator present.
  Arguments:

  * _`position`_: The clicked text's position.
  * _`modifiers`_: A bit-mask of any modifier keys used: `buffer.MOD_CTRL`,
    `buffer.MOD_SHIFT`, `buffer.MOD_ALT`, and `buffer.MOD_META`.
    Note: If you set `buffer.rectangular_selection_modifier` to
    `buffer.MOD_CTRL`, the "Control" modifier is reported as *both* "Control"
    and "Alt" due to a Scintilla limitation with GTK+.

<a id="events.INDICATOR_RELEASE"></a>
### `events.INDICATOR_RELEASE` (string)

Emitted when releasing the mouse after clicking on text that has an
  indicator present.
  Arguments:

  * _`position`_: The clicked text's position.

<a id="events.INITIALIZED"></a>
### `events.INITIALIZED` (string)

Emitted after Textadept finishes initializing.

<a id="events.KEYPRESS"></a>
### `events.KEYPRESS` (string)

Emitted when pressing a key.
  If any handler returns `true`, the key is not inserted into the buffer.
  Arguments:

  * _`code`_: The numeric key code.
  * _`shift`_: The "Shift" modifier key is held down.
  * _`ctrl`_: The "Control" modifier key is held down.
  * _`alt`_: The "Alt"/"Option" modifier key is held down.
  * _`meta`_: The "Command" modifier key on Mac OSX is held down.
  * _`caps_lock`_: The "Caps Lock" modifier is on.

<a id="events.MARGIN_CLICK"></a>
### `events.MARGIN_CLICK` (string)

Emitted when clicking the mouse inside a sensitive margin.
  Arguments:

  * _`margin`_: The margin number clicked.
  * _`position`_: The beginning position of the clicked margin's line.
  * _`modifiers`_: A bit-mask of any modifier keys used: `buffer.MOD_CTRL`,
    `buffer.MOD_SHIFT`, `buffer.MOD_ALT`, and `buffer.MOD_META`.
    Note: If you set `buffer.rectangular_selection_modifier` to
    `buffer.MOD_CTRL`, the "Control" modifier is reported as *both* "Control"
    and "Alt" due to a Scintilla limitation with GTK+.

<a id="events.MENU_CLICKED"></a>
### `events.MENU_CLICKED` (string)

Emitted after selecting a menu item.
  Arguments:

  * _`menu_id`_: The numeric ID of the menu item, which was defined in
    [`ui.menu()`](#ui.menu).

<a id="events.MOUSE"></a>
### `events.MOUSE` (string)

Emitted by the terminal version for an unhandled mouse event.
  Arguments:

  * _`event`_: The mouse event: `buffer.MOUSE_PRESS`, `buffer.MOUSE_DRAG`, or
    `buffer.MOUSE_RELEASE`.
  * _`button`_: The mouse button number.
  * _`y`_: The y-coordinate of the mouse event, starting from 1.
  * _`x`_: The x-coordinate of the mouse event, starting from 1.
  * _`shift`_: The "Shift" modifier key is held down.
  * _`ctrl`_: The "Control" modifier key is held down.
  * _`alt`_: The "Alt"/"Option" modifier key is held down.

<a id="events.QUIT"></a>
### `events.QUIT` (string)

Emitted when quitting Textadept.
  When connecting to this event, connect with an index of 1 if the handler
  needs to run before Textadept closes all open buffers. If a handler returns
  `true`, Textadept does not quit. It is not recommended to return `false`
  from a quit handler, as that may interfere with Textadept's normal shutdown
  procedure.
  Emitted by [`quit()`](#quit).

<a id="events.REPLACE"></a>
### `events.REPLACE` (string)

Emitted to replace selected (found) text.
  Arguments:

  * _`text`_: The replacement text.

<a id="events.REPLACE_ALL"></a>
### `events.REPLACE_ALL` (string)

Emitted to replace all occurrences of found text.
  Arguments:

  * _`find_text`_: The text to search for.
  * _`repl_text`_: The replacement text.

<a id="events.RESET_AFTER"></a>
### `events.RESET_AFTER` (string)

Emitted after resetting the Lua state.
  Emitted by [`reset()`](#reset).

<a id="events.RESET_BEFORE"></a>
### `events.RESET_BEFORE` (string)

Emitted before resetting the Lua state.
  Emitted by [`reset()`](#reset).

<a id="events.RESUME"></a>
### `events.RESUME` (string)

Emitted when resuming Textadept from a suspended state.
  This event is only emitted by the terminal version.

<a id="events.SAVE_POINT_LEFT"></a>
### `events.SAVE_POINT_LEFT` (string)

Emitted after leaving a save point.

<a id="events.SAVE_POINT_REACHED"></a>
### `events.SAVE_POINT_REACHED` (string)

Emitted after reaching a save point.

<a id="events.SUSPEND"></a>
### `events.SUSPEND` (string)

Emitted when suspending Textadept. If any handler returns `true`, Textadept
  does not suspend.
  This event is only emitted by the terminal version.

<a id="events.TAB_CLICKED"></a>
### `events.TAB_CLICKED` (string)

Emitted when the user clicks on a buffer tab.
  When connecting to this event, connect with an index of 1 if the handler
  needs to run before Textadept switches between buffers.
  Arguments:

  * _`index`_: The numeric index of the clicked tab.

<a id="events.UPDATE_UI"></a>
### `events.UPDATE_UI` (string)

Emitted after the view is visually updated.
  Arguments:

  * _`updated`_: A bitmask of changes since the last update.

    + `buffer.UPDATE_CONTENT`
      Buffer contents, styling, or markers have changed.
    + `buffer.UPDATE_SELECTION`
      Buffer selection has changed (including caret movement).
    + `buffer.UPDATE_V_SCROLL`
      Buffer has scrolled vertically.
    + `buffer.UPDATE_H_SCROLL`
      Buffer has scrolled horizontally.

<a id="events.URI_DROPPED"></a>
### `events.URI_DROPPED` (string)

Emitted after dragging and dropping a URI into a view.
  Arguments:

  * _`text`_: The UTF-8-encoded URI dropped.

<a id="events.USER_LIST_SELECTION"></a>
### `events.USER_LIST_SELECTION` (string)

Emitted after selecting an item in a user list.
  Arguments:

  * _`id`_: The *id* from [`buffer.user_list_show()`](#buffer.user_list_show).
  * _`text`_: The selection's text.
  * _`position`_: The position the list was displayed at.

<a id="events.VIEW_AFTER_SWITCH"></a>
### `events.VIEW_AFTER_SWITCH` (string)

Emitted right after switching to another view.
  Emitted by [`ui.goto_view()`](#ui.goto_view).

<a id="events.VIEW_BEFORE_SWITCH"></a>
### `events.VIEW_BEFORE_SWITCH` (string)

Emitted right before switching to another view.
  Emitted by [`ui.goto_view()`](#ui.goto_view).

<a id="events.VIEW_NEW"></a>
### `events.VIEW_NEW` (string)

Emitted after creating a new view.
  Emitted on startup and by [`view.split()`](#view.split).


## Functions defined by `events`

<a id="events.connect"></a>
### `events.connect`(*event, f, index*)

Adds function *f* to the set of event handlers for event *event* at position
*index*.
If *index* not given, appends *f* to the set of handlers. *event* may be any
arbitrary string and does not need to have been previously defined.

Parameters:

* *`event`*: The string event name.
* *`f`*: The Lua function to connect to *event*.
* *`index`*: Optional index to insert the handler into.

Usage:

* `events.connect('my_event', function(msg) ui.print(msg) end)`

See also:

* [`events.disconnect`](#events.disconnect)

<a id="events.disconnect"></a>
### `events.disconnect`(*event, f*)

Removes function *f* from the set of handlers for event *event*.

Parameters:

* *`event`*: The string event name.
* *`f`*: The Lua function connected to *event*.

See also:

* [`events.connect`](#events.connect)

<a id="events.emit"></a>
### `events.emit`(*event, ...*)

Sequentially calls all handler functions for event *event* with the given
arguments.
*event* may be any arbitrary string and does not need to have been previously
defined. If any handler explicitly returns `true` or `false`, `emit()`
returns that value and ceases to call subsequent handlers. This is useful for
stopping the propagation of an event like a keypress after it has been
handled.

Parameters:

* *`event`*: The string event name.
* *`...`*: Arguments passed to the handler.

Usage:

* `events.emit('my_event', 'my message')`

Return:

* `true` or `false` if any handler explicitly returned such; `nil`
  otherwise.


- - -

<a id="io"></a>
# The `io` Module

- - -

Extends Lua's `io` library with Textadept functions for working with files.

## Fields defined by `io`

<a id="events.FILE_AFTER_SAVE"></a>
### `events.FILE_AFTER_SAVE` (string)

Emitted right after saving a file to disk.
  Emitted by [`io.save_file()`](#io.save_file) and [`io.save_file_as()`](#io.save_file_as).
  Arguments:

  * _`filename`_: The filename of the file being saved.
  * _`saved_as`_: Whether or not the file was saved under a different
    filename.

<a id="events.FILE_BEFORE_SAVE"></a>
### `events.FILE_BEFORE_SAVE` (string)

Emitted right before saving a file to disk.
  Emitted by [`io.save_file()`](#io.save_file).
  Arguments:

  * _`filename`_: The filename of the file being saved.

<a id="events.FILE_CHANGED"></a>
### `events.FILE_CHANGED` (string)

Emitted when Textadept detects that an open file was modified externally.
  When connecting to this event, connect with an index of 1 in order to
  override the default prompt to reload the file.
  Arguments:

  * _`filename`_: The filename externally modified.

<a id="events.FILE_OPENED"></a>
### `events.FILE_OPENED` (string)

Emitted after opening a file in a new buffer.
  Emitted by [`io.open_file()`](#io.open_file).
  Arguments:

  * _`filename`_: The opened file's filename.

<a id="io.quick_open_max"></a>
### `io.quick_open_max` (number)

The maximum number of files listed in the quick open dialog.
  The default value is `1000`.


## Functions defined by `io`

<a id="io.close_all_buffers"></a>
### `io.close_all_buffers`()

Closes all open buffers, prompting the user to continue if there are unsaved
buffers, and returns `true` if the user did not cancel.
No buffers are saved automatically. They must be saved manually.

Return:

* `true` if user did not cancel.

See also:

* [`io.close_buffer`](#io.close_buffer)

<a id="io.close_buffer"></a>
### `io.close_buffer`()

Closes the current buffer, prompting the user to continue if there are
unsaved changes, and returns `true` if the buffer was closed.

Return:

* `true` if the buffer was closed; `nil` otherwise.

<a id="io.get_project_root"></a>
### `io.get_project_root`(*path*)

Returns the root directory of the project that contains filesystem path
*path*.
In order to be recognized, projects must be under version control. Recognized
VCSes are Bazaar, Git, Mercurial, and SVN.

Parameters:

* *`path`*: Optional filesystem path to a project or a file contained within
  a project. The default value is the buffer's filename or the current
  working directory.

Return:

* string root or nil

<a id="io.open_file"></a>
### `io.open_file`(*filenames, encodings*)

Opens *filenames*, a string filename or list of filenames, or the
user-selected filenames.
Emits a `FILE_OPENED` event.

Parameters:

* *`filenames`*: Optional string filename or table of filenames to open. If
  `nil`, the user is prompted with a fileselect dialog.
* *`encodings`*: Optional string encoding or table of encodings file contents
  are in (one encoding per file). If `nil`, encoding auto-detection is
  attempted via `io.encodings`.

See also:

* [`events`](#events)

<a id="io.open_recent_file"></a>
### `io.open_recent_file`()

Prompts the user to select a recently opened file to be reopened.

See also:

* [`io.recent_files`](#io.recent_files)

<a id="io.quick_open"></a>
### `io.quick_open`(*paths, filter, opts*)

Prompts the user to select files to be opened from *paths*, a string
directory path or list of directory paths, using a filtered list dialog.
If *paths* is `nil`, uses the current project's root directory, which is
obtained from `io.get_project_root()`.
Files shown in the dialog do not match any pattern in either string or table
*filter* (or `lfs.default_filter` if *filter* is `nil`). A filter table
contains:

  + Lua patterns that match filenames to exclude.
  + Optional `folders` sub-table that contains patterns matching directories
    to exclude.
  + Optional `extensions` sub-table that contains raw file extensions to
    exclude.
  + Optional `symlink` flag that when `true`, excludes symlinked files (but
    not symlinked directories).
  + Optional `folders.symlink` flag that when `true`, excludes symlinked
    directories.

Any filter patterns starting with '!' exclude files and directories that do
not match the pattern that follows. The number of files in the list is capped
at `quick_open_max`.
If *filter* is `nil` and *paths* is ultimately a string, the filter from the
`io.quick_open_filters` table is used in place of `lfs.default_filter` if the
former exists.
*opts* is an optional table of additional options for
`ui.dialogs.filteredlist()`.

Parameters:

* *`paths`*: Optional string directory path or table of directory paths to
  search. The default value is the current project's root directory, if
  available.
* *`filter`*: Optional filter for files and directories to exclude. The
  default value is `lfs.default_filter` unless *paths* is a string and a
  filter for it is defined in `io.quick_open_filters`.
* *`opts`*: Optional table of additional options for
  `ui.dialogs.filteredlist()`.

Usage:

* `io.quick_open(buffer.filename:match('^.+/')) -- list all files in the
  current file's directory, subject to the default filter`
* `io.quick_open(io.get_current_project(), '!%.lua$') -- list all Lua
   files in the current project`
* `io.quick_open(io.get_current_project(), {folders = {'build'}}) -- list
  all non-built files in the current project`

See also:

* [`io.quick_open_filters`](#io.quick_open_filters)
* [`lfs.default_filter`](#lfs.default_filter)
* [`io.quick_open_max`](#io.quick_open_max)
* [`ui.dialogs.filteredlist`](#ui.dialogs.filteredlist)

<a id="io.reload_file"></a>
### `io.reload_file`()

Reloads the current buffer's file contents, discarding any changes.

<a id="io.save_all_files"></a>
### `io.save_all_files`()

Saves all unsaved buffers to their respective files.

See also:

* [`io.save_file`](#io.save_file)

<a id="io.save_file"></a>
### `io.save_file`()

Saves the current buffer to its file.
Emits `FILE_BEFORE_SAVE` and `FILE_AFTER_SAVE` events.

<a id="io.save_file_as"></a>
### `io.save_file_as`(*filename*)

Saves the current buffer to file *filename* or the user-specified filename.
Emits a `FILE_AFTER_SAVE` event.

Parameters:

* *`filename`*: Optional new filepath to save the buffer to. If `nil`, the
  user is prompted for one.


## Tables defined by `io`

<a id="io.encodings"></a>
### `io.encodings`

List of encodings to attempt to decode files as.
You should add to this list if you get a "Conversion failed" error when
trying to open a file whose encoding is not recognized. Valid encodings are
[GNU iconv's encodings][] and include:

  * European: ASCII, ISO-8859-{1,2,3,4,5,7,9,10,13,14,15,16}, KOI8-R, KOI8-U,
    KOI8-RU, CP{1250,1251,1252,1253,1254,1257}, CP{850,866,1131},
    Mac{Roman,CentralEurope,Iceland,Croatian,Romania},
    Mac{Cyrillic,Ukraine,Greek,Turkish}, Macintosh.
  * Semitic: ISO-8859-{6,8}, CP{1255,1256}, CP862, Mac{Hebrew,Arabic}.
  * Japanese: EUC-JP, SHIFT_JIS, CP932, ISO-2022-JP, ISO-2022-JP-2,
    ISO-2022-JP-1.
  * Chinese: EUC-CN, HZ, GBK, CP936, GB18030, EUC-TW, BIG5, CP950,
    BIG5-HKSCS, BIG5-HKSCS:2004, BIG5-HKSCS:2001, BIG5-HKSCS:1999,
    ISO-2022-CN, ISO-2022-CN-EXT.
  * Korean: EUC-KR, CP949, ISO-2022-KR, JOHAB.
  * Armenian: ARMSCII-8.
  * Georgian: Georgian-Academy, Georgian-PS.
  * Tajik: KOI8-T.
  * Kazakh: PT154, RK1048.
  * Thai: ISO-8859-11, TIS-620, CP874, MacThai.
  * Laotian: MuleLao-1, CP1133.
  * Vietnamese: VISCII, TCVN, CP1258.
  * Unicode: UTF-8, UCS-2, UCS-2BE, UCS-2LE, UCS-4, UCS-4BE, UCS-4LE, UTF-16,
    UTF-16BE, UTF-16LE, UTF-32, UTF-32BE, UTF-32LE, UTF-7, C99, JAVA.

[GNU iconv's encodings]: http://www.gnu.org/software/libiconv/

Usage:

* `io.encodings[#io.encodings + 1] = 'UTF-16'`

<a id="io.quick_open_filters"></a>
### `io.quick_open_filters`

Map of file paths to filters used by `io.quick_open()`.

See also:

* [`io.quick_open`](#io.quick_open)

<a id="io.recent_files"></a>
### `io.recent_files`

List of recently opened files, the most recent being towards the top.

- - -

<a id="keys"></a>
# The `keys` Module

- - -

Manages key bindings in Textadept.


<a id="keys.Overview"></a>

## Overview

Define key bindings in the global `keys` table in key-value pairs. Each pair
consists of either a string key sequence and its associated command, a string
lexer language (from the *lexers/* directory) with a table of key sequences
and commands, a string key mode with a table of key sequences and commands,
or a key sequence with a table of more sequences and commands. The latter is
part of what is called a "key chain", to be discussed below. When searching
for a command to run based on a key sequence, Textadept considers key
bindings in the current key mode to have priority. If no key mode is active,
language-specific key bindings have priority, followed by the ones in the
global table. This means if there are two commands with the same key
sequence, Textadept runs the language-specific one. However, if the command
returns the boolean value `false`, Textadept also runs the lower-priority
command. (This is useful for language modules to override commands like
autocompletion, but fall back to word autocompletion if the first command
fails.)


<a id="keys.Key.Sequences"></a>

## Key Sequences

Key sequences are strings built from an ordered combination of modifier keys
and the key's inserted character. Modifier keys are "Control", "Shift", and
"Alt" on Windows, Linux, BSD, and in curses. On Mac OSX they are "Control"
(`^`), "Alt/Option" (`⌥`), "Command" (`⌘`), and "Shift" (`⇧`). These
modifiers have the following string representations:

Modifier | Linux / Win32 | Mac OSX | curses   |
---------|---------------|---------|----------|
Control  | `'c'`         | `'c'`   | `'c'`    |
Alt      | `'a'`         | `'a'`   | `'m'`    |
Command  | N/A           | `'m'`   | N/A      |
Shift    | `'s'`         | `'s'`   | `'s'`    |

The string representation of key values less than 255 is the character that
Textadept would normally insert if the "Control", "Alt", and "Command"
modifiers were not held down. Therefore, a combination of `Ctrl+Alt+Shift+A`
has the key sequence `caA` on Windows and Linux, but a combination of
`Ctrl+Shift+Tab` has the key sequence `cs\t`. On a United States English
keyboard, since the combination of `Ctrl+Shift+,` has the key sequence `c<`
(`Shift+,` inserts a `<`), Textadept recognizes the key binding as `Ctrl+<`.
This allows key bindings to be language and layout agnostic. For key values
greater than 255, Textadept uses the [`keys.KEYSYMS`](#keys.KEYSYMS) lookup table.
Therefore, `Ctrl+Right Arrow` has the key sequence `cright`. Uncommenting the
`print()` statements in *core/keys.lua* causes Textadept to print key
sequences to standard out (stdout) for inspection.


<a id="keys.Commands"></a>

## Commands

A command bound to a key sequence is simply a Lua function. For example:

    keys['cn'] = buffer.new
    keys['cz'] = buffer.undo
    keys['cu'] = function() io.quick_open(_USERHOME) end

Textadept handles [`buffer`](#buffer) references properly in static contexts.


<a id="keys.Modes"></a>

## Modes

Modes are groups of key bindings such that when a key [mode](#keys.MODE) is
active, Textadept ignores all key bindings defined outside the mode until the
mode is unset. Here is a simple vi mode example:

    keys.command_mode = {
      ['h'] = buffer.char_left,
      ['j'] = buffer.line_up,
      ['k'] = buffer.line_down,
      ['l'] = buffer.char_right,
      ['i'] = function()
        keys.MODE = nil
        ui.statusbar_text = 'INSERT MODE'
      end
    }
    keys['esc'] = function() keys.MODE = 'command_mode' end
    events.connect(events.UPDATE_UI, function()
      if keys.MODE == 'command_mode' then return end
      ui.statusbar_text = 'INSERT MODE'
    end)
    keys.MODE = 'command_mode' -- default mode


<a id="keys.Key.Chains"></a>

## Key Chains

Key chains are a powerful concept. They allow you to assign multiple key
bindings to one key sequence. By default, the `Esc` (`⎋` on Mac OSX | `Esc`
in curses) key cancels a key chain, but you can redefine it via
[`keys.CLEAR`](#keys.CLEAR). An example key chain looks like:

    keys['aa'] = {
      a = function1,
      b = function2,
      c = {...}
    }

## Fields defined by `keys`

<a id="keys.CLEAR"></a>
### `keys.CLEAR` (string)

The key that clears the current key chain.
  It cannot be part of a key chain.
  The default value is `'esc'` for the `Esc` key.

<a id="keys.MODE"></a>
### `keys.MODE` (string)

The current key mode.
  When non-`nil`, all key bindings defined outside of `keys[MODE]` are
  ignored.
  The default value is `nil`.


## Tables defined by `keys`

<a id="keys.KEYSYMS"></a>
### `keys.KEYSYMS`

Lookup table for string representations of key codes higher than 255.
Key codes can be identified by temporarily uncommenting the `print()`
statements in *core/keys.lua*.

<a id="_G.keys"></a>
### `_G.keys`

Map of key bindings to commands, with language-specific key tables assigned
to a lexer name key.

<a id="keys.keychain"></a>
### `keys.keychain`

The current chain of key sequences. (Read-only.)

- - -

<a id="lexer"></a>
# The `lexer` Module

- - -

Lexes Scintilla documents with Lua and LPeg.


<a id="lexer.Overview"></a>

## Overview

Lexers highlight the syntax of source code. Scintilla (the editing component
behind [Textadept][] and [SciTE][]) traditionally uses static, compiled C++
lexers which are notoriously difficult to create and/or extend. On the other
hand, Lua makes it easy to to rapidly create new lexers, extend existing
ones, and embed lexers within one another. Lua lexers tend to be more
readable than C++ lexers too.

Lexers are Parsing Expression Grammars, or PEGs, composed with the Lua
[LPeg library][]. The following table comes from the LPeg documentation and
summarizes all you need to know about constructing basic LPeg patterns. This
module provides convenience functions for creating and working with other
more advanced patterns and concepts.

Operator             | Description
---------------------|------------
`lpeg.P(string)`     | Matches `string` literally.
`lpeg.P(`_`n`_`)`    | Matches exactly _`n`_ characters.
`lpeg.S(string)`     | Matches any character in set `string`.
`lpeg.R("`_`xy`_`")` | Matches any character between range `x` and `y`.
`patt^`_`n`_         | Matches at least _`n`_ repetitions of `patt`.
`patt^-`_`n`_        | Matches at most _`n`_ repetitions of `patt`.
`patt1 * patt2`      | Matches `patt1` followed by `patt2`.
`patt1 + patt2`      | Matches `patt1` or `patt2` (ordered choice).
`patt1 - patt2`      | Matches `patt1` if `patt2` does not match.
`-patt`              | Equivalent to `("" - patt)`.
`#patt`              | Matches `patt` but consumes no input.

The first part of this document deals with rapidly constructing a simple
lexer. The next part deals with more advanced techniques, such as custom
coloring and embedding lexers within one another. Following that is a
discussion about code folding, or being able to tell Scintilla which code
blocks are "foldable" (temporarily hideable from view). After that are
instructions on how to use LPeg lexers with the aforementioned Textadept and
SciTE editors. Finally there are comments on lexer performance and
limitations.

[LPeg library]: http://www.inf.puc-rio.br/~roberto/lpeg/lpeg.html
[Textadept]: http://foicica.com/textadept
[SciTE]: http://scintilla.org/SciTE.html


<a id="lexer.Lexer.Basics"></a>

## Lexer Basics

The *lexers/* directory contains all lexers, including your new one. Before
attempting to write one from scratch though, first determine if your
programming language is similar to any of the 80+ languages supported. If so,
you may be able to copy and modify that lexer, saving some time and effort.
The filename of your lexer should be the name of your programming language in
lower case followed by a *.lua* extension. For example, a new Lua lexer has
the name *lua.lua*.

Note: Try to refrain from using one-character language names like "c", "d",
or "r". For example, Scintillua uses "ansi_c", "dmd", and "rstats",
respectively.


<a id="lexer.New.Lexer.Template"></a>

### New Lexer Template

There is a *lexers/template.txt* file that contains a simple template for a
new lexer. Feel free to use it, replacing the '?'s with the name of your
lexer:

    -- ? LPeg lexer.

    local l = require('lexer')
    local token, word_match = l.token, l.word_match
    local P, R, S = lpeg.P, lpeg.R, lpeg.S

    local M = {_NAME = '?'}

    -- Whitespace.
    local ws = token(l.WHITESPACE, l.space^1)

    M._rules = {
      {'whitespace', ws},
    }

    M._tokenstyles = {

    }

    return M

The first 3 lines of code simply define often used convenience variables. The
5th and last lines define and return the lexer object Scintilla uses; they
are very important and must be part of every lexer. The sixth line defines
something called a "token", an essential building block of lexers. You will
learn about tokens shortly. The rest of the code defines a set of grammar
rules and token styles. You will learn about those later. Note, however, the
`M.` prefix in front of `_rules` and `_tokenstyles`: not only do these tables
belong to their respective lexers, but any non-local variables need the `M.`
prefix too so-as not to affect Lua's global environment. All in all, this is
a minimal, working lexer that you can build on.


<a id="lexer.Tokens"></a>

### Tokens

Take a moment to think about your programming language's structure. What kind
of key elements does it have? In the template shown earlier, one predefined
element all languages have is whitespace. Your language probably also has
elements like comments, strings, and keywords. Lexers refer to these elements
as "tokens". Tokens are the fundamental "building blocks" of lexers. Lexers
break down source code into tokens for coloring, which results in the syntax
highlighting familiar to you. It is up to you how specific your lexer is when
it comes to tokens. Perhaps only distinguishing between keywords and
identifiers is necessary, or maybe recognizing constants and built-in
functions, methods, or libraries is desirable. The Lua lexer, for example,
defines 11 tokens: whitespace, comments, strings, numbers, keywords, built-in
functions, constants, built-in libraries, identifiers, labels, and operators.
Even though constants, built-in functions, and built-in libraries are subsets
of identifiers, Lua programmers find it helpful for the lexer to distinguish
between them all. It is perfectly acceptable to just recognize keywords and
identifiers.

In a lexer, tokens consist of a token name and an LPeg pattern that matches a
sequence of characters recognized as an instance of that token. Create tokens
using the [`lexer.token()`](#lexer.token) function. Let us examine the "whitespace" token
defined in the template shown earlier:

    local ws = token(l.WHITESPACE, l.space^1)

At first glance, the first argument does not appear to be a string name and
the second argument does not appear to be an LPeg pattern. Perhaps you
expected something like:

    local ws = token('whitespace', S('\t\v\f\n\r ')^1)

The `lexer` (`l`) module actually provides a convenient list of common token
names and common LPeg patterns for you to use. Token names include
[`lexer.DEFAULT`](#lexer.DEFAULT), [`lexer.WHITESPACE`](#lexer.WHITESPACE), [`lexer.COMMENT`](#lexer.COMMENT),
[`lexer.STRING`](#lexer.STRING), [`lexer.NUMBER`](#lexer.NUMBER), [`lexer.KEYWORD`](#lexer.KEYWORD),
[`lexer.IDENTIFIER`](#lexer.IDENTIFIER), [`lexer.OPERATOR`](#lexer.OPERATOR), [`lexer.ERROR`](#lexer.ERROR),
[`lexer.PREPROCESSOR`](#lexer.PREPROCESSOR), [`lexer.CONSTANT`](#lexer.CONSTANT), [`lexer.VARIABLE`](#lexer.VARIABLE),
[`lexer.FUNCTION`](#lexer.FUNCTION), [`lexer.CLASS`](#lexer.CLASS), [`lexer.TYPE`](#lexer.TYPE), [`lexer.LABEL`](#lexer.LABEL),
[`lexer.REGEX`](#lexer.REGEX), and [`lexer.EMBEDDED`](#lexer.EMBEDDED). Patterns include
[`lexer.any`](#lexer.any), [`lexer.ascii`](#lexer.ascii), [`lexer.extend`](#lexer.extend), [`lexer.alpha`](#lexer.alpha),
[`lexer.digit`](#lexer.digit), [`lexer.alnum`](#lexer.alnum), [`lexer.lower`](#lexer.lower), [`lexer.upper`](#lexer.upper),
[`lexer.xdigit`](#lexer.xdigit), [`lexer.cntrl`](#lexer.cntrl), [`lexer.graph`](#lexer.graph), [`lexer.print`](#lexer.print),
[`lexer.punct`](#lexer.punct), [`lexer.space`](#lexer.space), [`lexer.newline`](#lexer.newline),
[`lexer.nonnewline`](#lexer.nonnewline), [`lexer.nonnewline_esc`](#lexer.nonnewline_esc), [`lexer.dec_num`](#lexer.dec_num),
[`lexer.hex_num`](#lexer.hex_num), [`lexer.oct_num`](#lexer.oct_num), [`lexer.integer`](#lexer.integer),
[`lexer.float`](#lexer.float), and [`lexer.word`](#lexer.word). You may use your own token names if
none of the above fit your language, but an advantage to using predefined
token names is that your lexer's tokens will inherit the universal syntax
highlighting color theme used by your text editor.


<a id="lexer.Example.Tokens"></a>

#### Example Tokens

So, how might you define other tokens like comments, strings, and keywords?
Here are some examples.

**Comments**

Line-style comments with a prefix character(s) are easy to express with LPeg:

    local shell_comment = token(l.COMMENT, '#' * l.nonnewline^0)
    local c_line_comment = token(l.COMMENT, '//' * l.nonnewline_esc^0)

The comments above start with a '#' or "//" and go to the end of the line.
The second comment recognizes the next line also as a comment if the current
line ends with a '\' escape character.

C-style "block" comments with a start and end delimiter are also easy to
express:

    local c_comment = token(l.COMMENT, '/*' * (l.any - '*/')^0 * P('*/')^-1)

This comment starts with a "/\*" sequence and contains anything up to and
including an ending "\*/" sequence. The ending "\*/" is optional so the lexer
can recognize unfinished comments as comments and highlight them properly.

**Strings**

It is tempting to think that a string is not much different from the block
comment shown above in that both have start and end delimiters:

    local dq_str = '"' * (l.any - '"')^0 * P('"')^-1
    local sq_str = "'" * (l.any - "'")^0 * P("'")^-1
    local simple_string = token(l.STRING, dq_str + sq_str)

However, most programming languages allow escape sequences in strings such
that a sequence like "\\&quot;" in a double-quoted string indicates that the
'&quot;' is not the end of the string. The above token incorrectly matches
such a string. Instead, use the [`lexer.delimited_range()`](#lexer.delimited_range) convenience
function.

    local dq_str = l.delimited_range('"')
    local sq_str = l.delimited_range("'")
    local string = token(l.STRING, dq_str + sq_str)

In this case, the lexer treats '\' as an escape character in a string
sequence.

**Keywords**

Instead of matching _n_ keywords with _n_ `P('keyword_`_`n`_`')` ordered
choices, use another convenience function: [`lexer.word_match()`](#lexer.word_match). It is
much easier and more efficient to write word matches like:

    local keyword = token(l.KEYWORD, l.word_match{
      'keyword_1', 'keyword_2', ..., 'keyword_n'
    })

    local case_insensitive_keyword = token(l.KEYWORD, l.word_match({
      'KEYWORD_1', 'keyword_2', ..., 'KEYword_n'
    }, nil, true))

    local hyphened_keyword = token(l.KEYWORD, l.word_match({
      'keyword-1', 'keyword-2', ..., 'keyword-n'
    }, '-'))

By default, characters considered to be in keywords are in the set of
alphanumeric characters and underscores. The last token demonstrates how to
allow '-' (hyphen) characters to be in keywords as well.

**Numbers**

Most programming languages have the same format for integer and float tokens,
so it might be as simple as using a couple of predefined LPeg patterns:

    local number = token(l.NUMBER, l.float + l.integer)

However, some languages allow postfix characters on integers.

    local integer = P('-')^-1 * (l.dec_num * S('lL')^-1)
    local number = token(l.NUMBER, l.float + l.hex_num + integer)

Your language may need other tweaks, but it is up to you how fine-grained you
want your highlighting to be. After all, you are not writing a compiler or
interpreter!


<a id="lexer.Rules"></a>

### Rules

Programming languages have grammars, which specify valid token structure. For
example, comments usually cannot appear within a string. Grammars consist of
rules, which are simply combinations of tokens. Recall from the lexer
template the `_rules` table, which defines all the rules used by the lexer
grammar:

    M._rules = {
      {'whitespace', ws},
    }

Each entry in a lexer's `_rules` table consists of a rule name and its
associated pattern. Rule names are completely arbitrary and serve only to
identify and distinguish between different rules. Rule order is important: if
text does not match the first rule, the lexer tries the second rule, and so
on. This simple grammar says to match whitespace tokens under a rule named
"whitespace".

To illustrate the importance of rule order, here is an example of a
simplified Lua grammar:

    M._rules = {
      {'whitespace', ws},
      {'keyword', keyword},
      {'identifier', identifier},
      {'string', string},
      {'comment', comment},
      {'number', number},
      {'label', label},
      {'operator', operator},
    }

Note how identifiers come after keywords. In Lua, as with most programming
languages, the characters allowed in keywords and identifiers are in the same
set (alphanumerics plus underscores). If the lexer specified the "identifier"
rule before the "keyword" rule, all keywords would match identifiers and thus
incorrectly highlight as identifiers instead of keywords. The same idea
applies to function, constant, etc. tokens that you may want to distinguish
between: their rules should come before identifiers.

So what about text that does not match any rules? For example in Lua, the '!'
character is meaningless outside a string or comment. Normally the lexer
skips over such text. If instead you want to highlight these "syntax errors",
add an additional end rule:

    M._rules = {
      {'whitespace', ws},
      {'error', token(l.ERROR, l.any)},
    }

This identifies and highlights any character not matched by an existing
rule as an `lexer.ERROR` token.

Even though the rules defined in the examples above contain a single token,
rules may consist of multiple tokens. For example, a rule for an HTML tag
could consist of a tag token followed by an arbitrary number of attribute
tokens, allowing the lexer to highlight all tokens separately. The rule might
look something like this:

    {'tag', tag_start * (ws * attributes)^0 * tag_end^-1}

Note however that lexers with complex rules like these are more prone to lose
track of their state.


<a id="lexer.Summary"></a>

### Summary

Lexers primarily consist of tokens and grammar rules. At your disposal are a
number of convenience patterns and functions for rapidly creating a lexer. If
you choose to use predefined token names for your tokens, you do not have to
define how the lexer highlights them. The tokens will inherit the default
syntax highlighting color theme your editor uses.


<a id="lexer.Advanced.Techniques"></a>

## Advanced Techniques


<a id="lexer.Styles.and.Styling"></a>

### Styles and Styling

The most basic form of syntax highlighting is assigning different colors to
different tokens. Instead of highlighting with just colors, Scintilla allows
for more rich highlighting, or "styling", with different fonts, font sizes,
font attributes, and foreground and background colors, just to name a few.
The unit of this rich highlighting is called a "style". Styles are simply
strings of comma-separated property settings. By default, lexers associate
predefined token names like `lexer.WHITESPACE`, `lexer.COMMENT`,
`lexer.STRING`, etc. with particular styles as part of a universal color
theme. These predefined styles include [`lexer.STYLE_CLASS`](#lexer.STYLE_CLASS),
[`lexer.STYLE_COMMENT`](#lexer.STYLE_COMMENT), [`lexer.STYLE_CONSTANT`](#lexer.STYLE_CONSTANT),
[`lexer.STYLE_ERROR`](#lexer.STYLE_ERROR), [`lexer.STYLE_EMBEDDED`](#lexer.STYLE_EMBEDDED),
[`lexer.STYLE_FUNCTION`](#lexer.STYLE_FUNCTION), [`lexer.STYLE_IDENTIFIER`](#lexer.STYLE_IDENTIFIER),
[`lexer.STYLE_KEYWORD`](#lexer.STYLE_KEYWORD), [`lexer.STYLE_LABEL`](#lexer.STYLE_LABEL), [`lexer.STYLE_NUMBER`](#lexer.STYLE_NUMBER),
[`lexer.STYLE_OPERATOR`](#lexer.STYLE_OPERATOR), [`lexer.STYLE_PREPROCESSOR`](#lexer.STYLE_PREPROCESSOR),
[`lexer.STYLE_REGEX`](#lexer.STYLE_REGEX), [`lexer.STYLE_STRING`](#lexer.STYLE_STRING), [`lexer.STYLE_TYPE`](#lexer.STYLE_TYPE),
[`lexer.STYLE_VARIABLE`](#lexer.STYLE_VARIABLE), and [`lexer.STYLE_WHITESPACE`](#lexer.STYLE_WHITESPACE). Like with
predefined token names and LPeg patterns, you may define your own styles. At
their core, styles are just strings, so you may create new ones and/or modify
existing ones. Each style consists of the following comma-separated settings:

Setting        | Description
---------------|------------
font:_name_    | The name of the font the style uses.
size:_int_     | The size of the font the style uses.
[not]bold      | Whether or not the font face is bold.
weight:_int_   | The weight or boldness of a font, between 1 and 999.
[not]italics   | Whether or not the font face is italic.
[not]underlined| Whether or not the font face is underlined.
fore:_color_   | The foreground color of the font face.
back:_color_   | The background color of the font face.
[not]eolfilled | Does the background color extend to the end of the line?
case:_char_    | The case of the font ('u': upper, 'l': lower, 'm': normal).
[not]visible   | Whether or not the text is visible.
[not]changeable| Whether the text is changeable or read-only.

Specify font colors in either "#RRGGBB" format, "0xBBGGRR" format, or the
decimal equivalent of the latter. As with token names, LPeg patterns, and
styles, there is a set of predefined color names, but they vary depending on
the current color theme in use. Therefore, it is generally not a good idea to
manually define colors within styles in your lexer since they might not fit
into a user's chosen color theme. Try to refrain from even using predefined
colors in a style because that color may be theme-specific. Instead, the best
practice is to either use predefined styles or derive new color-agnostic
styles from predefined ones. For example, Lua "longstring" tokens use the
existing `lexer.STYLE_STRING` style instead of defining a new one.


<a id="lexer.Example.Styles"></a>

#### Example Styles

Defining styles is pretty straightforward. An empty style that inherits the
default theme settings is simply an empty string:

    local style_nothing = ''

A similar style but with a bold font face looks like this:

    local style_bold = 'bold'

If you want the same style, but also with an italic font face, define the new
style in terms of the old one:

    local style_bold_italic = style_bold..',italics'

This allows you to derive new styles from predefined ones without having to
rewrite them. This operation leaves the old style unchanged. Thus if you
had a "static variable" token whose style you wanted to base off of
`lexer.STYLE_VARIABLE`, it would probably look like:

    local style_static_var = l.STYLE_VARIABLE..',italics'

The color theme files in the *lexers/themes/* folder give more examples of
style definitions.


<a id="lexer.Token.Styles"></a>

### Token Styles

Lexers use the `_tokenstyles` table to assign tokens to particular styles.
Recall the token definition and `_tokenstyles` table from the lexer template:

    local ws = token(l.WHITESPACE, l.space^1)

    ...

    M._tokenstyles = {

    }

Why is a style not assigned to the `lexer.WHITESPACE` token? As mentioned
earlier, lexers automatically associate tokens that use predefined token
names with a particular style. Only tokens with custom token names need
manual style associations. As an example, consider a custom whitespace token:

    local ws = token('custom_whitespace', l.space^1)

Assigning a style to this token looks like:

    M._tokenstyles = {
      custom_whitespace = l.STYLE_WHITESPACE
    }

Do not confuse token names with rule names. They are completely different
entities. In the example above, the lexer assigns the "custom_whitespace"
token the existing style for `WHITESPACE` tokens. If instead you want to
color the background of whitespace a shade of grey, it might look like:

    local custom_style = l.STYLE_WHITESPACE..',back:$(color.grey)'
    M._tokenstyles = {
      custom_whitespace = custom_style
    }

Notice that the lexer peforms Scintilla/SciTE-style "$()" property expansion.
You may also use "%()". Remember to refrain from assigning specific colors in
styles, but in this case, all user color themes probably define the
"color.grey" property.


<a id="lexer.Line.Lexers"></a>

### Line Lexers

By default, lexers match the arbitrary chunks of text passed to them by
Scintilla. These chunks may be a full document, only the visible part of a
document, or even just portions of lines. Some lexers need to match whole
lines. For example, a lexer for the output of a file "diff" needs to know if
the line started with a '+' or '-' and then style the entire line
accordingly. To indicate that your lexer matches by line, use the
`_LEXBYLINE` field:

    M._LEXBYLINE = true

Now the input text for the lexer is a single line at a time. Keep in mind
that line lexers do not have the ability to look ahead at subsequent lines.


<a id="lexer.Embedded.Lexers"></a>

### Embedded Lexers

Lexers embed within one another very easily, requiring minimal effort. In the
following sections, the lexer being embedded is called the "child" lexer and
the lexer a child is being embedded in is called the "parent". For example,
consider an HTML lexer and a CSS lexer. Either lexer stands alone for styling
their respective HTML and CSS files. However, CSS can be embedded inside
HTML. In this specific case, the CSS lexer is the "child" lexer with the HTML
lexer being the "parent". Now consider an HTML lexer and a PHP lexer. This
sounds a lot like the case with CSS, but there is a subtle difference: PHP
_embeds itself_ into HTML while CSS is _embedded in_ HTML. This fundamental
difference results in two types of embedded lexers: a parent lexer that
embeds other child lexers in it (like HTML embedding CSS), and a child lexer
that embeds itself within a parent lexer (like PHP embedding itself in HTML).


<a id="lexer.Parent.Lexer"></a>

#### Parent Lexer

Before embedding a child lexer into a parent lexer, the parent lexer needs to
load the child lexer. This is done with the [`lexer.load()`](#lexer.load) function. For
example, loading the CSS lexer within the HTML lexer looks like:

    local css = l.load('css')

The next part of the embedding process is telling the parent lexer when to
switch over to the child lexer and when to switch back. The lexer refers to
these indications as the "start rule" and "end rule", respectively, and are
just LPeg patterns. Continuing with the HTML/CSS example, the transition from
HTML to CSS is when the lexer encounters a "style" tag with a "type"
attribute whose value is "text/css":

    local css_tag = P('<style') * P(function(input, index)
      if input:find('^[^>]+type="text/css"', index) then
        return index
      end
    end)

This pattern looks for the beginning of a "style" tag and searches its
attribute list for the text "`type="text/css"`". (In this simplified example,
the Lua pattern does not consider whitespace between the '=' nor does it
consider that using single quotes is valid.) If there is a match, the
functional pattern returns a value instead of `nil`. In this case, the value
returned does not matter because we ultimately want to style the "style" tag
as an HTML tag, so the actual start rule looks like this:

    local css_start_rule = #css_tag * tag

Now that the parent knows when to switch to the child, it needs to know when
to switch back. In the case of HTML/CSS, the switch back occurs when the
lexer encounters an ending "style" tag, though the lexer should still style
the tag as an HTML tag:

    local css_end_rule = #P('</style>') * tag

Once the parent loads the child lexer and defines the child's start and end
rules, it embeds the child with the [`lexer.embed_lexer()`](#lexer.embed_lexer) function:

    l.embed_lexer(M, css, css_start_rule, css_end_rule)

The first parameter is the parent lexer object to embed the child in, which
in this case is `M`. The other three parameters are the child lexer object
loaded earlier followed by its start and end rules.


<a id="lexer.Child.Lexer"></a>

#### Child Lexer

The process for instructing a child lexer to embed itself into a parent is
very similar to embedding a child into a parent: first, load the parent lexer
into the child lexer with the [`lexer.load()`](#lexer.load) function and then create
start and end rules for the child lexer. However, in this case, swap the
lexer object arguments to [`lexer.embed_lexer()`](#lexer.embed_lexer). For example, in the PHP
lexer:

    local html = l.load('html')
    local php_start_rule = token('php_tag', '<?php ')
    local php_end_rule = token('php_tag', '?>')
    l.embed_lexer(html, M, php_start_rule, php_end_rule)


<a id="lexer.Lexers.with.Complex.State"></a>

### Lexers with Complex State

A vast majority of lexers are not stateful and can operate on any chunk of
text in a document. However, there may be rare cases where a lexer does need
to keep track of some sort of persistent state. Rather than using `lpeg.P`
function patterns that set state variables, it is recommended to make use of
Scintilla's built-in, per-line state integers via [`lexer.line_state`](#lexer.line_state). It
was designed to accommodate up to 32 bit flags for tracking state.
[`lexer.line_from_position()`](#lexer.line_from_position) will return the line for any position given
to an `lpeg.P` function pattern. (Any positions derived from that position
argument will also work.)

Writing stateful lexers is beyond the scope of this document.


<a id="lexer.Code.Folding"></a>

## Code Folding

When reading source code, it is occasionally helpful to temporarily hide
blocks of code like functions, classes, comments, etc. This is the concept of
"folding". In the Textadept and SciTE editors for example, little indicators
in the editor margins appear next to code that can be folded at places called
"fold points". When the user clicks an indicator, the editor hides the code
associated with the indicator until the user clicks the indicator again. The
lexer specifies these fold points and what code exactly to fold.

The fold points for most languages occur on keywords or character sequences.
Examples of fold keywords are "if" and "end" in Lua and examples of fold
character sequences are '{', '}', "/\*", and "\*/" in C for code block and
comment delimiters, respectively. However, these fold points cannot occur
just anywhere. For example, lexers should not recognize fold keywords that
appear within strings or comments. The lexer's `_foldsymbols` table allows
you to conveniently define fold points with such granularity. For example,
consider C:

    M._foldsymbols = {
      [l.OPERATOR] = {['{'] = 1, ['}'] = -1},
      [l.COMMENT] = {['/*'] = 1, ['*/'] = -1},
      _patterns = {'[{}]', '/%*', '%*/'}
    }

The first assignment states that any '{' or '}' that the lexer recognized as
an `lexer.OPERATOR` token is a fold point. The integer `1` indicates the
match is a beginning fold point and `-1` indicates the match is an ending
fold point. Likewise, the second assignment states that any "/\*" or "\*/"
that the lexer recognizes as part of a `lexer.COMMENT` token is a fold point.
The lexer does not consider any occurences of these characters outside their
defined tokens (such as in a string) as fold points. Finally, every
`_foldsymbols` table must have a `_patterns` field that contains a list of
[Lua patterns][] that match fold points. If the lexer encounters text that
matches one of those patterns, the lexer looks up the matched text in its
token's table in order to determine whether or not the text is a fold point.
In the example above, the first Lua pattern matches any '{' or '}'
characters. When the lexer comes across one of those characters, it checks if
the match is an `lexer.OPERATOR` token. If so, the lexer identifies the match
as a fold point. The same idea applies for the other patterns. (The '%' is in
the other patterns because '\*' is a special character in Lua patterns that
needs escaping.) How do you specify fold keywords? Here is an example for
Lua:

    M._foldsymbols = {
      [l.KEYWORD] = {
        ['if'] = 1, ['do'] = 1, ['function'] = 1,
        ['end'] = -1, ['repeat'] = 1, ['until'] = -1
      },
      _patterns = {'%l+'}
    }

Any time the lexer encounters a lower case word, if that word is a
`lexer.KEYWORD` token and in the associated list of fold points, the lexer
identifies the word as a fold point.

If your lexer has case-insensitive keywords as fold points, simply add a
`_case_insensitive = true` option to the `_foldsymbols` table and specify
keywords in lower case.

If your lexer needs to do some additional processing to determine if a match
is a fold point, assign a function that returns an integer. Returning `1` or
`-1` indicates the match is a fold point. Returning `0` indicates it is not.
For example:

    local function fold_strange_token(text, pos, line, s, match)
      if ... then
        return 1 -- beginning fold point
      elseif ... then
        return -1 -- ending fold point
      end
      return 0
    end

    M._foldsymbols = {
      ['strange_token'] = {['|'] = fold_strange_token},
      _patterns = {'|'}
    }

Any time the lexer encounters a '|' that is a "strange_token", it calls the
`fold_strange_token` function to determine if '|' is a fold point. The lexer
calls these functions with the following arguments: the text to identify fold
points in, the beginning position of the current line in the text to fold,
the current line's text, the position in the current line the matched text
starts at, and the matched text itself.

[Lua patterns]: http://www.lua.org/manual/5.2/manual.html#6.4.1


<a id="lexer.Fold.by.Indentation"></a>

### Fold by Indentation

Some languages have significant whitespace and/or no delimiters that indicate
fold points. If your lexer falls into this category and you would like to
mark fold points based on changes in indentation, use the
`_FOLDBYINDENTATION` field:

    M._FOLDBYINDENTATION = true


<a id="lexer.Using.Lexers"></a>

## Using Lexers


<a id="lexer.Textadept"></a>

### Textadept

Put your lexer in your *~/.textadept/lexers/* directory so you do not
overwrite it when upgrading Textadept. Also, lexers in this directory
override default lexers. Thus, Textadept loads a user *lua* lexer instead of
the default *lua* lexer. This is convenient for tweaking a default lexer to
your liking. Then add a [file type][] for your lexer if necessary.

[file type]: _M.textadept.file_types.html


<a id="lexer.SciTE"></a>

### SciTE

Create a *.properties* file for your lexer and `import` it in either your
*SciTEUser.properties* or *SciTEGlobal.properties*. The contents of the
*.properties* file should contain:

    file.patterns.[lexer_name]=[file_patterns]
    lexer.$(file.patterns.[lexer_name])=[lexer_name]

where `[lexer_name]` is the name of your lexer (minus the *.lua* extension)
and `[file_patterns]` is a set of file extensions to use your lexer for.

Please note that Lua lexers ignore any styling information in *.properties*
files. Your theme file in the *lexers/themes/* directory contains styling
information.


<a id="lexer.Considerations"></a>

## Considerations


<a id="lexer.Performance"></a>

### Performance

There might be some slight overhead when initializing a lexer, but loading a
file from disk into Scintilla is usually more expensive. On modern computer
systems, I see no difference in speed between LPeg lexers and Scintilla's C++
ones. Optimize lexers for speed by re-arranging rules in the `_rules` table
so that the most common rules match first. Do keep in mind that order matters
for similar rules.


<a id="lexer.Limitations"></a>

### Limitations

Embedded preprocessor languages like PHP cannot completely embed in their
parent languages in that the parent's tokens do not support start and end
rules. This mostly goes unnoticed, but code like

    <div id="<?php echo $id; ?>">

or

    <div <?php if ($odd) { echo 'class="odd"'; } ?>>

will not style correctly.


<a id="lexer.Troubleshooting"></a>

### Troubleshooting

Errors in lexers can be tricky to debug. Lexers print Lua errors to
`io.stderr` and `_G.print()` statements to `io.stdout`. Running your editor
from a terminal is the easiest way to see errors as they occur.


<a id="lexer.Risks"></a>

### Risks

Poorly written lexers have the ability to crash Scintilla (and thus its
containing application), so unsaved data might be lost. However, I have only
observed these crashes in early lexer development, when syntax errors or
pattern errors are present. Once the lexer actually starts styling text
(either correctly or incorrectly, it does not matter), I have not observed
any crashes.


<a id="lexer.Acknowledgements"></a>

### Acknowledgements

Thanks to Peter Odding for his [lexer post][] on the Lua mailing list
that inspired me, and thanks to Roberto Ierusalimschy for LPeg.

[lexer post]: http://lua-users.org/lists/lua-l/2007-04/msg00116.html

## Fields defined by `lexer`

<a id="lexer.CLASS"></a>
### `lexer.CLASS` (string)

The token name for class tokens.

<a id="lexer.COMMENT"></a>
### `lexer.COMMENT` (string)

The token name for comment tokens.

<a id="lexer.CONSTANT"></a>
### `lexer.CONSTANT` (string)

The token name for constant tokens.

<a id="lexer.DEFAULT"></a>
### `lexer.DEFAULT` (string)

The token name for default tokens.

<a id="lexer.ERROR"></a>
### `lexer.ERROR` (string)

The token name for error tokens.

<a id="lexer.FOLD_BASE"></a>
### `lexer.FOLD_BASE` (number)

The initial (root) fold level.

<a id="lexer.FOLD_BLANK"></a>
### `lexer.FOLD_BLANK` (number)

Flag indicating that the line is blank.

<a id="lexer.FOLD_HEADER"></a>
### `lexer.FOLD_HEADER` (number)

Flag indicating the line is fold point.

<a id="lexer.FUNCTION"></a>
### `lexer.FUNCTION` (string)

The token name for function tokens.

<a id="lexer.IDENTIFIER"></a>
### `lexer.IDENTIFIER` (string)

The token name for identifier tokens.

<a id="lexer.KEYWORD"></a>
### `lexer.KEYWORD` (string)

The token name for keyword tokens.

<a id="lexer.LABEL"></a>
### `lexer.LABEL` (string)

The token name for label tokens.

<a id="lexer.LEXERPATH"></a>
### `lexer.LEXERPATH` (string)

The path used to search for a lexer to load.
  Identical in format to Lua's `package.path` string.
  The default value is `package.path`.

<a id="lexer.NUMBER"></a>
### `lexer.NUMBER` (string)

The token name for number tokens.

<a id="lexer.OPERATOR"></a>
### `lexer.OPERATOR` (string)

The token name for operator tokens.

<a id="lexer.PREPROCESSOR"></a>
### `lexer.PREPROCESSOR` (string)

The token name for preprocessor tokens.

<a id="lexer.REGEX"></a>
### `lexer.REGEX` (string)

The token name for regex tokens.

<a id="lexer.STRING"></a>
### `lexer.STRING` (string)

The token name for string tokens.

<a id="lexer.STYLE_BRACEBAD"></a>
### `lexer.STYLE_BRACEBAD` (string)

The style used for unmatched brace characters.

<a id="lexer.STYLE_BRACELIGHT"></a>
### `lexer.STYLE_BRACELIGHT` (string)

The style used for highlighted brace characters.

<a id="lexer.STYLE_CALLTIP"></a>
### `lexer.STYLE_CALLTIP` (string)

The style used by call tips if [`buffer.call_tip_use_style`](#buffer.call_tip_use_style) is set.
  Only the font name, size, and color attributes are used.

<a id="lexer.STYLE_CLASS"></a>
### `lexer.STYLE_CLASS` (string)

The style typically used for class definitions.

<a id="lexer.STYLE_COMMENT"></a>
### `lexer.STYLE_COMMENT` (string)

The style typically used for code comments.

<a id="lexer.STYLE_CONSTANT"></a>
### `lexer.STYLE_CONSTANT` (string)

The style typically used for constants.

<a id="lexer.STYLE_CONTROLCHAR"></a>
### `lexer.STYLE_CONTROLCHAR` (string)

The style used for control characters.
  Color attributes are ignored.

<a id="lexer.STYLE_DEFAULT"></a>
### `lexer.STYLE_DEFAULT` (string)

The style all styles are based off of.

<a id="lexer.STYLE_EMBEDDED"></a>
### `lexer.STYLE_EMBEDDED` (string)

The style typically used for embedded code.

<a id="lexer.STYLE_ERROR"></a>
### `lexer.STYLE_ERROR` (string)

The style typically used for erroneous syntax.

<a id="lexer.STYLE_FOLDDISPLAYTEXT"></a>
### `lexer.STYLE_FOLDDISPLAYTEXT` (string)

The style used for fold display text.

<a id="lexer.STYLE_FUNCTION"></a>
### `lexer.STYLE_FUNCTION` (string)

The style typically used for function definitions.

<a id="lexer.STYLE_IDENTIFIER"></a>
### `lexer.STYLE_IDENTIFIER` (string)

The style typically used for identifier words.

<a id="lexer.STYLE_INDENTGUIDE"></a>
### `lexer.STYLE_INDENTGUIDE` (string)

The style used for indentation guides.

<a id="lexer.STYLE_KEYWORD"></a>
### `lexer.STYLE_KEYWORD` (string)

The style typically used for language keywords.

<a id="lexer.STYLE_LABEL"></a>
### `lexer.STYLE_LABEL` (string)

The style typically used for labels.

<a id="lexer.STYLE_LINENUMBER"></a>
### `lexer.STYLE_LINENUMBER` (string)

The style used for all margins except fold margins.

<a id="lexer.STYLE_NUMBER"></a>
### `lexer.STYLE_NUMBER` (string)

The style typically used for numbers.

<a id="lexer.STYLE_OPERATOR"></a>
### `lexer.STYLE_OPERATOR` (string)

The style typically used for operators.

<a id="lexer.STYLE_PREPROCESSOR"></a>
### `lexer.STYLE_PREPROCESSOR` (string)

The style typically used for preprocessor statements.

<a id="lexer.STYLE_REGEX"></a>
### `lexer.STYLE_REGEX` (string)

The style typically used for regular expression strings.

<a id="lexer.STYLE_STRING"></a>
### `lexer.STYLE_STRING` (string)

The style typically used for strings.

<a id="lexer.STYLE_TYPE"></a>
### `lexer.STYLE_TYPE` (string)

The style typically used for static types.

<a id="lexer.STYLE_VARIABLE"></a>
### `lexer.STYLE_VARIABLE` (string)

The style typically used for variables.

<a id="lexer.STYLE_WHITESPACE"></a>
### `lexer.STYLE_WHITESPACE` (string)

The style typically used for whitespace.

<a id="lexer.TYPE"></a>
### `lexer.TYPE` (string)

The token name for type tokens.

<a id="lexer.VARIABLE"></a>
### `lexer.VARIABLE` (string)

The token name for variable tokens.

<a id="lexer.WHITESPACE"></a>
### `lexer.WHITESPACE` (string)

The token name for whitespace tokens.

<a id="lexer.alnum"></a>
### `lexer.alnum` (pattern)

A pattern that matches any alphanumeric character ('A'-'Z', 'a'-'z',
    '0'-'9').

<a id="lexer.alpha"></a>
### `lexer.alpha` (pattern)

A pattern that matches any alphabetic character ('A'-'Z', 'a'-'z').

<a id="lexer.any"></a>
### `lexer.any` (pattern)

A pattern that matches any single character.

<a id="lexer.ascii"></a>
### `lexer.ascii` (pattern)

A pattern that matches any ASCII character (codes 0 to 127).

<a id="lexer.cntrl"></a>
### `lexer.cntrl` (pattern)

A pattern that matches any control character (ASCII codes 0 to 31).

<a id="lexer.dec_num"></a>
### `lexer.dec_num` (pattern)

A pattern that matches a decimal number.

<a id="lexer.digit"></a>
### `lexer.digit` (pattern)

A pattern that matches any digit ('0'-'9').

<a id="lexer.extend"></a>
### `lexer.extend` (pattern)

A pattern that matches any ASCII extended character (codes 0 to 255).

<a id="lexer.float"></a>
### `lexer.float` (pattern)

A pattern that matches a floating point number.

<a id="lexer.fold_level"></a>
### `lexer.fold_level` (table, Read-only)

Table of fold level bit-masks for line numbers starting from zero.
  Fold level masks are composed of an integer level combined with any of the
  following bits:

  * `lexer.FOLD_BASE`
    The initial fold level.
  * `lexer.FOLD_BLANK`
    The line is blank.
  * `lexer.FOLD_HEADER`
    The line is a header, or fold point.

<a id="lexer.graph"></a>
### `lexer.graph` (pattern)

A pattern that matches any graphical character ('!' to '~').

<a id="lexer.hex_num"></a>
### `lexer.hex_num` (pattern)

A pattern that matches a hexadecimal number.

<a id="lexer.indent_amount"></a>
### `lexer.indent_amount` (table, Read-only)

Table of indentation amounts in character columns, for line numbers
  starting from zero.

<a id="lexer.integer"></a>
### `lexer.integer` (pattern)

A pattern that matches either a decimal, hexadecimal, or octal number.

<a id="lexer.line_state"></a>
### `lexer.line_state` (table)

Table of integer line states for line numbers starting from zero.
  Line states can be used by lexers for keeping track of persistent states.

<a id="lexer.lower"></a>
### `lexer.lower` (pattern)

A pattern that matches any lower case character ('a'-'z').

<a id="lexer.newline"></a>
### `lexer.newline` (pattern)

A pattern that matches any set of end of line characters.

<a id="lexer.nonnewline"></a>
### `lexer.nonnewline` (pattern)

A pattern that matches any single, non-newline character.

<a id="lexer.nonnewline_esc"></a>
### `lexer.nonnewline_esc` (pattern)

A pattern that matches any single, non-newline character or any set of end
  of line characters escaped with '\'.

<a id="lexer.oct_num"></a>
### `lexer.oct_num` (pattern)

A pattern that matches an octal number.

<a id="lexer.print"></a>
### `lexer.print` (pattern)

A pattern that matches any printable character (' ' to '~').

<a id="lexer.property"></a>
### `lexer.property` (table)

Map of key-value string pairs.

<a id="lexer.property_expanded"></a>
### `lexer.property_expanded` (table, Read-only)

Map of key-value string pairs with `$()` and `%()` variable replacement
  performed in values.

<a id="lexer.property_int"></a>
### `lexer.property_int` (table, Read-only)

Map of key-value pairs with values interpreted as numbers, or `0` if not
  found.

<a id="lexer.punct"></a>
### `lexer.punct` (pattern)

A pattern that matches any punctuation character ('!' to '/', ':' to '@',
  '[' to ''', '{' to '~').

<a id="lexer.space"></a>
### `lexer.space` (pattern)

A pattern that matches any whitespace character ('\t', '\v', '\f', '\n',
  '\r', space).

<a id="lexer.style_at"></a>
### `lexer.style_at` (table, Read-only)

Table of style names at positions in the buffer starting from 1.

<a id="lexer.upper"></a>
### `lexer.upper` (pattern)

A pattern that matches any upper case character ('A'-'Z').

<a id="lexer.word"></a>
### `lexer.word` (pattern)

A pattern that matches a typical word. Words begin with a letter or
  underscore and consist of alphanumeric and underscore characters.

<a id="lexer.xdigit"></a>
### `lexer.xdigit` (pattern)

A pattern that matches any hexadecimal digit ('0'-'9', 'A'-'F', 'a'-'f').


## Functions defined by `lexer`

<a id="lexer.delimited_range"></a>
### `lexer.delimited_range`(*chars, single\_line, no\_escape, balanced*)

Creates and returns a pattern that matches a range of text bounded by
*chars* characters.
This is a convenience function for matching more complicated delimited ranges
like strings with escape characters and balanced parentheses. *single_line*
indicates whether or not the range must be on a single line, *no_escape*
indicates whether or not to ignore '\' as an escape character, and *balanced*
indicates whether or not to handle balanced ranges like parentheses and
requires *chars* to be composed of two characters.

Parameters:

* *`chars`*: The character(s) that bound the matched range.
* *`single_line`*: Optional flag indicating whether or not the range must be
  on a single line.
* *`no_escape`*: Optional flag indicating whether or not the range end
  character may be escaped by a '\\' character.
* *`balanced`*: Optional flag indicating whether or not to match a balanced
  range, like the "%b" Lua pattern. This flag only applies if *chars*
  consists of two different characters (e.g. "()").

Usage:

* `local dq_str_escapes = l.delimited_range('"')`
* `local dq_str_noescapes = l.delimited_range('"', false, true)`
* `local unbalanced_parens = l.delimited_range('()')`
* `local balanced_parens = l.delimited_range('()', false, false, true)`

Return:

* pattern

See also:

* [`lexer.nested_pair`](#lexer.nested_pair)

<a id="lexer.embed_lexer"></a>
### `lexer.embed_lexer`(*parent, child, start\_rule, end\_rule*)

Embeds child lexer *child* in parent lexer *parent* using patterns
*start_rule* and *end_rule*, which signal the beginning and end of the
embedded lexer, respectively.

Parameters:

* *`parent`*: The parent lexer.
* *`child`*: The child lexer.
* *`start_rule`*: The pattern that signals the beginning of the embedded
  lexer.
* *`end_rule`*: The pattern that signals the end of the embedded lexer.

Usage:

* `l.embed_lexer(M, css, css_start_rule, css_end_rule)`
* `l.embed_lexer(html, M, php_start_rule, php_end_rule)`
* `l.embed_lexer(html, ruby, ruby_start_rule, ruby_end_rule)`

<a id="lexer.fold"></a>
### `lexer.fold`(*lexer, text, start\_pos, start\_line, start\_level*)

Determines fold points in a chunk of text *text* with lexer *lexer*.
*text* starts at position *start_pos* on line number *start_line* with a
beginning fold level of *start_level* in the buffer. If *lexer* has a `_fold`
function or a `_foldsymbols` table, that field is used to perform folding.
Otherwise, if *lexer* has a `_FOLDBYINDENTATION` field set, or if a
`fold.by.indentation` property is set, folding by indentation is done.

Parameters:

* *`lexer`*: The lexer object to fold with.
* *`text`*: The text in the buffer to fold.
* *`start_pos`*: The position in the buffer *text* starts at, starting at
  zero.
* *`start_line`*: The line number *text* starts on.
* *`start_level`*: The fold level *text* starts on.

Return:

* table of fold levels.

<a id="lexer.fold_line_comments"></a>
### `lexer.fold_line_comments`(*prefix*)

Returns a fold function (to be used within the lexer's `_foldsymbols` table)
that folds consecutive line comments that start with string *prefix*.

Parameters:

* *`prefix`*: The prefix string defining a line comment.

Usage:

* `[l.COMMENT] = {['--'] = l.fold_line_comments('--')}`
* `[l.COMMENT] = {['//'] = l.fold_line_comments('//')}`

<a id="lexer.last_char_includes"></a>
### `lexer.last_char_includes`(*s*)

Creates and returns a pattern that verifies that string set *s* contains the
first non-whitespace character behind the current match position.

Parameters:

* *`s`*: String character set like one passed to `lpeg.S()`.

Usage:

* `local regex = l.last_char_includes('+-*!%^&|=,([{') *
  l.delimited_range('/')`

Return:

* pattern

<a id="lexer.lex"></a>
### `lexer.lex`(*lexer, text, init\_style*)

Lexes a chunk of text *text* (that has an initial style number of
*init_style*) with lexer *lexer*.
If *lexer* has a `_LEXBYLINE` flag set, the text is lexed one line at a time.
Otherwise the text is lexed as a whole.

Parameters:

* *`lexer`*: The lexer object to lex with.
* *`text`*: The text in the buffer to lex.
* *`init_style`*: The current style. Multiple-language lexers use this to
  determine which language to start lexing in.

Return:

* table of token names and positions.

<a id="lexer.line_from_position"></a>
### `lexer.line_from_position`(*pos*)

Returns the line number of the line that contains position *pos*, which
starts from 1.

Parameters:

* *`pos`*: The position to get the line number of.

Return:

* number

<a id="lexer.load"></a>
### `lexer.load`(*name, alt\_name, cache*)

Initializes or loads and returns the lexer of string name *name*.
Scintilla calls this function in order to load a lexer. Parent lexers also
call this function in order to load child lexers and vice-versa. The user
calls this function in order to load a lexer when using Scintillua as a Lua
library.

Parameters:

* *`name`*: The name of the lexing language.
* *`alt_name`*: The alternate name of the lexing language. This is useful for
  embedding the same child lexer with multiple sets of start and end tokens.
* *`cache`*: Flag indicating whether or not to load lexers from the cache.
  This should only be `true` when initially loading a lexer (e.g. not from
  within another lexer for embedding purposes).
  The default value is `false`.

Return:

* lexer object

<a id="lexer.nested_pair"></a>
### `lexer.nested_pair`(*start\_chars, end\_chars*)

Returns a pattern that matches a balanced range of text that starts with
string *start_chars* and ends with string *end_chars*.
With single-character delimiters, this function is identical to
`delimited_range(start_chars..end_chars, false, true, true)`.

Parameters:

* *`start_chars`*: The string starting a nested sequence.
* *`end_chars`*: The string ending a nested sequence.

Usage:

* `local nested_comment = l.nested_pair('/*', '*/')`

Return:

* pattern

See also:

* [`lexer.delimited_range`](#lexer.delimited_range)

<a id="lexer.starts_line"></a>
### `lexer.starts_line`(*patt*)

Creates and returns a pattern that matches pattern *patt* only at the
beginning of a line.

Parameters:

* *`patt`*: The LPeg pattern to match on the beginning of a line.

Usage:

* `local preproc = token(l.PREPROCESSOR, l.starts_line('#') *
  l.nonnewline^0)`

Return:

* pattern

<a id="lexer.token"></a>
### `lexer.token`(*name, patt*)

Creates and returns a token pattern with token name *name* and pattern
*patt*.
If *name* is not a predefined token name, its style must be defined in the
lexer's `_tokenstyles` table.

Parameters:

* *`name`*: The name of token. If this name is not a predefined token name,
  then a style needs to be assiciated with it in the lexer's `_tokenstyles`
  table.
* *`patt`*: The LPeg pattern associated with the token.

Usage:

* `local ws = token(l.WHITESPACE, l.space^1)`
* `local annotation = token('annotation', '@' * l.word)`

Return:

* pattern

<a id="lexer.word_match"></a>
### `lexer.word_match`(*words, word\_chars, case\_insensitive*)

Creates and returns a pattern that matches any single word in list *words*.
Words consist of alphanumeric and underscore characters, as well as the
characters in string set *word_chars*. *case_insensitive* indicates whether
or not to ignore case when matching words.
This is a convenience function for simplifying a set of ordered choice word
patterns.

Parameters:

* *`words`*: A table of words.
* *`word_chars`*: Optional string of additional characters considered to be
  part of a word. By default, word characters are alphanumerics and
  underscores ("%w_" in Lua). This parameter may be `nil` or the empty string
  in order to indicate no additional word characters.
* *`case_insensitive`*: Optional boolean flag indicating whether or not the
  word match is case-insensitive. The default is `false`.

Usage:

* `local keyword = token(l.KEYWORD, word_match{'foo', 'bar', 'baz'})`
* `local keyword = token(l.KEYWORD, word_match({'foo-bar', 'foo-baz',
  'bar-foo', 'bar-baz', 'baz-foo', 'baz-bar'}, '-', true))`

Return:

* pattern


## Tables defined by `lexer`

<a id="lexer.lexer"></a>
### `lexer.lexer`

Individual fields for a lexer instance.

Fields:

* `_NAME`: The string name of the lexer.
* `_rules`: An ordered list of rules for a lexer grammar.
  Each rule is a table containing an arbitrary rule name and the LPeg pattern
  associated with the rule. The order of rules is important, as rules are
  matched sequentially.
  Child lexers should not use this table to access and/or modify their
  parent's rules and vice-versa. Use the `_RULES` table instead.
* `_tokenstyles`: A map of non-predefined token names to styles.
  Remember to use token names, not rule names. It is recommended to use
  predefined styles or color-agnostic styles derived from predefined styles
  to ensure compatibility with user color themes.
* `_foldsymbols`: A table of recognized fold points for the lexer.
  Keys are token names with table values defining fold points. Those table
  values have string keys of keywords or characters that indicate a fold
  point whose values are integers. A value of `1` indicates a beginning fold
  point and a value of `-1` indicates an ending fold point. Values can also
  be functions that return `1`, `-1`, or `0` (indicating no fold point) for
  keys which need additional processing.
  There is also a required `_patterns` key whose value is a table containing
  Lua pattern strings that match all fold points (the string keys contained
  in token name table values). When the lexer encounters text that matches
  one of those patterns, the matched text is looked up in its token's table
  to determine whether or not it is a fold point.
  There is also an optional `_case_insensitive` option that indicates whether
  or not fold point keys are case-insensitive. If `true`, fold point keys
  should be in lower case.
* `_fold`: If this function exists in the lexer, it is called for folding
  the document instead of using `_foldsymbols` or indentation.
* `_lexer`: The parent lexer object whose rules should be used. This field
  is only necessary to disambiguate a proxy lexer that loaded parent and
  child lexers for embedding and ended up having multiple parents loaded.
* `_RULES`: A map of rule name keys with their associated LPeg pattern
  values for the lexer.
  This is constructed from the lexer's `_rules` table and accessible to other
  lexers for embedded lexer applications like modifying parent or child
  rules.
* `_LEXBYLINE`: Indicates the lexer can only process one whole line of text
   (instead of an arbitrary chunk of text) at a time.
   The default value is `false`. Line lexers cannot look ahead to subsequent
   lines.
* `_FOLDBYINDENTATION`: Declares the lexer does not define fold points and
   that fold points should be calculated based on changes in indentation.

- - -

<a id="lfs"></a>
# The `lfs` Module

- - -

Extends the `lfs` library to find files in directories and determine absolute
file paths.

## Functions defined by `lfs`

<a id="lfs.abspath"></a>
### `lfs.abspath`(*filename, prefix*)

Returns the absolute path to string *filename*.
*prefix* or `lfs.currentdir()` is prepended to a relative filename. The
returned path is not guaranteed to exist.

Parameters:

* *`filename`*: The relative or absolute path to a file.
* *`prefix`*: Optional prefix path prepended to a relative filename.

Return:

* string absolute path

<a id="lfs.dir_foreach"></a>
### `lfs.dir_foreach`(*dir, f, filter, n, include\_dirs, level*)

Iterates over all files and sub-directories (up to *n* levels deep) in
directory *dir*, calling function *f* with each file found.
Files passed to *f* do not match any pattern in string or table *filter*
(or `lfs.default_filter` when *filter* is `nil`). A filter table contains:

  + Lua patterns that match filenames to exclude.
  + Optional `folders` sub-table that contains patterns matching directories
    to exclude.
  + Optional `extensions` sub-table that contains raw file extensions to
    exclude.
  + Optional `symlink` flag that when `true`, excludes symlinked files (but
    not symlinked directories).
  + Optional `folders.symlink` flag that when `true`, excludes symlinked
    directories.

Any filter patterns starting with '!' include files and directories that
match the pattern that follows. This is useful for filtering out all files
and directories except a select few.

Parameters:

* *`dir`*: The directory path to iterate over.
* *`f`*: Function to call with each full file path found. If *f* returns
  `false` explicitly, iteration ceases.
* *`filter`*: Optional filter for files and directories to exclude. The
  default value is `lfs.default_filter`.
* *`n`*: Optional maximum number of directory levels to descend into.
  The default value is `nil`, which indicates no limit.
* *`include_dirs`*: Optional flag indicating whether or not to call *f* with
  directory names too. Directory names are passed with a trailing '/' or '\',
  depending on the current platform.
  The default value is `false`.
* *`level`*: Utility value indicating the directory level this function is
  at. This value is used and set internally, and should not be set otherwise.

See also:

* [`lfs.filter`](#lfs.filter)


## Tables defined by `lfs`

<a id="lfs.default_filter"></a>
### `lfs.default_filter`

The filter table containing common binary file extensions and version control
directories to exclude when iterating over files and directories using
`dir_foreach`.

See also:

* [`lfs.dir_foreach`](#lfs.dir_foreach)

- - -

<a id="string"></a>
# The `string` Module

- - -

Extends Lua's `string` library to provide character set conversions.

## Functions defined by `string`

<a id="string.iconv"></a>
### `string.iconv`(*text, new, old*)

Converts string *text* from encoding *old* to encoding *new* using iconv,
returning the string result.
Valid encodings are [GNU iconv's encodings][] and include:

  * European: ASCII, ISO-8859-{1,2,3,4,5,7,9,10,13,14,15,16}, KOI8-R, KOI8-U,
    KOI8-RU, CP{1250,1251,1252,1253,1254,1257}, CP{850,866,1131},
    Mac{Roman,CentralEurope,Iceland,Croatian,Romania},
    Mac{Cyrillic,Ukraine,Greek,Turkish}, Macintosh.
  * Semitic: ISO-8859-{6,8}, CP{1255,1256}, CP862, Mac{Hebrew,Arabic}.
  * Japanese: EUC-JP, SHIFT_JIS, CP932, ISO-2022-JP, ISO-2022-JP-2,
    ISO-2022-JP-1.
  * Chinese: EUC-CN, HZ, GBK, CP936, GB18030, EUC-TW, BIG5, CP950,
    BIG5-HKSCS, BIG5-HKSCS:2004, BIG5-HKSCS:2001, BIG5-HKSCS:1999,
    ISO-2022-CN, ISO-2022-CN-EXT.
  * Korean: EUC-KR, CP949, ISO-2022-KR, JOHAB.
  * Armenian: ARMSCII-8.
  * Georgian: Georgian-Academy, Georgian-PS.
  * Tajik: KOI8-T.
  * Kazakh: PT154, RK1048.
  * Thai: ISO-8859-11, TIS-620, CP874, MacThai.
  * Laotian: MuleLao-1, CP1133.
  * Vietnamese: VISCII, TCVN, CP1258.
  * Unicode: UTF-8, UCS-2, UCS-2BE, UCS-2LE, UCS-4, UCS-4BE, UCS-4LE, UTF-16,
    UTF-16BE, UTF-16LE, UTF-32, UTF-32BE, UTF-32LE, UTF-7, C99, JAVA.

[GNU iconv's encodings]: http://www.gnu.org/software/libiconv/

Parameters:

* *`text`*: The text to convert.
* *`new`*: The string encoding to convert to.
* *`old`*: The string encoding to convert from.


- - -

<a id="textadept"></a>
# The `textadept` Module

- - -

The textadept module.
It provides utilities for editing text in Textadept.

- - -

<a id="textadept.bookmarks"></a>
# The `textadept.bookmarks` Module

- - -

Bookmarks for Textadept.

## Fields defined by `textadept.bookmarks`

<a id="textadept.bookmarks.MARK_BOOKMARK"></a>
### `textadept.bookmarks.MARK_BOOKMARK` (number)

The bookmark mark number.


## Functions defined by `textadept.bookmarks`

<a id="textadept.bookmarks.clear"></a>
### `textadept.bookmarks.clear`()

Clears all bookmarks in the current buffer.

<a id="textadept.bookmarks.goto_mark"></a>
### `textadept.bookmarks.goto_mark`(*next*)

Prompts the user to select a bookmarked line to move the caret to the
beginning of unless *next* is given.
If *next* is `true` or `false`, moves the caret to the beginning of the next
or previously bookmarked line, respectively.

Parameters:

* *`next`*: Optional flag indicating whether to go to the next or previous
  bookmarked line relative to the current line. The default value is `nil`,
  prompting the user for a bookmarked line to go to.

<a id="textadept.bookmarks.toggle"></a>
### `textadept.bookmarks.toggle`(*on, line*)

Toggles the bookmark on line number *line* or the current line, unless *on*
is given.
If *on* is `true` or `false`, adds or removes the bookmark, respectively.

Parameters:

* *`on`*: Optional flag indicating whether to add or remove a bookmark on
  line *line* or the current line. The default value is `nil`, toggling a
  bookmark.
* *`line`*: Optional line number to add or remove a bookmark on.


- - -

<a id="textadept.editing"></a>
# The `textadept.editing` Module

- - -

Editing features for Textadept.

## Fields defined by `textadept.editing`

<a id="textadept.editing.INDIC_BRACEMATCH"></a>
### `textadept.editing.INDIC_BRACEMATCH` (number)

The matching brace highlight indicator number.

<a id="textadept.editing.INDIC_HIGHLIGHT"></a>
### `textadept.editing.INDIC_HIGHLIGHT` (number)

The word highlight indicator number.

<a id="textadept.editing.auto_indent"></a>
### `textadept.editing.auto_indent` (bool)

Match the previous line's indentation level after inserting a new line.
  The default value is `true`.

<a id="textadept.editing.autocomplete_all_words"></a>
### `textadept.editing.autocomplete_all_words` (bool)

Autocomplete the current word using words from all open buffers.
  If `true`, performance may be slow when many buffers are open.
  The default value is `false`.

<a id="textadept.editing.strip_trailing_spaces"></a>
### `textadept.editing.strip_trailing_spaces` (bool)

Strip trailing whitespace before saving files.
  The default value is `false`.


## Functions defined by `textadept.editing`

<a id="textadept.editing.autocomplete"></a>
### `textadept.editing.autocomplete`(*name*)

Displays an autocompletion list provided by the autocompleter function
associated with string *name*, and returns `true` if completions were found.

Parameters:

* *`name`*: The name of an autocompleter function in the `autocompleters`
  table to use for providing autocompletions.

See also:

* [`textadept.editing.autocompleters`](#textadept.editing.autocompleters)

<a id="textadept.editing.block_comment"></a>
### `textadept.editing.block_comment`()

Comments or uncomments the selected lines based on the current language.
As long as any part of a line is selected, the entire line is eligible for
commenting/uncommenting.

See also:

* [`textadept.editing.comment_string`](#textadept.editing.comment_string)

<a id="textadept.editing.convert_indentation"></a>
### `textadept.editing.convert_indentation`()

Converts indentation between tabs and spaces according to `buffer.use_tabs`.
If `buffer.use_tabs` is `true`, `buffer.tab_width` indenting spaces are
converted to tabs. Otherwise, all indenting tabs are converted to
`buffer.tab_width` spaces.

See also:

* [`buffer.use_tabs`](#buffer.use_tabs)

<a id="textadept.editing.enclose"></a>
### `textadept.editing.enclose`(*left, right*)

Encloses the selected text or the current word within strings *left* and
*right*, taking multiple selections into account.

Parameters:

* *`left`*: The left part of the enclosure.
* *`right`*: The right part of the enclosure.

<a id="textadept.editing.filter_through"></a>
### `textadept.editing.filter_through`(*command*)

Passes the selected text or all buffer text to string shell command *command*
as standard input (stdin) and replaces the input text with the command's
standard output (stdout).
Standard input is as follows:

1. If text is selected and spans multiple lines, all text on the lines that
have text selected is passed as stdin. However, if the end of the selection
is at the beginning of a line, only the line ending delimiters from the
previous line are included. The rest of the line is excluded.
2. If text is selected and spans a single line, only the selected text is
used.
3. If no text is selected, the entire buffer is used.

Parameters:

* *`command`*: The Linux, BSD, Mac OSX, or Windows shell command to filter
  text through.

<a id="textadept.editing.goto_line"></a>
### `textadept.editing.goto_line`(*line*)

Moves the caret to the beginning of line number *line* or the user-specified
line, ensuring *line* is visible.

Parameters:

* *`line`*: Optional line number to go to. If `nil`, the user is prompted for
  one.

<a id="textadept.editing.highlight_word"></a>
### `textadept.editing.highlight_word`()

Highlights all occurrences of the selected text or all occurrences of the
current word.

See also:

* [`buffer.word_chars`](#buffer.word_chars)

<a id="textadept.editing.join_lines"></a>
### `textadept.editing.join_lines`()

Joins the currently selected lines or the current line with the line below
it.
As long as any part of a line is selected, the entire line is eligible for
joining.

<a id="textadept.editing.match_brace"></a>
### `textadept.editing.match_brace`(*select*)

Goes to the current character's matching brace, selecting the text in between
if *select* is `true`.

Parameters:

* *`select`*: Optional flag indicating whether or not to select the text
  between matching braces. The default value is `false`.

<a id="textadept.editing.select_enclosed"></a>
### `textadept.editing.select_enclosed`(*left, right*)

Selects the text between strings *left* and *right* that enclose the caret.
If that range is already selected, toggles between selecting *left* and
*right* as well.

Parameters:

* *`left`*: The left part of the enclosure.
* *`right`*: The right part of the enclosure.

<a id="textadept.editing.select_line"></a>
### `textadept.editing.select_line`()

Selects the current line.

<a id="textadept.editing.select_paragraph"></a>
### `textadept.editing.select_paragraph`()

Selects the current paragraph.
Paragraphs are surrounded by one or more blank lines.

<a id="textadept.editing.select_word"></a>
### `textadept.editing.select_word`(*all*)

Selects the current word or, if *all* is `true`, all occurrences of the
current word.
If a word is already selected, selects the next occurrence as a multiple
selection.

Parameters:

* *`all`*: Whether or not to select all occurrences of the current word.
  The default value is `false`.

See also:

* [`buffer.word_chars`](#buffer.word_chars)

<a id="textadept.editing.show_documentation"></a>
### `textadept.editing.show_documentation`()

Displays a call tip with documentation for the symbol under or directly
behind the caret.
Documentation is read from API files in the `api_files` table.
If a call tip is already shown, cycles to the next one if it exists.
Symbols are determined by using `buffer.word_chars`.

See also:

* [`textadept.editing.api_files`](#textadept.editing.api_files)
* [`buffer.word_chars`](#buffer.word_chars)

<a id="textadept.editing.transpose_chars"></a>
### `textadept.editing.transpose_chars`()

Transposes characters intelligently.
If the caret is at the end of a line, transposes the two characters before
the caret. Otherwise, the characters to the left and right are.


## Tables defined by `textadept.editing`

<a id="textadept.editing.XPM_IMAGES"></a>
### `textadept.editing.XPM_IMAGES`

Map of image names to registered image numbers.

Fields:

* `CLASS`: The image number for classes.
* `NAMESPACE`: The image number for namespaces.
* `METHOD`: The image number for methods.
* `SIGNAL`: The image number for signals.
* `SLOT`: The image number for slots.
* `VARIABLE`: The image number for variables.
* `STRUCT`: The image number for structures.
* `TYPEDEF`: The image number for type definitions.

<a id="textadept.editing.api_files"></a>
### `textadept.editing.api_files`

Map of lexer names to API documentation file tables.
Each line in an API file consists of a symbol name (not a fully qualified
symbol name), a space character, and that symbol's documentation. "\n"
represents a newline character.

See also:

* [`textadept.editing.show_documentation`](#textadept.editing.show_documentation)

<a id="textadept.editing.auto_pairs"></a>
### `textadept.editing.auto_pairs`

Map of auto-paired characters like parentheses, brackets, braces, and quotes.
The ASCII values of opening characters are assigned to strings that contain
complement characters. The default auto-paired characters are "()", "[]",
"{}", "&apos;&apos;", and "&quot;&quot;".

Usage:

* `textadept.editing.auto_pairs[60] = '>' -- pair '<' and '>'`
* `textadept.editing.auto_pairs = nil -- disable completely`

<a id="textadept.editing.autocompleters"></a>
### `textadept.editing.autocompleters`

Map of autocompleter names to autocompletion functions.
Names are typically lexer names and autocompletion functions typically
autocomplete symbols.
Autocompletion functions must return two values: the number of characters
behind the caret that are used as the prefix of the entity to be
autocompleted, and a list of completions to be shown. Autocompletion lists
are sorted automatically.

See also:

* [`textadept.editing.autocomplete`](#textadept.editing.autocomplete)

<a id="textadept.editing.brace_matches"></a>
### `textadept.editing.brace_matches`

Table of brace characters to highlight.
The ASCII values of brace characters are keys and are assigned non-`nil`
values. The default brace characters are '(', ')', '[', ']', '{', and '}'.

Usage:

* `textadept.editing.brace_matches[60] = true -- '<'`
* `textadept.editing.brace_matches[62] = true -- '>'`

<a id="textadept.editing.comment_string"></a>
### `textadept.editing.comment_string`

Map of lexer names to line comment strings for programming languages, used by
the `block_comment()` function.
Keys are lexer names and values are either the language's line comment
prefixes or block comment delimiters separated by a '|' character.

See also:

* [`textadept.editing.block_comment`](#textadept.editing.block_comment)

<a id="textadept.editing.typeover_chars"></a>
### `textadept.editing.typeover_chars`

Table of characters to move over when typed.
The ASCII values of characters are keys and are assigned non-`nil` values.
The default characters are ')', ']', '}', '&apos;', and '&quot;'.

Usage:

* `textadept.editing.typeover_chars[62] = true -- '>'`

- - -

<a id="textadept.file_types"></a>
# The `textadept.file_types` Module

- - -

Handles file type detection for Textadept.

## Fields defined by `textadept.file_types`

<a id="events.LEXER_LOADED"></a>
### `events.LEXER_LOADED` (string)

Emitted after loading a language lexer.
  This is useful for overriding a language module's key bindings or other
  properties since the module is not loaded when Textadept starts.
  Arguments:

  * *`lexer`*: The language lexer's name.


## Functions defined by `textadept.file_types`

<a id="textadept.file_types.select_lexer"></a>
### `textadept.file_types.select_lexer`()

Prompts the user to select a lexer for the current buffer.

See also:

* [`buffer.set_lexer`](#buffer.set_lexer)


## Tables defined by `textadept.file_types`

<a id="textadept.file_types.extensions"></a>
### `textadept.file_types.extensions`

Map of file extensions to their associated lexer names.
If the file type is not recognized by its first-line, each file extension is
matched against the file's extension.

<a id="textadept.file_types.lexers"></a>
### `textadept.file_types.lexers`

List of available lexer names.

<a id="textadept.file_types.patterns"></a>
### `textadept.file_types.patterns`

Map of first-line patterns to their associated lexer names.
Each pattern is matched against the first line in the file.

- - -

<a id="textadept.keys"></a>
# The `textadept.keys` Module

- - -

Defines key commands for Textadept.
This set of key commands is pretty standard among other text editors, at
least for basic editing commands and movements.


<a id="textadept.keys.Key.Bindings"></a>

## Key Bindings

Linux / Win32 | Mac OSX | Terminal | Command
--------------|---------|----------|--------
**File**      |         |          |
Ctrl+N        |⌘N       |M-^N      |New file
Ctrl+O        |⌘O       |^O        |Open file
Ctrl+Alt+O    |^⌘O      |M-^O      |Open recent file...
Ctrl+Shift+O  |⌘⇧O      |M-O       |Reload file
Ctrl+S        |⌘S       |^S        |Save file
Ctrl+Shift+S  |⌘⇧S      |M-^S      |Save file as..
Ctrl+W        |⌘W       |^W        |Close file
Ctrl+Shift+W  |⌘⇧W      |M-^W      |Close all files
None          |None     |None      |Load session...
None          |None     |None      |Load session...
Ctrl+Q        |⌘Q       |^Q        |Quit
**Edit**                |         |              |
Ctrl+Z<br/>Alt+Bksp     |⌘Z       |^Z^(†)<br/>M-Z|Undo
Ctrl+Y<br/>Ctrl+Shift+Z |⌘⇧Z      |^Y<br/>M-S-Z  |Redo
Ctrl+X<br/>Shift+Del    |⌘X<br/>⇧⌦|^X            |Cut
Ctrl+C<br/>Ctrl+Ins     |⌘C       |^C            |Copy
Ctrl+V<br/>Shift+Ins    |⌘V       |^V            |Paste
Ctrl+D                  |⌘D       |None          |Duplicate line
Del                     |⌦<br/>^D |Del<br/>^D    |Delete
Alt+Del                 |^⌦       |M-Del<br/>M-D |Delete word
Ctrl+A                  |⌘A       |M-A           |Select all
Ctrl+M                  |^M       |M-M           |Match brace
Ctrl+Enter              |^Esc     |M-Enter^(‡)   |Complete word
Ctrl+Alt+Shift+H        |⌘⇧H      |None          |Highlight word
Ctrl+/                  |^/       |M-/           |Toggle block comment
Ctrl+T                  |^T       |^T            |Transpose characters
Ctrl+Shift+J            |^J       |M-J           |Join lines
Ctrl+&#124;             |⌘&#124;  |^\            |Filter text through
Ctrl+Shift+M            |^⇧M      |M-S-M         |Select to matching brace
Ctrl+<                  |⌘<       |M-<           |Select between XML tags
Ctrl+>                  |⌘>       |None          |Select in XML tag
Ctrl+"                  |⌘"       |M-"           |Select in double quotes
Ctrl+'                  |⌘'       |M-'           |Select in single quotes
Ctrl+(                  |⌘(       |M-(           |Select in parentheses
Ctrl+[                  |⌘[       |M-[           |Select in brackets
Ctrl+{                  |⌘{       |M-{           |Select in braces
Ctrl+Shift+D            |⌘⇧D      |M-S-W         |Select word
Ctrl+Shift+N            |⌘⇧N      |M-S-N         |Select line
Ctrl+Shift+P            |⌘⇧P      |M-S-P         |Select paragraph
Ctrl+Alt+U              |^U       |M-^U          |Upper case selection
Ctrl+Alt+Shift+U        |^⇧U      |M-^L          |Lower case selection
Alt+<                   |^<       |M->           |Enclose as XML tags
Alt+>                   |^>       |None          |Enclose as single XML tag
Alt+"                   |^"       |None          |Enclose in double quotes
Alt+'                   |^'       |None          |Enclose in single quotes
Alt+(                   |^(       |M-)           |Enclose in parentheses
Alt+[                   |^[       |M-]           |Enclose in brackets
Alt+{                   |^{       |M-}           |Enclose in braces
Ctrl+Shift+Up           |^⇧⇡      |S-^Up         |Move selected lines up
Ctrl+Shift+Down         |^⇧⇣      |S-^Down       |Move selected lines down
**Search**               |    |             |
Ctrl+F                   |⌘F  |M-F<br/>M-S-F|Find
Ctrl+G<br/>F3            |⌘G  |M-G          |Find next
Ctrl+Shift+G<br/>Shift+F3|⌘⇧G |M-S-G        |Find previous
Ctrl+Alt+R               |^R  |M-R          |Replace
Ctrl+Alt+Shift+R         |^⇧R |M-S-R        |Replace all
Ctrl+Alt+F               |^⌘F |M-^F         |Find incremental
Ctrl+Shift+F             |⌘⇧F |None         |Find in files
Ctrl+Alt+G               |^⌘G |None         |Goto next file found
Ctrl+Alt+Shift+G         |^⌘⇧G|None         |Goto previous file found
Ctrl+J                   |⌘J  |^J           |Jump to line
**Tools**       |       |             |
Ctrl+E          |⌘E     |M-C          |Command entry
Ctrl+Shift+E    |⌘⇧E    |M-S-C        |Select command
Ctrl+R          |⌘R     |^R           |Run
Ctrl+Shift+R    |⌘⇧R    |M-^R         |Compile
Ctrl+Shift+A    |⌘⇧A    |None         |Set Arguments...
Ctrl+Shift+B    |⌘⇧B    |M-^B         |Build
Ctrl+Shift+X    |⌘⇧X    |M-^X         |Stop
Ctrl+Alt+E      |^⌘E    |M-X          |Next Error
Ctrl+Alt+Shift+E|^⌘⇧E   |M-S-X        |Previous Error
Ctrl+Space      |⌥Esc   |^Space       |Complete symbol
Ctrl+H          |^H     |M-H<br/>M-S-H|Show documentation
Tab             |⇥      |Tab          |Expand snippet or next placeholder
Ctrl+K          |⌥⇥     |M-K          |Insert snippet...
Shift+Tab       |⇧⇥     |S-Tab        |Previous snippet placeholder
Esc             |Esc    |Esc          |Cancel snippet
Ctrl+F2         |⌘F2    |F1           |Toggle bookmark
Ctrl+Shift+F2   |⌘⇧F2   |F6           |Clear bookmarks
F2              |F2     |F2           |Next bookmark
Shift+F2        |⇧F2    |F3           |Previous bookmark
Alt+F2          |⌥F2    |F4           |Goto bookmark...
Ctrl+U          |⌘U     |^U           |Quickly open `_USERHOME`
None            |None   |None         |Quickly open `_HOME`
Ctrl+Alt+Shift+O|^⌘⇧O   |M-S-O        |Quickly open current directory
Ctrl+Alt+Shift+P|^⌘⇧P   |M-^P         |Quickly open current project
Ctrl+I          |⌘I     |M-S-I        |Show style
**Buffer**      |      |             |
Ctrl+Tab        |^⇥    |M-N          |Next buffer
Ctrl+Shift+Tab  |^⇧⇥   |M-P          |Previous buffer
Ctrl+B          |⌘B    |M-B<br/>M-S-B|Switch to buffer...
None            |None  |None         |Tab width: 2
None            |None  |None         |Tab width: 3
None            |None  |None         |Tab width: 4
None            |None  |None         |Tab width: 8
Ctrl+Alt+Shift+T|^⇧T   |M-T<br/>M-S-T|Toggle use tabs
Ctrl+Alt+I      |^I    |M-I          |Convert indentation
None            |None  |None         |CR+LF EOL mode
None            |None  |None         |LF EOL mode
None            |None  |None         |UTF-8 encoding
None            |None  |None         |ASCII encoding
None            |None  |None         |ISO-8859-1 encoding
None            |None  |None         |UTF-16 encoding
Ctrl+Alt+Enter  |^↩    |None         |Toggle view EOL
Ctrl+Alt+\\     |^\\   |None         |Toggle wrap mode
Ctrl+Alt+Shift+S|^⇧S   |None         |Toggle view whitespace
Ctrl+Shift+L    |⌘⇧L   |M-S-L        |Select lexer...
F5              |F5    |^L<br/>F5    |Refresh syntax highlighting
**View**                 |         |                 |
Ctrl+Alt+N               |^⌥⇥      |M-^V N           |Next view
Ctrl+Alt+P               |^⌥⇧⇥     |M-^V P           |Previous view
Ctrl+Alt+S<br/>Ctrl+Alt+H|^S       |M-^V S<br/>M-^V H|Split view horizontal
Ctrl+Alt+V               |^V       |M-^V V           |Split view vertical
Ctrl+Alt+W               |^W       |M-^V W           |Unsplit view
Ctrl+Alt+Shift+W         |^⇧W      |M-^V S-W         |Unsplit all views
Ctrl+Alt++<br/>Ctrl+Alt+=|^+<br/>^=|M-^V +<br/>M-^V =|Grow view
Ctrl+Alt+-               |^-       |M-^V -           |Shrink view
Ctrl+*                   |⌘*       |M-*              |Toggle current fold
Ctrl+Alt+Shift+I         |^⇧I      |N/A              |Toggle indent guides
Ctrl+Alt+Shift+V         |^⇧V      |None             |Toggle virtual space
Ctrl+=                   |⌘=       |N/A              |Zoom in
Ctrl+-                   |⌘-       |N/A              |Zoom out
Ctrl+0                   |⌘0       |N/A              |Reset zoom
**Help**|    |    |
F1      |F1  |None|Open manual
Shift+F1|⇧F1 |None|Open LuaDoc
None    |None|None|About
**Movement**    |            |            |
Down            |⇣<br/>^N    |^N<br/>Down |Line down
Shift+Down      |⇧⇣<br/>^⇧N  |S-Down      |Line down extend selection
Ctrl+Down       |^⇣          |^Down       |Scroll line down
Alt+Shift+Down  |⌥⇧⇣         |M-S-Down    |Line down extend rect. selection
Up              |⇡<br/>^P    |^P<br/>Up   |Line up
Shift+Up        |⇧⇡<br/>^⇧P  |S-Up        |Line up extend selection
Ctrl+Up         |^⇡          |^Up         |Scroll line up
Alt+Shift+Up    |⌥⇧⇡         |M-S-Up      |Line up extend rect. selection
Left            |⇠<br/>^B    |^B<br/>Left |Char left
Shift+Left      |⇧⇠<br/>^⇧B  |S-Left      |Char left extend selection
Ctrl+Left       |^⇠<br/>^⌘B  |^Left       |Word left
Ctrl+Shift+Left |^⇧⇠<br/>^⌘⇧B|S-^Left     |Word left extend selection
Alt+Shift+Left  |⌥⇧⇠         |M-S-Left    |Char left extend rect. selection
Right           |⇢<br/>^F    |^F<br/>Right|Char right
Shift+Right     |⇧⇢<br/>^⇧F  |S-Right     |Char right extend selection
Ctrl+Right      |^⇢<br/>^⌘F  |^Right      |Word right
Ctrl+Shift+Right|^⇧⇢<br/>^⌘⇧F|S-^Right    |Word right extend selection
Alt+Shift+Right |⌥⇧⇢         |M-S-Right   |Char right extend rect. selection
Home            |⌘⇠<br/>^A   |^A<br/>Home |Line start
Shift+Home      |⌘⇧⇠<br/>^⇧A |M-S-A       |Line start extend selection
Ctrl+Home       |⌘⇡<br/>⌘↖   |M-^A        |Document start
Ctrl+Shift+Home |⌘⇧⇡<br/>⌘⇧↖ |None        |Document start extend selection
Alt+Shift+Home  |⌥⇧↖         |None        |Line start extend rect. selection
End             |⌘⇢<br/>^E   |^E<br/>End  |Line end
Shift+End       |⌘⇧⇢<br/>^⇧E |M-S-E       |Line end extend selection
Ctrl+End        |⌘⇣<br/>⌘↘   |M-^E        |Document end
Ctrl+Shift+End  |⌘⇧⇣<br/>⌘⇧↘ |None        |Document end extend selection
Alt+Shift+End   |⌥⇧↘         |None        |Line end extend rect. selection
PgUp            |⇞           |PgUp        |Page up
Shift+PgUp      |⇧⇞          |M-S-U       |Page up extend selection
Alt+Shift+PgUp  |⌥⇧⇞         |None        |Page up extend rect. selection
PgDn            |⇟           |PgDn        |Page down
Shift+PgDn      |⇧⇟          |M-S-D       |Page down extend selection
Alt+Shift+PgDn  |⌥⇧⇟         |None        |Page down extend rect. selection
Ctrl+Del        |⌘⌦          |^Del        |Delete word right
Ctrl+Shift+Del  |⌘⇧⌦         |S-^Del      |Delete line right
Ins             |Ins         |Ins         |Toggle overtype
Bksp            |⌫<br/>⇧⌫    |^H<br/>Bksp |Delete back
Ctrl+Bksp       |⌘⌫          |None        |Delete word left
Ctrl+Shift+Bksp |⌘⇧⌫         |None        |Delete line left
Tab             |⇥           |Tab<br/>^I  |Insert tab or indent
Shift+Tab       |⇧⇥          |S-Tab       |Dedent
None            |^K          |^K          |Cut to line end
None            |^L          |None        |Center line vertically
N/A             |N/A         |^^          |Mark text at the caret position
N/A             |N/A         |^]          |Swap caret and mark anchor
**UTF-8 Input**          |            |                |
Ctrl+Shift+U *xxxx* Enter|⌘⇧U *xxxx* ↩|M-U *xxxx* Enter|Insert U-*xxxx* char.
**Find Fields**|               |            |
Left           |⇠<br/>^B       |^B<br/>Left |Cursor left
Right          |⇢<br/>^F       |^F<br/>Right|Cursor right
Del            |⌦              |Del         |Delete forward
Bksp           |⌫              |^H<br/>Bksp |Delete back
Ctrl+V         |⌘V             |^V          |Paste
N/A            |N/A            |^X          |Cut all
N/A            |N/A            |^Y          |Copy all
N/A            |N/A            |^U          |Erase all
Home           |↖<br/>⌘⇠<br/>^A|^A          |Home
End            |↘<br/>⌘⇢<br/>^E|^E          |End
N/A            |N/A            |^T          |Transpose characters
N/A            |N/A            |Tab         |Focus find buttons
N/A            |N/A            |S-Tab       |Focus replace buttons
Tab            |⇥              |Down        |Focus replace field
Shift+Tab      |⇧⇥             |Up          |Focus find field
Down           |⇣              |^P          |Cycle back through history
Up             |⇡              |^N          |Cycle forward through history
N/A            |N/A            |F1          |Toggle "Match Case"
N/A            |N/A            |F2          |Toggle "Whole Word"
N/A            |N/A            |F3          |Toggle "Regex"
N/A            |N/A            |F4          |Toggle "Find in Files"

†: Some terminals interpret ^Z as suspend.

‡: Ctrl+Enter in Win32 curses.

- - -

<a id="textadept.menu"></a>
# The `textadept.menu` Module

- - -

Defines the menus used by Textadept.
Menus are simply tables of menu items and submenus and may be edited in
place. A menu item itself is a table whose first element is a menu label and
whose second element is a menu command to run. Submenus have `title` keys
assigned to string text.

## Functions defined by `textadept.menu`

<a id="textadept.menu.select_command"></a>
### `textadept.menu.select_command`()

Prompts the user to select a menu command to run.


## Tables defined by `textadept.menu`

<a id="textadept.menu.context_menu"></a>
### `textadept.menu.context_menu`

The default right-click context menu.
Submenus, and menu items can be retrieved by name in addition to table index
number.

Usage:

* `textadept.menu.context_menu[#textadept.menu.context_menu + 1] = {...}`

<a id="textadept.menu.menubar"></a>
### `textadept.menu.menubar`

The default main menubar.
Individual menus, submenus, and menu items can be retrieved by name in
addition to table index number.

Usage:

* `textadept.menu.menubar[_L['_File']][_L['_New']]`
* `textadept.menu.menubar[_L['_File']][_L['_New']][2] = function() .. end`

<a id="textadept.menu.tab_context_menu"></a>
### `textadept.menu.tab_context_menu`

The default tabbar context menu.
Submenus, and menu items can be retrieved by name in addition to table index
number.

- - -

<a id="textadept.run"></a>
# The `textadept.run` Module

- - -

Compile and run source code files with Textadept.
[Language modules](#_M.Compile.and.Run) may tweak the `compile_commands`,
`run_commands`, and `error_patterns` tables for particular languages.
The user may tweak `build_commands` for particular projects.

## Fields defined by `textadept.run`

<a id="textadept.run.MARK_ERROR"></a>
### `textadept.run.MARK_ERROR` (number)

The run or compile error marker number.

<a id="textadept.run.MARK_WARNING"></a>
### `textadept.run.MARK_WARNING` (number)

The run or compile warning marker number.

<a id="events.BUILD_OUTPUT"></a>
### `events.BUILD_OUTPUT` (string)

Emitted when executing a project's build shell command.
  By default, output is printed to the message buffer. In order to override
  this behavior, connect to the event with an index of `1` and return `true`.
  Arguments:

  * `output`: A line of string output from the command.

<a id="events.COMPILE_OUTPUT"></a>
### `events.COMPILE_OUTPUT` (string)

Emitted when executing a language's compile shell command.
  By default, compiler output is printed to the message buffer. In order to
  override this behavior, connect to the event with an index of `1` and
  return `true`.
  Arguments:

  * `output`: A line of string output from the command.
  * `ext_or_lexer`: The file extension or lexer name associated with the
    executed compile command.

<a id="events.RUN_OUTPUT"></a>
### `events.RUN_OUTPUT` (string)

Emitted when executing a language's run shell command.
  By default, output is printed to the message buffer. In order to override
  this behavior, connect to the event with an index of `1` and return `true`.
  Arguments:

  * `output`: A line of string output from the command.
  * `ext_or_lexer`: The file extension or lexer name associated with the
    executed run command.

<a id="textadept.run.run_in_background"></a>
### `textadept.run.run_in_background` (bool)

Run shell commands silently in the background.
  This only applies when the message buffer is open, though it does not have
  to be visible.
  The default value is `false`.


## Functions defined by `textadept.run`

<a id="textadept.run.build"></a>
### `textadept.run.build`(*root\_directory*)

Builds the project whose root path is *root_directory* or the current project
using the shell command from the `build_commands` table.
If a "makefile" type of build file is found, prompts the user for the full
build command.
The current project is determined by either the buffer's filename or the
current working directory.
Emits `BUILD_OUTPUT` events.

Parameters:

* *`root_directory`*: The path to the project to build. The default value is
  the current project.

See also:

* [`textadept.run.build_commands`](#textadept.run.build_commands)
* [`events`](#events)

<a id="textadept.run.compile"></a>
### `textadept.run.compile`(*filename*)

Compiles file *filename* or the current file using an appropriate shell
command from the `compile_commands` table.
The shell command is determined from the file's filename, extension, or
language in that order.
Emits `COMPILE_OUTPUT` events.

Parameters:

* *`filename`*: Optional path to the file to compile. The default value is
  the current file's filename.

See also:

* [`textadept.run.compile_commands`](#textadept.run.compile_commands)
* [`events`](#events)

<a id="textadept.run.goto_error"></a>
### `textadept.run.goto_error`(*line, next*)

Jumps to the source of the recognized compile/run warning or error on line
number *line* in the message buffer.
If *line* is `nil`, jumps to the next or previous warning or error, depending
on boolean *next*. Displays an annotation with the warning or error message
if possible.

Parameters:

* *`line`*: The line number in the message buffer that contains the
  compile/run warning or error to go to.
* *`next`*: Optional flag indicating whether to go to the next recognized
  warning/error or the previous one. Only applicable when *line* is `nil` or
  `false`.

See also:

* [`textadept.run.error_patterns`](#textadept.run.error_patterns)

<a id="textadept.run.run"></a>
### `textadept.run.run`(*filename*)

Runs file *filename* or the current file using an appropriate shell command
from the `run_commands` table.
The shell command is determined from the file's filename, extension, or
language in that order.
Emits `RUN_OUTPUT` events.

Parameters:

* *`filename`*: Optional path to the file to run. The default value is the
  current file's filename.

See also:

* [`textadept.run.run_commands`](#textadept.run.run_commands)
* [`events`](#events)

<a id="textadept.run.stop"></a>
### `textadept.run.stop`()

Stops the currently running process, if any.


## Tables defined by `textadept.run`

<a id="textadept.run.build_commands"></a>
### `textadept.run.build_commands`

Map of project root paths and "makefiles" to their associated "build" shell
command line strings or functions that return such strings.
Functions may also return a working directory to operate in. By default, it
is the project's root directory.

<a id="textadept.run.compile_commands"></a>
### `textadept.run.compile_commands`

Map of filenames, file extensions, and lexer names to their associated
"compile" shell command line strings or functions that return such strings.
Command line strings may have the following macros:

  + `%f`: The file's name, including its extension.
  + `%e`: The file's name, excluding its extension.
  + `%d`: The file's directory path.
  + `%p`: The file's full path.

Functions may also return a working directory to operate in. By default, it
is the current file's parent directory.

<a id="textadept.run.error_patterns"></a>
### `textadept.run.error_patterns`

Map of file extensions and lexer names to their associated lists of string
patterns that match warning and error messages emitted by compile and run
commands for those file extensions and lexers.
Patterns match single lines and contain captures for a filename, line number,
column number (optional), and warning or error message (optional).
Double-clicking a warning or error message takes the user to the source of
that warning/error.
Note: `(.-)` captures in patterns are interpreted as filenames; `(%d+)`
captures are interpreted as line numbers first, and then column numbers; and
any other capture is treated as warning/error message text.

<a id="textadept.run.run_commands"></a>
### `textadept.run.run_commands`

Map of filenames, file extensions, and lexer names to their associated "run"
shell command line strings or functions that return strings.
Command line strings may have the following macros:

  + `%f`: The file's name, including its extension.
  + `%e`: The file's name, excluding its extension.
  + `%d`: The file's directory path.
  + `%p`: The file's full path.

Functions may also return a working directory to operate in. By default, it
is the current file's parent directory.

- - -

<a id="textadept.session"></a>
# The `textadept.session` Module

- - -

Session support for Textadept.

## Fields defined by `textadept.session`

<a id="textadept.session.default_session"></a>
### `textadept.session.default_session` (string)

The path to the default session file, *`_USERHOME`/session*, or
  *`_USERHOME`/session_term* if [`CURSES`](#CURSES) is `true`.

<a id="textadept.session.max_recent_files"></a>
### `textadept.session.max_recent_files` (number)

The maximum number of recent files to save in session files.
  Recent files are stored in [`io.recent_files`](#io.recent_files).
  The default value is `10`.

<a id="textadept.session.save_on_quit"></a>
### `textadept.session.save_on_quit` (bool)

Save the session when quitting.
  The session file saved is always `textadept.session.default_session`, even
  if a different session was loaded with [`textadept.session.load()`](#textadept.session.load).
  The default value is `true` unless the user passed the command line switch
  `-n` or `--nosession` to Textadept.


## Functions defined by `textadept.session`

<a id="textadept.session.load"></a>
### `textadept.session.load`(*filename*)

Loads session file *filename* or the user-selected session, returning `true`
if a session file was opened and read.
Textadept restores split views, opened buffers, cursor information, recent
files, and bookmarks.

Parameters:

* *`filename`*: Optional absolute path to the session file to load. If `nil`,
  the user is prompted for one.

Usage:

* `textadept.session.load(filename)`

Return:

* `true` if the session file was opened and read; `false` otherwise.

See also:

* [`textadept.session.default_session`](#textadept.session.default_session)

<a id="textadept.session.save"></a>
### `textadept.session.save`(*filename*)

Saves the session to file *filename* or the user-selected file.
Saves split views, opened buffers, cursor information, recent files, and
bookmarks.

Parameters:

* *`filename`*: Optional absolute path to the session file to save. If `nil`,
  the user is prompted for one.

Usage:

* `textadept.session.save(filename)`

See also:

* [`textadept.session.default_session`](#textadept.session.default_session)


- - -

<a id="textadept.snippets"></a>
# The `textadept.snippets` Module

- - -

Snippets for Textadept.


<a id="textadept.snippets.Overview"></a>

## Overview

Define snippets in the global `snippets` table in key-value pairs. Each pair
consists of either a string trigger word and its snippet text, or a string
lexer language (from the *lexers/* directory) with a table of trigger words
and snippet texts. When searching for a snippet to insert based on a trigger
word, Textadept considers snippets in the current lexer to have priority,
followed by the ones in the global table. This means if there are two
snippets with the same trigger word, Textadept inserts the one specific to
the current lexer, not the global one.


<a id="textadept.snippets.Special.Sequences"></a>

## Special Sequences


<a id="textadept.snippets.....n.....text...."></a>

### `%`*n*`(`*text*`)`

Represents a placeholder, where *n* is an integer and *text* is default
placeholder text. Textadept moves the caret to placeholders in numeric order
each time it calls [`textadept.snippets._insert()`](#textadept.snippets._insert), finishing at either
the "%0" placeholder if it exists or at the end of the snippet. Examples are

    snippets['foo'] = 'foobar%1(baz)'
    snippets['bar'] = 'start\n\t%0\nend'


<a id="textadept.snippets.....n.....list...."></a>

### `%`*n*`{`*list*`}`

Also represents a placeholder (where *n* is an integer), but presents a list
of choices for placeholder text constructed from comma-separated *list*.
Examples are

    snippets['op'] = 'operator(%1(1), %2(1), "%3{add,sub,mul,div}")'


<a id="textadept.snippets.....n."></a>

### `%`*n*

Represents a mirror, where *n* is an integer. Mirrors with the same *n* as a
placeholder mirror any user input in the placeholder. If no placeholder
exists for *n*, the first occurrence of that mirror in the snippet becomes
the placeholder, but with no default text. Examples are

    snippets['foo'] = '%1(mirror), %1, on the wall'
    snippets['q'] = '"%1"'


<a id="textadept.snippets.....n.....Lua.code.....br......n.....Shell.code...."></a>

### `%`*n*`<`*Lua code*`>`<br/>`%`*n*`[`*Shell code*`]`

Represents a transform, where *n* is an integer that has an associated
placeholder, *Lua code* is arbitrary Lua code, and *Shell code* is arbitrary
Shell code. Textadept executes the code as text is typed into placeholder
*n*. If the transform omits *n*, Textadept executes the transform's code the
moment the editor inserts the snippet.

Textadept runs Lua code in its Lua State and replaces the transform with the
code's return text. The code may use the temporary `text` and `selected_text`
global variables which contain placeholder *n*'s text and the text originally
selected when the snippet was inserted, respectively. An example is

    snippets['attr'] = [[
    %1(int) %2(foo) = %3;

    %1 get%2<text:gsub('^.', function(c) return c:upper() end)>() {
    	return %2;
    }
    void set%2<text:gsub('^.', function(c) return c:upper() end)>(%1 value) {
    	%2 = value;
    }
    ]]

Textadept executes shell code using Lua's [`io.popen()`][] and replaces the
transform with the process' standard output (stdout). The code may use a `%`
character to represent placeholder *n*'s text. An example is

    snippets['env'] = '$%1(HOME) = %1[echo $%]'


<a id="textadept.snippets....."></a>

### `%%`

Stands for a single '%' since '%' by itself has a special meaning in
snippets.


<a id="textadept.snippets......br......"></a>

### `%(`<br/>`%{`

Stands for a single '(' or '{', respectively, after a `%`*n* mirror.
Otherwise, the mirror would be interpreted as a placeholder or transform.
Note: it is currently not possible to escape a '<' or '[' immediately after
a `%`*n* mirror due to `%<...>` and `%[...]` sequences being interpreted as
code to execute.


<a id="textadept.snippets...t."></a>

### `\t`

A single unit of indentation based on the buffer's indentation settings
([`buffer.use_tabs`](#buffer.use_tabs) and [`buffer.tab_width`](#buffer.tab_width)).


<a id="textadept.snippets...n."></a>

### `\n`

A single set of line ending delimiters based on the buffer's end of line mode
([`buffer.eol_mode`](#buffer.eol_mode)).

[`io.popen()`]: http://www.lua.org/manual/5.3/manual.html#pdf-io.popen


## Fields defined by `textadept.snippets`

<a id="textadept.snippets.INDIC_PLACEHOLDER"></a>
### `textadept.snippets.INDIC_PLACEHOLDER` (number)

The snippet placeholder indicator number.


## Functions defined by `textadept.snippets`

<a id="textadept.snippets._cancel_current"></a>
### `textadept.snippets._cancel_current`()

Cancels the active snippet, removing all inserted text.
Returns `false` if no snippet is active.

Return:

* `false` if no snippet is active; `nil` otherwise.

<a id="textadept.snippets._insert"></a>
### `textadept.snippets._insert`(*text*)

Inserts snippet text *text* or the snippet assigned to the trigger word
behind the caret.
Otherwise, if a snippet is active, goes to the active snippet's next
placeholder. Returns `false` if no action was taken.

Parameters:

* *`text`*: Optional snippet text to insert. If `nil`, attempts to insert a
  new snippet based on the trigger, the word behind caret, and the current
  lexer.

Return:

* `false` if no action was taken; `nil` otherwise.

See also:

* [`buffer.word_chars`](#buffer.word_chars)

<a id="textadept.snippets._previous"></a>
### `textadept.snippets._previous`()

Jumps back to the previous snippet placeholder, reverting any changes from
the current one.
Returns `false` if no snippet is active.

Return:

* `false` if no snippet is active; `nil` otherwise.

<a id="textadept.snippets._select"></a>
### `textadept.snippets._select`()

Prompts the user to select a snippet to insert from a list of global and
language-specific snippets.


## Tables defined by `textadept.snippets`

<a id="_G.snippets"></a>
### `_G.snippets`

Map of snippet triggers with their snippet text or functions that return such
text, with language-specific snippets tables assigned to a lexer name key.
This table also contains the `textadept.snippets` module.

<a id="textadept.snippets._paths"></a>
### `textadept.snippets._paths`

List of directory paths to look for snippet files in.
Filenames are of the form *lexer.trigger.ext* or *trigger.ext* (*.ext* is an
optional, arbitrary file extension). If the global `snippets` table does not
contain a snippet for a given trigger, this table is consulted for a matching
filename, and the contents of that file is inserted as a snippet.
Note: If a directory has multiple snippets with the same trigger, the snippet
chosen for insertion is not defined and may not be constant.

- - -

<a id="ui"></a>
# The `ui` Module

- - -

Utilities for interacting with Textadept's user interface.

## Fields defined by `ui`

<a id="ui.bufstatusbar_text"></a>
### `ui.bufstatusbar_text` (string, Write-only)

The text displayed in the buffer statusbar.

<a id="ui.clipboard_text"></a>
### `ui.clipboard_text` (string)

The text on the clipboard.

<a id="ui.context_menu"></a>
### `ui.context_menu` (userdata)

The buffer's context menu, a [`ui.menu()`](#ui.menu).
  This is a low-level field. You probably want to use the higher-level
  [`textadept.menu.context_menu`](#textadept.menu.context_menu).

<a id="ui.maximized"></a>
### `ui.maximized` (bool)

Whether or not Textadept's window is maximized.

<a id="ui.silent_print"></a>
### `ui.silent_print` (bool)

Whether or not to print messages to buffers silently.
  This is not guaranteed to be a constant value, as Textadept may change it
  for the editor's own purposes. This flag should be used only in conjunction
  with a group of [`ui.print()`](#ui.print) and [`ui._print()`](#ui._print) function calls.
  The default value is `false`, and focuses buffers when messages are printed
  to them.

<a id="ui.statusbar_text"></a>
### `ui.statusbar_text` (string, Write-only)

The text displayed in the statusbar.

<a id="ui.tab_context_menu"></a>
### `ui.tab_context_menu` (userdata)

The context menu for the buffer's tab, a [`ui.menu()`](#ui.menu).
  This is a low-level field. You probably want to use the higher-level
  [`textadept.menu.tab_context_menu`](#textadept.menu.tab_context_menu).

<a id="ui.tabs"></a>
### `ui.tabs` (bool)

Whether or not to display the tab bar when multiple buffers are open.
  The default value is `true`.

<a id="ui.title"></a>
### `ui.title` (string, Write-only)

The title text of Textadept's window.


## Functions defined by `ui`

<a id="ui._print"></a>
### `ui._print`(*buffer\_type, ...*)

Prints the given string messages to the buffer of string type *buffer_type*.
Opens a new buffer for printing messages to if necessary. If the message
buffer is already open in a view, the message is printed to that view.
Otherwise the view is split (unless `ui.tabs` is `true`) and the message
buffer is displayed before being printed to.

Parameters:

* *`buffer_type`*: String type of message buffer.
* *`...`*: Message strings.

Usage:

* `ui._print(_L['[Message Buffer]'], message)`

<a id="ui.dialog"></a>
### `ui.dialog`(*kind, ...*)

Low-level function for prompting the user with a [gtdialog][] of kind *kind*
with the given string and table arguments, returning a formatted string of
the dialog's output.
You probably want to use the higher-level functions in the [`ui.dialogs`](#ui.dialogs)
module.
Table arguments containing strings are allowed and expanded in place. This is
useful for filtered list dialogs with many items.

[gtdialog]: http://foicica.com/gtdialog/manual.html#Usage

Parameters:

* *`kind`*: The kind of gtdialog.
* *`...`*: Parameters to the gtdialog.

Return:

* string gtdialog result.

<a id="ui.get_split_table"></a>
### `ui.get_split_table`()

Returns a split table that contains Textadept's current split view structure.
This is primarily used in session saving.

Return:

* table of split views. Each split view entry is a table with 4
  fields: `1`, `2`, `vertical`, and `size`. `1` and `2` have values of either
  nested split view entries or the views themselves; `vertical` is a flag
  that indicates if the split is vertical or not; and `size` is the integer
  position of the split resizer.

<a id="ui.goto_file"></a>
### `ui.goto_file`(*filename, split, preferred\_view, sloppy*)

Switches to the existing view whose buffer's filename is *filename*.
If no view was found and *split* is `true`, splits the current view in order
to show the requested file. If *split* is `false`, shifts to the next or
*preferred_view* view in order to show the requested file. If *sloppy* is
`true`, requires only the last part of *filename* to match a buffer's
`filename`. If the requested file was not found, it is opened in the desired
view.

Parameters:

* *`filename`*: The filename of the buffer to go to.
* *`split`*: Optional flag that indicates whether or not to open the buffer
  in a split view if there is only one view. The default value is `false`.
* *`preferred_view`*: Optional view to open the desired buffer in if the
  buffer is not visible in any other view.
* *`sloppy`*: Optional flag that indicates whether or not to not match
  *filename* to `buffer.filename` exactly. When `true`, matches *filename* to
  only the last part of `buffer.filename` This is useful for run and compile
  commands which output relative filenames and paths instead of full ones and
  it is likely that the file in question is already open. The default value
  is `false`.

<a id="ui.goto_view"></a>
### `ui.goto_view`(*view*)

Shifts to view *view* or the view *view* number of views relative to the
current one.
Emits `VIEW_BEFORE_SWITCH` and `VIEW_AFTER_SWITCH` events.

Parameters:

* *`view`*: A view or relative view number (typically 1 or -1).

See also:

* [`_VIEWS`](#_VIEWS)
* [`events.VIEW_BEFORE_SWITCH`](#events.VIEW_BEFORE_SWITCH)
* [`events.VIEW_AFTER_SWITCH`](#events.VIEW_AFTER_SWITCH)

<a id="ui.menu"></a>
### `ui.menu`(*menu\_table*)

Low-level function for creating a menu from table *menu_table* and returning
the userdata.
You probably want to use the higher-level `textadept.menu.menubar`,
`textadept.menu.context_menu`, or `textadept.menu.tab_context_menu` tables.
Emits a `MENU_CLICKED` event when a menu item is selected.

Parameters:

* *`menu_table`*: A table defining the menu. It is an ordered list of tables
  with a string menu item, integer menu ID, and optional GDK keycode and
  modifier mask. The latter two are used to display key shortcuts in the
  menu. '_' characters are treated as a menu mnemonics. If the menu item is
  empty, a menu separator item is created. Submenus are just nested
  menu-structure tables. Their title text is defined with a `title` key.

Usage:

* `ui.menu{{'_New', 1}, {'_Open', 2}, {''}, {'_Quit', 4}}`
* `ui.menu{{'_New', 1, string.byte('n'), 4}} -- 'Ctrl+N'`

See also:

* [`events.MENU_CLICKED`](#events.MENU_CLICKED)
* [`textadept.menu.menubar`](#textadept.menu.menubar)
* [`textadept.menu.context_menu`](#textadept.menu.context_menu)
* [`textadept.menu.tab_context_menu`](#textadept.menu.tab_context_menu)

<a id="ui.print"></a>
### `ui.print`(*...*)

Prints the given string messages to the message buffer.
Opens a new buffer if one has not already been opened for printing messages.

Parameters:

* *`...`*: Message strings.

<a id="ui.set_theme"></a>
### `ui.set_theme`(*name, props*)

Switches the editor theme to string *name* and (optionally) assigns the
properties contained in table *props*.
User themes override Textadept's default themes when they have the same name.
If *name* contains slashes, it is assumed to be an absolute path to a theme
instead of a theme name.

Parameters:

* *`name`*: The name or absolute path of a theme to set.
* *`props`*: Optional table of theme property assignments that override the
  theme's defaults.

Usage:

* `ui.set_theme('light', {font = 'Monospace', fontsize = 12})`

<a id="ui.switch_buffer"></a>
### `ui.switch_buffer`(*zorder*)

Prompts the user to select a buffer to switch to.
Buffers are listed in the order they were opened unless `zorder` is `true`,
in which case buffers are listed by their z-order (most recently viewed to
least recently viewed).

Parameters:

* *`zorder`*: Flag that indicates whether or not to list buffers by their
  z-order. The default value is `false`.


## Tables defined by `ui`

<a id="ui.menubar"></a>
### `ui.menubar`

A table of menus defining a menubar. (Write-only).
This is a low-level field. You probably want to use the higher-level
`textadept.menu.menubar`.

See also:

* [`textadept.menu.menubar`](#textadept.menu.menubar)

<a id="ui.size"></a>
### `ui.size`

A table containing the width and height pixel values of Textadept's window.

- - -

<a id="ui.command_entry"></a>
# The `ui.command_entry` Module

- - -

Textadept's Command Entry.


<a id="ui.command_entry.Modes"></a>

## Modes

The command entry supports multiple [modes](#keys.Modes) that have their own
sets of key bindings stored in a separate table in `_G.keys` under a mode
prefix key. Mode names are arbitrary, but cannot conflict with lexer names or
key sequence strings (e.g. `'lua'` or `'send'`) due to the method Textadept
uses for looking up key bindings. An example mode is "lua_command" mode for
executing Lua commands:

    local function complete_lua() ... end
    local function run_lua() ... end
    keys['ce'] = function() ui.command_entry.enter_mode('lua_command') end
    keys.lua_command = {
      ['\t'] = complete_lua,
      ['\n'] = function() return ui.command_entry.finish_mode(run_lua) end
    }

In this case, `Ctrl+E` opens the command entry and enters "lua_command" key
mode where the `Tab` and `Enter` keys have special, defined functionality.
(By default, Textadept pre-defines `Esc` to exit any command entry mode.)
`Tab` shows a list of Lua completions for the entry text and `Enter` exits
"lua_command" key mode and executes the entered code. The command entry
handles all other editing and movement keys normally.

## Fields defined by `ui.command_entry`

<a id="ui.command_entry.height"></a>
### `ui.command_entry.height` (number)

The height in pixels of the command entry.


## Functions defined by `ui.command_entry`

<a id="ui.command_entry.enter_mode"></a>
### `ui.command_entry.enter_mode`(*mode, lexer, height*)

Opens the command entry in key mode *mode*, highlighting text with lexer name
*lexer*, and displaying *height* number of lines at a time.
Key bindings will be looked up in `keys[mode]` instead of `keys`. The `Esc`
key exits the current mode, closes the command entry, and restores normal key
lookup.
This function is useful for binding keys to enter a command entry mode.

Parameters:

* *`mode`*: The key mode to enter into, or `nil` to exit the current mode.
* *`lexer`*: Optional string lexer name to use for command entry text. The
  default value is `'text'`.
* *`height`*: Optional number of lines to display in the command entry. The
  default value is `1`.

Usage:

* `keys['ce'] =
  function() ui.command_entry.enter_mode('command_entry') end`

See also:

* [`keys.MODE`](#keys.MODE)

<a id="ui.command_entry.finish_mode"></a>
### `ui.command_entry.finish_mode`(*f*)

Exits the current key mode, closes the command entry, and calls function *f*
(if given) with the command entry's text as an argument.
This is useful for binding keys to exit a command entry mode and perform an
action with the entered text.

Parameters:

* *`f`*: Optional function to call. It should accept the command entry text
  as an argument.

Usage:

* `keys['\n'] =
  function() return ui.command_entry.finish_mode(ui.print) end`

<a id="ui.command_entry.focus"></a>
### `ui.command_entry.focus`()

Opens the command entry.


## Tables defined by `ui.command_entry`

<a id="ui.command_entry.editing_keys"></a>
### `ui.command_entry.editing_keys`

A metatable with typical platform-specific key bindings for text entries.
This metatable may be used to add basic editing keys to command entry modes.
It is automatically added to command entry modes unless a metatable was
previously set.

Usage:

* `setmetatable(keys.my_mode, ui.command_entry.editing_keys)`

- - -

<a id="ui.dialogs"></a>
# The `ui.dialogs` Module

- - -

Provides a set of interactive dialog prompts for user input.

## Functions defined by `ui.dialogs`

<a id="ui.dialogs.colorselect"></a>
### `ui.dialogs.colorselect`(*options*)

Prompts the user with a color selection dialog defined by dialog options
table *options*, returning the color selected.
If the user canceled the dialog, returns `nil`.

Parameters:

* *`options`*: Table of key-value option pairs for the option select dialog.

  * `title`: The dialog's title text.
  * `color`: The initially selected color as either a number in "0xBBGGRR"
    format, or as a string in "#RRGGBB" format.
  * `palette`: The list of colors to show in the dialog's color palette.
    Up to 20 colors can be specified as either numbers in "0xBBGGRR" format
    or as strings in "#RRGGBB" format. If `true` (no list was given), a
    default palette is shown.
  * `string_output`: Return the selected color in string "#RRGGBB" format
    instead of numeric "0xBBGGRR" format. The default value is `false`.
  * `float`: Show the dialog on top of all desktop windows. The default value
    is `false`.

Usage:

* `ui.dialogs.colorselect{title = 'Foreground color', color = 0x000000,
  palette = {'#000000', 0x0000FF, '#00FF00', 0xFF0000}}`

Return:

* selected color

<a id="ui.dialogs.dropdown"></a>
### `ui.dialogs.dropdown`(*options*)

Prompts the user with a drop-down item selection dialog defined by dialog
options table *options*, returning the selected button's index along with the
index of the selected item.
If *options*.`string_output` is `true`, returns the selected button's label
along with the selected item's text.
If the dialog closed due to *options*.`exit_onchange`, returns `4` along with
either the selected item's index or its text. If the dialog timed out,
returns `0` or `"timeout"`. If the user canceled the dialog, returns `-1` or
`"delete"`.

Parameters:

* *`options`*: Table of key-value option pairs for the drop-down dialog.

  * `title`: The dialog's title text.
  * `text`: The dialog's main message text.
  * `items`: The list of string items to show in the drop-down.
  * `button1`: The right-most button's label. The default value is
    `_L['_OK']`.
  * `button2`: The middle button's label.
  * `button3`: The left-most button's label. This option requires `button2`
    to be set.
  * `exit_onchange`: Close the dialog after selecting a new item. The default
    value is `false`.
  * `select`: The index of the initially selected list item. The default
    value is `1`.
  * `string_output`: Return the selected button's label (instead of its
    index) and the selected item's text (instead of its index). If no item
    was selected, returns the dialog's exit status (instead of its exit
    code). The default value is `false`.
  * `width`: The dialog's pixel width.
  * `height`: The dialog's pixel height.
  * `float`: Show the dialog on top of all desktop windows. The default value
    is `false`.
  * `timeout`: The integer number of seconds the dialog waits for the user to
    select a button before timing out. Dialogs do not time out by default.

Usage:

* `ui.dialogs.dropdown{title = 'Select Encoding', width = 200,
  items = io.encodings, string_output = true}`

Return:

* selected button or exit code, selected item

<a id="ui.dialogs.filesave"></a>
### `ui.dialogs.filesave`(*options*)

Prompts the user with a file save dialog defined by dialog options table
*options*, returning the string file chosen.
If the user canceled the dialog, returns `nil`.

Parameters:

* *`options`*: Table of key-value option pairs for the dialog.

  * `title`: The dialog's title text.
  * `with_directory`: The initial filesystem directory to show.
  * `with_file`: The initially chosen filename. This option requires
    `with_directory` to be set.
  * `with_extension`: The list of extensions selectable files must have.
  * `no_create_directories`: Prevent the user from creating new directories.
    The default value is `false`.

Return:

* filename or nil

<a id="ui.dialogs.fileselect"></a>
### `ui.dialogs.fileselect`(*options*)

Prompts the user with a file selection dialog defined by dialog options
table *options*, returning the string file selected.
If *options*.`select_multiple` is `true`, returns the list of files selected.
If the user canceled the dialog, returns `nil`.

Parameters:

* *`options`*: Table of key-value option pairs for the dialog.

  * `title`: The dialog's title text.
  * `with_directory`: The initial filesystem directory to show.
  * `with_file`: The initially selected filename. This option requires
    `with_directory` to be set.
  * `with_extension`: The list of extensions selectable files must have.
  * `select_multiple`: Allow the user to select multiple files. The default
    value is `false`.
  * `select_only_directories`: Only allow the user to select directories. The
    default value is `false`.

Usage:

* `ui.dialogs.fileselect{title = 'Open C File', with_directory = _HOME,
  with_extension = {'c', 'h'}, select_multiple = true}`

Return:

* filename, list of filenames, or nil

<a id="ui.dialogs.filteredlist"></a>
### `ui.dialogs.filteredlist`(*options*)

Prompts the user with a filtered list item selection dialog defined by dialog
options table *options*, returning the selected button's index along with the
index or indices of the selected item or items (depending on whether or not
*options*.`select_multiple` is `true`).
If *options*.`string_output` is `true`, returns the selected button's label
along with the text of the selected item or items.
If the dialog timed out, returns `0` or `"timeout"`. If the user canceled the
dialog, returns `-1` or `"delete"`.
Spaces in the filter text are treated as wildcards.

Parameters:

* *`options`*: Table of key-value option pairs for the filtered list dialog.

  * `title`: The dialog's title text.
  * `informative_text`: The dialog's main message text.
  * `text`: The dialog's initial input text.
  * `columns`: The list of string column names for list rows.
  * `items`: The list of string items to show in the filtered list.
  * `button1`: The right-most button's label. The default value is
    `_L['_OK']`.
  * `button2`: The middle button's label.
  * `button3`: The left-most button's label. This option requires `button2`
    to be set.
  * `select_multiple`: Allow the user to select multiple items. The default
    value is `false`.
  * `search_column`: The column number to filter the input text against. The
    default value is `1`. This option requires `columns` to be set and
    contain at least *n* column names.
  * `output_column`: The column number to use for `string_output`. The
    default value is `1`. This option requires `columns` to be set and
    contain at least *n* column names.
  * `string_output`: Return the selected button's label (instead of its
    index) and the selected item's text (instead of its index). If no item
    was selected, returns the dialog's exit status (instead of its exit
    code). The default value is `false`.
  * `width`: The dialog's pixel width.
  * `height`: The dialog's pixel height.
  * `float`: Show the dialog on top of all desktop windows. The default value
    is `false`.
  * `timeout`: The integer number of seconds the dialog waits for the user to
    select a button before timing out. Dialogs do not time out by default.

Usage:

* `ui.dialogs.filteredlist{title = 'Title', columns = {'Foo', 'Bar'},
  items = {'a', 'b', 'c', 'd'}}`

Return:

* selected button or exit code, selected item or list of selected items

<a id="ui.dialogs.fontselect"></a>
### `ui.dialogs.fontselect`(*options*)

Prompts the user with a font selection dialog defined by dialog options
table *options*, returning the font selected (including style and size).
If the user canceled the dialog, returns `nil`.

Parameters:

* *`options`*: Table of key-value option pairs for the option select dialog.

  * `title`: The dialog's title text.
  * `text`: The font preview text.
  * `font-name`: The initially selected font name.
  * `font-size`: The initially selected font size. The default value is `12`.
  * `font-style`: The initially selected font style. The available options
    are `"regular"`, `"bold"`, `"italic"`, and `"bold italic"`. The default
    value is `"regular"`.
  * `float`: Show the dialog on top of all desktop windows. The default value
    is `false`.

Usage:

* `ui.dialogs.fontselect{title = 'Font', font_name = 'Monospace',
  font_size = 10}`

Return:

* selected font, including style and size

<a id="ui.dialogs.inputbox"></a>
### `ui.dialogs.inputbox`(*options*)

Prompts the user with an inputbox dialog defined by dialog options table
*options*, returning the selected button's index along with the user's
input text (the latter as a string or table, depending on the type of
*options*.`informative_text`).
If *options*.`string_output` is `true`, returns the selected button's label
along with the user's input text.
If the dialog timed out, returns `0` or `"timeout"`. If the user canceled the
dialog, returns `-1` or `"delete"`.

Parameters:

* *`options`*: Table of key-value option pairs for the inputbox.

  * `title`: The dialog's title text.
  * `informative_text`: The dialog's main message text. If the value is a
    table, the first table value is the main message text and any subsequent
    values are used as the labels for multiple entry boxes. Providing a
    single label has no effect.
  * `text`: The dialog's initial input text. If the value is a table, the
    table values are used to populate the multiple entry boxes defined by
    `informative_text`.
  * `button1`: The right-most button's label. The default value is
    `_L['_OK']`.
  * `button2`: The middle button's label.
  * `button3`: The left-most button's label. This option requires `button2`
    to be set.
  * `string_output`: Return the selected button's label (instead of its
    index) or the dialog's exit status instead of the button's index (instead
    of its exit code). The default value is `false`.
  * `width`: The dialog's pixel width.
  * `height`: The dialog's pixel height.
  * `float`: Show the dialog on top of all desktop windows. The default value
    is `false`.
  * `timeout`: The integer number of seconds the dialog waits for the user to
    select a button before timing out. Dialogs do not time out by default.

Usage:

* `ui.dialogs.inputbox{title = 'Goto Line', informative_text = 'Line:',
  text = '1'}`

Return:

* selected button or exit code, input text

<a id="ui.dialogs.msgbox"></a>
### `ui.dialogs.msgbox`(*options*)

Prompts the user with a generic message box dialog defined by dialog options
table *options*, returning the selected button's index.
If *options*.`string_output` is `true`, returns the selected button's label.
If the dialog timed out, returns `0` or `"timeout"`. If the user canceled the
dialog, returns `-1` or `"delete"`.

Parameters:

* *`options`*: Table of key-value option pairs for the message box.

  * `title`: The dialog's title text.
  * `text`: The dialog's main message text.
  * `informative_text`: The dialog's extra informative text.
  * `icon`: The dialog's GTK stock icon name. Examples are
    "gtk-dialog-error", "gtk-dialog-info", "gtk-dialog-question", and
    "gtk-dialog-warning". The dialog does not display an icon by default.
  * `icon_file`: The dialog's icon file path. This option has no effect when
    `icon` is set.
  * `button1`: The right-most button's label. The default value is
    `_L['_OK']`.
  * `button2`: The middle button's label.
  * `button3`: The left-most button's label. This option requires `button2`
    to be set.
  * `string_output`: Return the selected button's label (instead of its
    index) or the dialog's exit status instead of the button's index (instead
    of its exit code). The default value is `false`.
  * `width`: The dialog's pixel width.
  * `height`: The dialog's pixel height.
  * `float`: Show the dialog on top of all desktop windows. The default value
    is `false`.
  * `timeout`: The integer number of seconds the dialog waits for the user to
    select a button before timing out. Dialogs do not time out by default.

Usage:

* `ui.dialogs.msgbox{title = 'EOL Mode', text = 'Which EOL?',
  icon = 'gtk-dialog-question', button1 = 'CRLF', button2 = 'CR',
  button3 = 'LF'}`

Return:

* selected button or exit code

<a id="ui.dialogs.ok_msgbox"></a>
### `ui.dialogs.ok_msgbox`(*options*)

Prompts the user with a generic message box dialog defined by dialog options
table *options* and with localized "Ok" and "Cancel" buttons, returning the
selected button's index.
If *options*.`string_output` is `true`, returns the selected button's label.
If the dialog timed out, returns `0` or `"timeout"`. If the user canceled the
dialog, returns `-1` or `"delete"`.

Parameters:

* *`options`*: Table of key-value option pairs for the message box.

  * `title`: The dialog's title text.
  * `text`: The dialog's main message text.
  * `informative_text`: The dialog's extra informative text.
  * `icon`: The dialog's GTK stock icon name. Examples are
    "gtk-dialog-error", "gtk-dialog-info", "gtk-dialog-question", and
    "gtk-dialog-warning". The dialog does not display an icon by default.
  * `icon_file`: The dialog's icon file path. This option has no effect when
    `icon` is set.
  * `no_cancel`: Do not display the "Cancel" button. The default value is
    `false`.
  * `string_output`: Return the selected button's label (instead of its
    index) or the dialog's exit status instead of the button's index (instead
    of its exit code). The default value is `false`.
  * `width`: The dialog's pixel width.
  * `height`: The dialog's pixel height.
  * `float`: Show the dialog on top of all desktop windows. The default value
    is `false`.
  * `timeout`: The integer number of seconds the dialog waits for the user to
    select a button before timing out. Dialogs do not time out by default.

Return:

* selected button or exit code

<a id="ui.dialogs.optionselect"></a>
### `ui.dialogs.optionselect`(*options*)

Prompts the user with an option selection dialog defined by dialog options
table *options*, returning the selected button's index along with the indices
of the selected options.
If *options*.`string_output` is `true`, returns the selected button's label
along with the text of the selected options.
If the dialog timed out, returns `0` or `"timeout"`. If the user canceled the
dialog, returns `-1` or `"delete"`.

Parameters:

* *`options`*: Table of key-value option pairs for the option select dialog.

  * `title`: The dialog's title text.
  * `text`: The dialog's main message text.
  * `items`: The list of string options to show in the option group.
  * `button1`: The right-most button's label. The default value is
    `_L['_OK']`.
  * `button2`: The middle button's label.
  * `button3`: The left-most button's label. This option requires `button2`
    to be set.
  * `select`: The indices of initially selected options.
  * `string_output`: Return the selected button's label or the dialog's exit
    status along with the selected options' text instead of the button's
    index or the dialog's exit code along with the options' indices. The
    default value is `false`.
  * `width`: The dialog's pixel width.
  * `height`: The dialog's pixel height.
  * `float`: Show the dialog on top of all desktop windows. The default value
    is `false`.
  * `timeout`: The integer number of seconds the dialog waits for the user to
    select a button before timing out. Dialogs do not time out by default.

Usage:

* `ui.dialogs.optionselect{title = 'Language',
  informative_text = 'Check the languages you understand'
  items = {'English', 'Romanian'}, select = 1, string_output = true}`

Return:

* selected button or exit code, list of selected options

<a id="ui.dialogs.secure_inputbox"></a>
### `ui.dialogs.secure_inputbox`(*options*)

Prompts the user with a masked inputbox dialog defined by dialog options
table *options*, returning the selected button's index along with the user's
input text (the latter as a string or table, depending on the type of
*options*.`informative_text`).
If *options*.`string_output` is `true`, returns the selected button's label
along with the user's input text.
If the dialog timed out, returns `0` or `"timeout"`. If the user canceled the
dialog, returns `-1` or `"delete"`.

Parameters:

* *`options`*: Table of key-value option pairs for the inputbox.

  * `title`: The dialog's title text.
  * `informative_text`: The dialog's main message text. If the value is a
    table, the first table value is the main message text and any subsequent
    values are used as the labels for multiple entry boxes. Providing a
    single label has no effect.
  * `text`: The dialog's initial input text. If the value is a table, the
    table values are used to populate the multiple entry boxes defined by
    `informative_text`.
  * `button1`: The right-most button's label. The default value is
    `_L['_OK']`.
  * `button2`: The middle button's label.
  * `button3`: The left-most button's label. This option requires `button2`
    to be set.
  * `string_output`: Return the selected button's label (instead of its
    index) or the dialog's exit status instead of the button's index (instead
    of its exit code). The default value is `false`.
  * `width`: The dialog's pixel width.
  * `height`: The dialog's pixel height.
  * `float`: Show the dialog on top of all desktop windows. The default value
    is `false`.
  * `timeout`: The integer number of seconds the dialog waits for the user to
    select a button before timing out. Dialogs do not time out by default.

Return:

* selected button or exit code, input text

<a id="ui.dialogs.secure_standard_inputbox"></a>
### `ui.dialogs.secure_standard_inputbox`(*options*)

Prompts the user with a masked inputbox dialog defined by dialog options
table *options* and with localized "Ok" and "Cancel" buttons, returning the
selected button's index along with the user's input text (the latter as a
string or table, depending on the type of *options*.`informative_text`).
If *options*.`string_output` is `true`, returns the selected button's label
along with the user's input text.
If the dialog timed out, returns `0` or `"timeout"`. If the user canceled the
dialog, returns `-1` or `"delete"`.

Parameters:

* *`options`*: Table of key-value option pairs for the inputbox.

  * `title`: The dialog's title text.
  * `informative_text`: The dialog's main message text. If the value is a
    table, the first table value is the main message text and any subsequent
    values are used as the labels for multiple entry boxes. Providing a
    single label has no effect.
  * `text`: The dialog's initial input text. If the value is a table, the
    table values are used to populate the multiple entry boxes defined by
    `informative_text`.
  * `no_cancel`: Do not display the "Cancel" button. The default value is
    `false`.
  * `string_output`: Return the selected button's label (instead of its
    index) or the dialog's exit status instead of the button's index (instead
    of its exit code). The default value is `false`.
  * `width`: The dialog's pixel width.
  * `height`: The dialog's pixel height.
  * `float`: Show the dialog on top of all desktop windows. The default value
    is `false`.
  * `timeout`: The integer number of seconds the dialog waits for the user to
    select a button before timing out. Dialogs do not time out by default.

Return:

* selected button or exit code, input text

<a id="ui.dialogs.standard_dropdown"></a>
### `ui.dialogs.standard_dropdown`(*options*)

Prompts the user with a drop-down item selection dialog defined by dialog
options table *options* and with localized "Ok" and "Cancel" buttons,
returning the selected button's index along with the selected item's index.
If *options*.`string_output` is `true`, returns the selected button's label
along with the selected item's text.
If the dialog closed due to *options*.`exit_onchange`, returns `4` along with
either the selected item's index or its text. If the dialog timed out,
returns `0` or `"timeout"`. If the user canceled the dialog, returns `-1` or
`"delete"`.

Parameters:

* *`options`*: Table of key-value option pairs for the drop-down dialog.

  * `title`: The dialog's title text.
  * `text`: The dialog's main message text.
  * `items`: The list of string items to show in the drop-down.
  * `no_cancel`: Do not display the "Cancel" button. The default value is
    `false`.
  * `exit_onchange`: Close the dialog after selecting a new item. The default
    value is `false`.
  * `select`: The index of the initially selected list item. The default
    value is `1`.
  * `string_output`: Return the selected button's label (instead of its
    index) and the selected item's text (instead of its index). If no item
    was selected, returns the dialog's exit status (instead of its exit
    code). The default value is `false`.
  * `width`: The dialog's pixel width.
  * `height`: The dialog's pixel height.
  * `float`: Show the dialog on top of all desktop windows. The default value
    is `false`.
  * `timeout`: The integer number of seconds the dialog waits for the user to
    select a button before timing out. Dialogs do not time out by default.

Return:

* selected button or exit code, selected item

<a id="ui.dialogs.standard_inputbox"></a>
### `ui.dialogs.standard_inputbox`(*options*)

Prompts the user with an inputbox dialog defined by dialog options table
*options* and with localized "Ok" and "Cancel" buttons, returning the
selected button's index along with the user's input text (the latter as a
string or table, depending on the type of *options*.`informative_text`).
If *options*.`string_output` is `true`, returns the selected button's label
along with the user's input text.
If the dialog timed out, returns `0` or `"timeout"`. If the user canceled the
dialog, returns `-1` or `"delete"`.

Parameters:

* *`options`*: Table of key-value option pairs for the inputbox.

  * `title`: The dialog's title text.
  * `informative_text`: The dialog's main message text. If the value is a
    table, the first table value is the main message text and any subsequent
    values are used as the labels for multiple entry boxes. Providing a
    single label has no effect.
  * `text`: The dialog's initial input text. If the value is a table, the
    table values are used to populate the multiple entry boxes defined by
    `informative_text`.
  * `no_cancel`: Do not display the "Cancel" button. The default value is
    `false`.
  * `string_output`: Return the selected button's label (instead of its
    index) or the dialog's exit status instead of the button's index (instead
    of its exit code). The default value is `false`.
  * `width`: The dialog's pixel width.
  * `height`: The dialog's pixel height.
  * `float`: Show the dialog on top of all desktop windows. The default value
    is `false`.
  * `timeout`: The integer number of seconds the dialog waits for the user to
    select a button before timing out. Dialogs do not time out by default.

Return:

* selected button or exit code, input text

<a id="ui.dialogs.textbox"></a>
### `ui.dialogs.textbox`(*options*)

Prompts the user with a multiple-line textbox dialog defined by dialog
options table *options*, returning the selected button's index.
If *options*.`string_output` is `true`, returns the selected button's label.
If *options*.`editable` is `true`, also returns the textbox's text. If the
dialog timed out, returns `0` or `"timeout"`. If the user canceled the
dialog, returns `-1` or `"delete"`.

Parameters:

* *`options`*: Table of key-value option pairs for the dialog.

  * `title`: The dialog's title text.
  * `informative_text`: The dialog's main message text.
  * `text`: The dialog's initial textbox text.
  * `text_from_file`: The filename whose contents are loaded into the
    textbox. This option has no effect when `text` is given.
  * `button1`: The right-most button's label. The default value is
    `_L['_OK']`.
  * `button2`: The middle button's label.
  * `button3`: The left-most button's label. This option requires `button2`
    to be set.
  * `editable`: Allows the user to edit the textbox's text. The default value
    is `false`.
  * `focus_textbox`: Focus the textbox instead of the buttons. The default
    value is `false`.
  * `scroll_to`: Where to scroll the textbox's text.
    The available values are `"top"` and `"bottom"`. The default value is
    `"top"`.
  * `selected`: Select all of the textbox's text. The default value is
    `false`.
  * `monospaced_font`: Use a monospaced font in the textbox instead of a
    proportional one. The default value is `false`.
  * `string_output`: Return the selected button's label (instead of its
    index) or the dialog's exit status instead of the button's index (instead
    of its exit code). The default value is `false`.
  * `width`: The dialog's pixel width.
  * `height`: The dialog's pixel height.
  * `float`: Show the dialog on top of all desktop windows. The default value
    is `false`.
  * `timeout`: The integer number of seconds the dialog waits for the user to
    select a button before timing out. Dialogs do not time out by default.

Usage:

* `ui.dialogs.textbox{title = 'License Agreement',
  informative_text = 'You agree to:', text_from_file = _HOME..'/LICENSE'}`

Return:

* selected button or exit code, textbox text

<a id="ui.dialogs.yesno_msgbox"></a>
### `ui.dialogs.yesno_msgbox`(*options*)

Prompts the user with a generic message box dialog defined by dialog options
table *options* and with localized "Yes", "No", and "Cancel" buttons,
returning the selected button's index.
If *options*.`string_output` is `true`, returns the selected button's label.
If the dialog timed out, returns `0` or `"timeout"`. If the user canceled the
dialog, returns `-1` or `"delete"`.

Parameters:

* *`options`*: Table of key-value option pairs for the message box.

  * `title`: The dialog's title text.
  * `text`: The dialog's main message text.
  * `informative_text`: The dialog's extra informative text.
  * `icon`: The dialog's GTK stock icon name. Examples are
    "gtk-dialog-error", "gtk-dialog-info", "gtk-dialog-question", and
    "gtk-dialog-warning". The dialog does not display an icon by default.
  * `icon_file`: The dialog's icon file path. This option has no effect when
    `icon` is set.
  * `no_cancel`: Do not display the "Cancel" button. The default value is
    `false`.
  * `string_output`: Return the selected button's label (instead of its
    index) or the dialog's exit status instead of the button's index (instead
    of its exit code). The default value is `false`.
  * `width`: The dialog's pixel width.
  * `height`: The dialog's pixel height.
  * `float`: Show the dialog on top of all desktop windows. The default value
    is `false`.
  * `timeout`: The integer number of seconds the dialog waits for the user to
    select a button before timing out. Dialogs do not time out by default.

Return:

* selected button or exit code


- - -

<a id="ui.find"></a>
# The `ui.find` Module

- - -

Textadept's Find & Replace pane.

## Fields defined by `ui.find`

<a id="ui.find.INDIC_FIND"></a>
### `ui.find.INDIC_FIND` (number)

The find in files highlight indicator number.

<a id="events.FIND_WRAPPED"></a>
### `events.FIND_WRAPPED` (string)

Emitted when a text search wraps (passes through the beginning of the
  buffer), either from bottom to top (when searching for a next occurrence),
  or from top to bottom (when searching for a previous occurrence).
  This is useful for implementing a more visual or audible notice when a
  search wraps in addition to the statusbar message.

<a id="ui.find.find_entry_text"></a>
### `ui.find.find_entry_text` (string)

The text in the "Find" entry.

<a id="ui.find.find_in_files_timeout"></a>
### `ui.find.find_in_files_timeout` (number)

The approximate interval in seconds between prompts for continuing an
  "In files" search.
  The default value is 10 seconds.

<a id="ui.find.find_label_text"></a>
### `ui.find.find_label_text` (string, Write-only)

The text of the "Find" label.
  This is primarily used for localization.

<a id="ui.find.find_next_button_text"></a>
### `ui.find.find_next_button_text` (string, Write-only)

The text of the "Find Next" button.
  This is primarily used for localization.

<a id="ui.find.find_prev_button_text"></a>
### `ui.find.find_prev_button_text` (string, Write-only)

The text of the "Find Prev" button.
  This is primarily used for localization.

<a id="ui.find.in_files"></a>
### `ui.find.in_files` (bool)

Find search text in a list of files.
  The default value is `false`.

<a id="ui.find.in_files_label_text"></a>
### `ui.find.in_files_label_text` (string, Write-only)

The text of the "In files" label.
  This is primarily used for localization.

<a id="ui.find.match_case"></a>
### `ui.find.match_case` (bool)

Match search text case sensitively.
  The default value is `false`.

<a id="ui.find.match_case_label_text"></a>
### `ui.find.match_case_label_text` (string, Write-only)

The text of the "Match case" label.
  This is primarily used for localization.

<a id="ui.find.regex"></a>
### `ui.find.regex` (bool)

Interpret search text as a Regular Expression.
  The default value is `false`.

<a id="ui.find.regex_label_text"></a>
### `ui.find.regex_label_text` (string, Write-only)

The text of the "Regex" label.
  This is primarily used for localization.

<a id="ui.find.replace_all_button_text"></a>
### `ui.find.replace_all_button_text` (string, Write-only)

The text of the "Replace All" button.
  This is primarily used for localization.

<a id="ui.find.replace_button_text"></a>
### `ui.find.replace_button_text` (string, Write-only)

The text of the "Replace" button.
  This is primarily used for localization.

<a id="ui.find.replace_entry_text"></a>
### `ui.find.replace_entry_text` (string)

The text in the "Replace" entry.

<a id="ui.find.replace_label_text"></a>
### `ui.find.replace_label_text` (string, Write-only)

The text of the "Replace" label.
  This is primarily used for localization.

<a id="ui.find.whole_word"></a>
### `ui.find.whole_word` (bool)

Match search text only when it is surrounded by non-word characters in
  searches.
  The default value is `false`.

<a id="ui.find.whole_word_label_text"></a>
### `ui.find.whole_word_label_text` (string, Write-only)

The text of the "Whole word" label.
  This is primarily used for localization.


## Functions defined by `ui.find`

<a id="ui.find.find_in_files"></a>
### `ui.find.find_in_files`(*dir, filter*)

Searches directory *dir* or the user-specified directory for files that match
search text and search options (subject to optional filter *filter*), and
prints the results to a buffer titled "Files Found", highlighting found text.
Use the `find_text`, `match_case`, `whole_word`, and `regex` fields to set
the search text and option flags, respectively.

Parameters:

* *`dir`*: Optional directory path to search. If `nil`, the user is prompted
  for one.
* *`filter`*: Optional filter for files and directories to exclude. The
  default value is `ui.find.find_in_files_filter`.

See also:

* [`ui.find.find_in_files_filter`](#ui.find.find_in_files_filter)

<a id="ui.find.find_incremental"></a>
### `ui.find.find_incremental`(*text, next, anchor*)

Begins an incremental search using the command entry if *text* is `nil`.
Otherwise, continues an incremental search by searching for the next or
previous instance of string *text*, depending on boolean *next*.
*anchor* indicates whether or not to search for *text* starting from the
caret position instead of the position where the incremental search began.
Only the `match_case` find option is recognized. Normal command entry
functionality is unavailable until the search is finished or by pressing
`Esc` (`⎋` on Mac OSX | `Esc` in curses).

Parameters:

* *`text`*: The text to incrementally search for, or `nil` to begin an
  incremental search.
* *`next`*: Flag indicating whether or not the search direction is forward.
* *`anchor`*: Optional flag indicating whether or not to start searching from
  the caret position. The default value is `false`.

<a id="ui.find.find_next"></a>
### `ui.find.find_next`()

Mimics pressing the "Find Next" button.

<a id="ui.find.find_prev"></a>
### `ui.find.find_prev`()

Mimics pressing the "Find Prev" button.

<a id="ui.find.focus"></a>
### `ui.find.focus`()

Displays and focuses the Find & Replace Pane.

<a id="ui.find.goto_file_found"></a>
### `ui.find.goto_file_found`(*line\_num, next*)

Jumps to the source of the find in files search result on line number
*line_num* in the buffer titled "Files Found" or, if *line_num* is `nil`,
jumps to the next or previous search result, depending on boolean *next*.

Parameters:

* *`line_num`*: The line number in the files found buffer that contains the
  search result to go to.
* *`next`*: Optional flag indicating whether to go to the next search result
  or the previous one. Only applicable when *line_num* is `nil` or `false`.

<a id="ui.find.replace"></a>
### `ui.find.replace`()

Mimics pressing the "Replace" button.

<a id="ui.find.replace_all"></a>
### `ui.find.replace_all`()

Mimics pressing the "Replace All" button.


## Tables defined by `ui.find`

<a id="ui.find.find_in_files_filter"></a>
### `ui.find.find_in_files_filter`

The table of Lua patterns matching files and directories to exclude when
finding in files.
The filter table contains:

  + Lua patterns that match filenames to exclude.
  + Optional `folders` sub-table that contains patterns matching directories
    to exclude.
  + Optional `extensions` sub-table that contains raw file extensions to
    exclude.
  + Optional `symlink` flag that when `true`, excludes symlinked files (but
    not symlinked directories).
  + Optional `folders.symlink` flag that when `true`, excludes symlinked
    directories.

Any patterns starting with '!' exclude files and directories that do not
match the pattern that follows.
The default value is `lfs.default_filter`, a filter for common binary file
extensions and version control directories.

See also:

* [`ui.find.find_in_files`](#ui.find.find_in_files)
* [`lfs.default_filter`](#lfs.default_filter)

- - -

<a id="view"></a>
# The `view` Module

- - -

A Textadept view object.

## Fields defined by `view`

<a id="view.size"></a>
### `view.size` (number)

The split resizer's pixel position if the view is a split one.


## Functions defined by `view`

<a id="view.goto_buffer"></a>
### `view.goto_buffer`(*view, buffer*)

Switches to buffer *buffer* or the buffer *buffer* number of buffers relative
to the current one.
Emits `BUFFER_BEFORE_SWITCH` and `BUFFER_AFTER_SWITCH` events.

Parameters:

* *`view`*: The view to switch buffers in.
* *`buffer`*: A buffer or relative buffer number (typically 1 or -1).

See also:

* [`_BUFFERS`](#_BUFFERS)
* [`events.BUFFER_BEFORE_SWITCH`](#events.BUFFER_BEFORE_SWITCH)
* [`events.BUFFER_AFTER_SWITCH`](#events.BUFFER_AFTER_SWITCH)

<a id="view.split"></a>
### `view.split`(*view, vertical*)

Splits the view into top and bottom views (unless *vertical* is `true`),
focuses the new view, and returns both the old and new views.
If *vertical* is `false`, splits the view vertically into left and
right views.
Emits a `VIEW_NEW` event.

Parameters:

* *`view`*: The view to split.
* *`vertical`*: Optional flag indicating whether or not to split the view
  vertically. The default value is `false`, for horizontal.

Return:

* old view and new view.

See also:

* [`events.VIEW_NEW`](#events.VIEW_NEW)

<a id="view.unsplit"></a>
### `view.unsplit`(*view*)

Unsplits the view if possible, returning `true` on success.

Parameters:

* *`view`*: The view to unsplit.

Return:

* boolean if the view was unsplit or not.


## Tables defined by `view`

<a id="view.buffer"></a>
### `view.buffer`

The [buffer](#buffer) the view contains. (Read-only)

- - -

