"""Per-project tmux window setup, run by tmuxdev --pick right after it
creates a new session/window for a project (not on every re-select).

Add an entry to PROJECTS to wire up a project name. Each setup function
receives:
  target -- tmux target for the new window, e.g. "main:myproject" (window
            mode) or "myproject:" (session mode)
  path   -- the project's working directory

This file (custom/tmuxdev.py) applies to every project. For setup specific
to one project, drop a same-shaped `.tmuxdev` file (plain Python despite the
extensionless name, like the old `.tmux-init`) in that project's own
directory instead -- both are loaded and run the same way. Example
/data/dev/my-project/.tmuxdev:

    import subprocess

    def tmux(*args):
        subprocess.run(["tmux", *args], check=False)

    def setup_my_project(target, path):
        tmux("split-window", "-t", target, "-h", "-c", path, "$EDITOR ; exec fish")

    PROJECTS = {
        "my_project": setup_my_project,
    }
"""

import subprocess


def tmux(*args):
    subprocess.run(["tmux", *args], check=False)


def setup_myproject(target, path):
    tmux("split-window", "-t", target, "-h", "-c", path, "$EDITOR ; exec fish")
    tmux("select-layout", "-t", target, "even-horizontal")


PROJECTS = {
    "myproject": setup_myproject,
}
