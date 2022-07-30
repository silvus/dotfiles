# Test this configuration without reload with :
# python3 -m py_compile ~/.config/qtile/config.py


from typing import List, Callable
from libqtile import bar, layout, widget
from libqtile.config import Click, Drag, Group, Key, Match, Screen
from libqtile.lazy import lazy
from libqtile.utils import guess_terminal
from libqtile import hook, extension
from libqtile.core.manager import Qtile

mod = "mod4"
terminal = guess_terminal()

keys = [
    # Switch between windows
    Key([mod], "Left", lazy.layout.left(), desc="Move focus to left"),
    Key([mod], "Right", lazy.layout.right(), desc="Move focus to right"),
    Key([mod], "Down", lazy.layout.down(), desc="Move focus down"),
    Key([mod], "Up", lazy.layout.up(), desc="Move focus up"),
    Key([mod], "Tab", lazy.layout.next(), desc="Move window focus to other window"),
    # Move windows between left/right columns or move up/down in current stack.
    # Moving out of range in Columns layout will create new column.
    Key(
        [mod, "shift"],
        "Left",
        lazy.layout.shuffle_left(),
        desc="Move window to the left",
    ),
    Key(
        [mod, "shift"],
        "Right",
        lazy.layout.shuffle_right(),
        desc="Move window to the right",
    ),
    Key([mod, "shift"], "Down", lazy.layout.shuffle_down(), desc="Move window down"),
    Key([mod, "shift"], "Up", lazy.layout.shuffle_up(), desc="Move window up"),
    # Grow windows. If current window is on the edge of screen and direction
    # will be to screen edge - window would shrink.
    Key(
        [mod, "control"],
        "Left",
        lazy.layout.grow_left(),
        desc="Grow window to the left",
    ),
    Key(
        [mod, "control"],
        "Right",
        lazy.layout.grow_right(),
        desc="Grow window to the right",
    ),
    Key([mod, "control"], "Down", lazy.layout.grow_down(), desc="Grow window down"),
    Key([mod, "control"], "Up", lazy.layout.grow_up(), desc="Grow window up"),
    Key([mod], "n", lazy.layout.normalize(), desc="Reset all window sizes"),
    # Toggle between split and unsplit sides of stack.
    # Split = all windows displayed
    # Unsplit = 1 window displayed, like Max layout, but still with
    # multiple stack panes
    Key(
        [mod, "shift"],
        "Return",
        lazy.layout.toggle_split(),
        desc="Toggle between split and unsplit sides of stack",
    ),
    Key([mod], "Return", lazy.spawn(terminal), desc="Launch terminal"),
    # Toggle between different layouts as defined below
    Key([mod], "space", lazy.window.toggle_floating(), desc="Toggle floating"),
    # Key([mod, "shift"], "space", lazy.previous_layout(), desc="Toggle between layouts"),
    Key([mod, "shift"], "q", lazy.window.kill(), desc="Kill focused window"),
    Key([mod, "control"], "r", lazy.reload_config(), desc="Reload the config"),
    Key([mod, "control"], "q", lazy.shutdown(), desc="Shutdown Qtile"),
    Key([mod], "e", lazy.spawncmd(), desc="Spawn a command using a prompt widget"),
    Key([mod], "r", lazy.spawncmd(), desc="Spawn a command using a prompt widget"),
    # Sound
    Key([], "XF86AudioMute", lazy.spawn("amixer -q set Master toggle")),
    Key([], "XF86AudioLowerVolume", lazy.spawn("amixer -c 0 sset Master 1- unmute")),
    Key([], "XF86AudioRaiseVolume", lazy.spawn("amixer -c 0 sset Master 1+ unmute")),
    Key(
        [mod],
        "m",
        lazy.run_extension(
            extension.CommandSet(
                commands={
                    "play/pause": "[ $(mocp -i | wc -l) -lt 2 ] && mocp -p || mocp -G",
                    "next": "mocp -f",
                    "previous": "mocp -r",
                    "quit": "mocp -x",
                    "open": "urxvt -e mocp",
                    "shuffle": "mocp -t shuffle",
                    "repeat": "mocp -t repeat",
                },
                pre_commands=["[ $(mocp -i | wc -l) -lt 1 ] && mocp -S"],
                # **Theme.dmenu
            )
        ),
    ),
]

# QWERTY keyboards
# group_names = [
#     "1",
#     "2",
#     "3",
#     "4",
#     "5",
#     "6",
#     "7",
#     "8",
#     "9",
#     "0",
# ]

# AZERTY keyboards
# group_names = [
#     "ampersand",
#     "eacute",
#     "quotedbl",
#     "apostrophe",
#     "parenleft",
#     "section",
#     "egrave",
#     "exclam",
#     "ccedilla",
#     "agrave",
# ]
# group_labels = [
#     "1",
#     "2",
#     "3",
#     "4",
#     "5",
#     "6",
#     "7",
#     "8",
#     "9",
#     "0",
# ]
# group_layouts = [
#     "monadtall",
#     "treetab",
#     "treetab",
#     "treetab",
#     "treetab",
#     "treetab",
#     "treetab",
#     "treetab",
#     "treetab",
#     "treetab",
# ]

# # groups = [Group(i) for i in "123456789"]
# groups = []
# for i in range(len(group_names)):
#     groups.append(
#         Group(
#             name=group_names[i],
#             layout=group_layouts[i],
#             label=group_labels[i],
#         )
#     )

groups = [
    Group(
        name="ampersand",  # 1
        layout="max",
        label="Web",
        matches=[Match(wm_class=["Firefox"])],
        exclusive=True,
        spawn="firefox",
    ),
    Group(
        name="eacute",  # 2
        layout="max",
        label="Code",
        matches=[Match(wm_class=["VSCodium", "jetbrains-phpstorm"])],
        exclusive=True,
        spawn="codium",
    ),
    Group(
        name="quotedbl",  # 3
        layout="max",
        label="Mail",
        matches=[Match(wm_class=["Thunderbird"])],
        exclusive=True,
        spawn="thunderbird",
    ),
    Group(
        name="apostrophe",  # 4
        layout="max",
        label="File",
        matches=[Match(wm_class=["Pcmanfm", "Thunar", "Nemo"])],
        exclusive=True,
    ),
    Group(
        name="parenleft",  # 5
        layout="columns",
        label="5",
        matches=[],
    ),
    Group(
        name="minus",  # 6
        layout="columns",
        label="6",
        matches=[],
    ),
    Group(
        name="egrave",  # 7
        layout="columns",
        label="7",
        matches=[],
    ),
    Group(
        name="underscore",  # 8
        layout="columns",
        label="8",
        matches=[],
    ),
    Group(
        name="ccedilla",  # 9
        layout="columns",
        label="9",
        matches=[],
    ),
    Group(
        name="agrave",  # 0
        layout="columns",
        label="0",
        matches=[],
    ),
]


for i in groups:
    keys.extend(
        [
            # mod1 + letter of group = switch to group
            # Key(
            #     [mod],
            #     i.name,
            #     lazy.group[i.name].toscreen(),
            #     desc="Switch to group {}".format(i.name),
            # ),
            # mod1 + shift + letter of group = switch to & move focused window to group
            Key(
                [mod, "shift"],
                i.name,
                lazy.window.togroup(i.name, switch_group=True),
                desc="Switch to & move focused window to group {}".format(i.name),
            ),
            # Or, use below if you prefer not to switch to that group.
            # # mod1 + shift + letter of group = move focused window to group
            # Key([mod, "shift"], i.name, lazy.window.togroup(i.name),
            #     desc="move focused window to group {}".format(i.name)),
        ]
    )


def go_to_group(name: str) -> Callable:
    def _inner(qtile: Qtile) -> None:
        if len(qtile.screens) == 1:
            qtile.groups_map[name].cmd_toscreen()
            return

        if name in "ccedilla":
            # 9
            qtile.focus_screen(1)
            qtile.groups_map[name].cmd_toscreen()
        elif name in "underscore":
            # 8
            qtile.focus_screen(2)
            qtile.groups_map[name].cmd_toscreen()
        else:
            qtile.focus_screen(0)
            qtile.groups_map[name].cmd_toscreen()

    return _inner


for i in groups:
    keys.append(Key([mod], i.name, lazy.function(go_to_group(i.name))))


# https://docs.qtile.org/en/latest/manual/ref/layouts.html
layouts = [
    layout.Columns(border_focus_stack=["#d75f5f", "#8f3d3d"], border_width=2),
    # layout.Stack(num_stacks=2),  # How to access other apps ?
    # layout.Bsp(),  # Horizontal matrix
    # layout.Matrix(),  # 1 left, 1 right, 1 left, etc...
    layout.MonadTall(),  # Left Main, all right
    layout.Max(),
    # layout.MonadWide(),  # Left Top, all bottom
    # layout.RatioTile(),  # Horizontal boxes
    # layout.Tile(),  # Left large Main, all right
    # layout.VerticalTile(),  # Bottom large, all top
    # layout.TreeTab(),  # Left sidebar app navigation
    # layout.Zoomy(),  # Right sidebar  app navigation miniature
    # layout.Floating(),
]

widget_defaults = dict(
    font="sans",
    fontsize=11,
    padding=2,
)
extension_defaults = widget_defaults.copy()

screens = [
    Screen(
        top=bar.Bar(
            [
                widget.CurrentScreen(),
                widget.CurrentLayoutIcon(),
                widget.CurrentLayout(),
                widget.Prompt(),
                widget.GroupBox(),
                # widget.AGroupBox(),
                widget.TaskList(),
                # widget.WindowName(),
                # widget.WindowTabs(),
                # widget.Backlight(),
                # widget.BatteryIcon(),
                # widget.Battery(),
                # widget.CPU(),
                widget.WidgetBox(
                    widgets=[
                        widget.TextBox(text="This widget is in the box"),
                        widget.CPU(),
                        widget.Memory(),
                        widget.Net(),
                    ]
                ),
                # widget.Memory(),
                widget.MemoryGraph(),
                widget.NetGraph(),
                widget.CheckUpdates(),
                # widget.KeyboardKbdd(),
                # widget.KeyboardLayout(),
                # widget.KhalCalendar(),
                widget.Chord(
                    chords_colors={
                        "launch": ("#ff0000", "#ffffff"),
                    },
                    name_transform=lambda name: name.upper(),
                ),
                widget.Moc(),
                widget.Systray(),
                widget.PulseVolume(),
                widget.Volume(),
                widget.Clock(format="%H:%M"),
                widget.Wallpaper(
                    directory="~/.config/qtile/wallpapers/",
                    random_selection=True,
                    fmt="",
                ),
            ],
            22,
        ),
    ),
    Screen(
        bottom=bar.Bar(
            [
                widget.CurrentScreen(),
                widget.CurrentLayoutIcon(),
                widget.CurrentLayout(),
                groupbox2,
                widget.TaskList(),
                widget.Wallpaper(
                    directory="~/.config/qtile/wallpapers/",
                    random_selection=True,
                    fmt="",
                ),
            ],
            22,
        ),
    ),
    Screen(
        bottom=bar.Bar(
            [
                widget.CurrentScreen(),
                widget.CurrentLayoutIcon(),
                widget.CurrentLayout(),
                groupbox3,
                widget.TaskList(),
                widget.Wallpaper(
                    directory="~/.config/qtile/wallpapers/",
                    random_selection=True,
                    fmt="",
                ),
            ],
            22,
        ),
    ),
]

# Drag floating layouts.
mouse = [
    Drag(
        [mod],
        "Button1",
        lazy.window.set_position_floating(),
        start=lazy.window.get_position(),
    ),
    Drag(
        [mod], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()
    ),
    Click([mod], "Button2", lazy.window.bring_to_front()),
]

dgroups_key_binder = None
dgroups_app_rules = []  # type: List
follow_mouse_focus = False
bring_front_click = False
cursor_warp = False
floating_layout = layout.Floating(
    float_rules=[
        # Run the utility of `xprop` to see the wm class and name of an X client.
        *layout.Floating.default_float_rules,
        Match(wm_class="confirmreset"),  # gitk
        Match(wm_class="makebranch"),  # gitk
        Match(wm_class="maketag"),  # gitk
        Match(wm_class="ssh-askpass"),  # ssh-askpass
        Match(title="branchdialog"),  # gitk
        Match(title="pinentry"),  # GPG key password entry
        Match(title="Event Tester"),  # xev
    ]
)
auto_fullscreen = True
focus_on_window_activation = "smart"
reconfigure_screens = True

# If things like steam games want to auto-minimize themselves when losing
# focus, should we respect this or not?
auto_minimize = True

# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"


# @hook.subscribe.screens_reconfigured
# async def _():
#     if len(qtile.screens) > 1:
#         groupbox1.visible_groups = ["1", "2", "3"]
#     else:
#         groupbox1.visible_groups = ["1", "2", "3", "q", "w", "e"]
#     if hasattr(groupbox1, "bar"):
#         groupbox1.bar.draw()


# @hook.subscribe.startup_once
# def autostart():
#     home = os.path.expanduser("~")
#     subprocess.Popen([home + "/.config/qtile/autostart.sh"])
