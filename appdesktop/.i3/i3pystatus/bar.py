from i3pystatus import Status
from i3pystatus.updates import aptget
from i3pystatus.mail import maildir

# Colors
color_blue = "#1e90ff"
color_green = "#00cd00"
color_gray = "#cccccc"
color_red = "#ff2929"

status = Status(standalone=True)

# Note: requires libasound2-dev and pyalsaaudiaaudio from PyPI
status.register("alsa",
    format="  {volume}%",
    format_muted="♪ Mute",
    color_muted=color_gray,
    color=color_blue,
    hints={"separator": False, "min_width": 60, "align" : "center"},)

# Displays clock
status.register("clock",
    format="  %X",
    color=color_green,
    hints={"min_width": 60, "align" : "left"},)

# Displays date
status.register("clock",
    format="  %a %-d %b",
    hints={"separator": False, "min_width": 100, "align" : "right"},)

# Shows the average load of the last minute and the last 5 minutes
#status.register("load")

# Unread mail
status.register("mail",
    color=color_blue,
    color_unread=color_green,
    hide_if_null=True,
    format=" {unread}",
    format_plural=" {unread}",
    interval=60,
    hints={"min_width": 30, "align" : "center"},
    backends=[
        maildir.MaildirMail(
            directory="/data/silvus/mail/INBOX",
            # account="",
        )
    ])

# Updates
status.register("updates",
    format=" {count}",
    format_no_updates="",
    color=color_blue,
    color_no_updates=color_blue,
    hints={"min_width": 30, "align" : "center"},
    backends=[aptget.AptGet()])

# Requires psutil
status.register("mem_bar",
    color=color_blue,
    hints={"align" : "center"},)
status.register("mem",
    color=color_blue,
    format="  {percent_used_mem:.0f}%",
    hints={"separator": False, "min_width": 70, "align" : "center"},)

status.register("cpu_usage_bar",
    start_color=color_blue,
    end_color=color_blue,
    hints={"align" : "center"},)
status.register("cpu_usage",
    format="  {usage:02}%",
    hints={"separator": False, "min_width": 60, "align" : "center"},)

# Shows disk usage of /data
status.register("disk",
    path="/data",
    interval=60,
    format=" {used:.0f}/{total:.0f} G",
    hints={"min_width": 120, "align" : "center"},)

# Shows disk usage of /
status.register("disk",
    path="/",
    interval=60,
    format=" {used:.0f}/{total:.0f} G",
    hints={"min_width": 120, "align" : "center"},)

# Shows the address and up/down state of eth0.
# Note: the network module requires PyPI package netifaces
status.register("network",
    color_up=color_blue,
    start_color=color_blue,
    interface="eth0",
    dynamic_color=False,
    upper_limit=1300,
    format_up=" {v4} {network_graph}{kbs}KB/s",
    hints={"min_width": 30, "align" : "center"},)

status.register("network",
    color_up=color_blue,
    color_down=color_red,
    interface="tun0",
    format_down='',
    unknown_up=True,
    format_up=" {v4} {kbs}KB/s",
    hints={"min_width": 30, "align" : "center"},)

# status.register("now_playing",
#     player="mpv")

status.run()
