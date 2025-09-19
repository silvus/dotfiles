-- replay_with_subtitles.lua
-- - If subtitles are off: enable + rewind 20
-- - If subtitles are on: disable
-- Shows notification of state

local mp = require 'mp'

local last_press_time = 0
local threshold = 0.4  -- seconds allowed between presses

function toggle_subs()
    local sub_visible = mp.get_property_native("sub-visibility")

    if sub_visible then
        -- Subtitles currently ON: disable
        mp.set_property("sub-visibility", "no")
        mp.osd_message("Subtitles OFF", 2)
    else
        -- Subtitles currently OFF: enable + rewind 20
        mp.set_property("sub-visibility", "yes")
        mp.command("seek -20 relative")
        mp.osd_message("Subtitles ON (rewind -20)", 2)
    end
end

function on_space()
    local now = mp.get_time()
    if last_press_time > 0 and (now - last_press_time) <= threshold then
        -- Double press detected
        toggle_subs()
        last_press_time = 0
    else
        -- First press
        last_press_time = now
    end
end

-- Override default Space behaviour
function space_override()
    mp.command("cycle pause")
    on_space()
end

mp.add_forced_key_binding("SPACE", "double-space-subs", space_override)
