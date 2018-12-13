local settings = require "settings"
local modes = require "modes"
local window = require "window"
require "lousy.widget.tablist"
-- local binds = require "binds"

-- ~/.local/share/luakit/adblock

-- Bindings
-- -------------------------------------------------

-- Copy text with Control-C
modes.add_binds("normal", {
    { "<Control-c>", "Copy selected text.", function ()
        luakit.selection.clipboard = luakit.selection.primary
    end},
})


-- Search engines
-- -------------------------------------------------

local search_engines = settings.window.search_engines
search_engines.duckduckgo     = "https://duckduckgo.com/?q=%s"
search_engines.github         = "https://github.com/search?q=%s"
search_engines.google         = "https://google.com/search?q=%s"

search_engines.default = search_engines.duckduckgo

-- settings.window.search_engines = {
--     gh = "https://github.com/search?q=%s",
--     g = "https://encrypted.google.com/search?q=%s",
--     ddg = "https://duckduckgo.com/?q=%s",
--     default = "https://encrypted.google.com/search?q=%s",
-- }

-- Config
-- -------------------------------------------------

-- Home page
settings.window.home_page = "https://duckduckgo.com/?kae=d&kaq=-1&kp=-2&kaj=m&kam=osm&kak=-1&kax=-1&kap=-1&kao=1&kau=-1&kt=p&kw=s&kg=g"
-- New tab (works only because newtab_chrome is not require in rc.lua)
-- settings.window.new_tab_page = settings.window.home_page
settings.override_setting("window.new_tab_page", settings.window.home_page)
-- Whether the tab list should be visible with only a single tab open
settings.tablist.always_visible = true
-- Whether the Java plugin is enabled
settings.webview.enable_java = false

settings.window.scroll_step = 100
-- settings.window.zoom_step = 0.2
settings.window.new_window_size = "1366x768"
settings.webview.enable_mediasource = true
settings.webview.enable_developer_extras = true
settings.window.max_title_len = 200
-- settings.window.close_with_last_tab = true
settings.webview.enable_webgl = true
settings.webview.enable_dns_prefetching = true
settings.webview.enable_webaudio = true
-- settings.webview.enable_javascript                   = true
-- settings.on["youtube.com"].webview.enable_javascript  = true

-- webview.add_signal("init", function (view)
--     view:add_signal("navigation-request", function (_, uri)
--         if uri == "about:blank" then
--             local html = "<html><body bgcolor='#000000'></body></html>"
--             view:load_string(html, "about:blank")
--             return true
--         end
--     end)
-- end)
