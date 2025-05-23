-- Derived from https://github.com/pzim-devdata/mpv-scripts

function select_forced_subtitle()
	local last_sub = nil
	for i, sub in ipairs(mp.get_property_native("track-list")) do
		-- if sub.type == "sub" then
		-- print()
		-- print(sub.lang)
		-- print(sub.lang:find("fr"))
		-- print(sub.type)
		-- print(sub.title)
		-- print(sub.forced)
		-- end
		
		-- if sub.type == "sub" and not sub.forced and (sub.title == nil or not sub.title:find("SDH") and not string.lower(sub.title):find("force")) then
		if sub.type == "sub" and sub.lang:find("fr") and (sub.forced or (sub.title ~= nil and sub.title:find("SDH"))) then
			last_sub = sub
		end
	end
	if last_sub then
		mp.set_property("sid", last_sub.id)
	end
end
mp.register_event("file-loaded", select_forced_subtitle)
