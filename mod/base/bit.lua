-- pages are made up of bits (ui elements)

-- bit function converts everything to a unified
-- string format so as to not have to deal with casting
function text (mx,my,text,color)
    -- TODO: sanitize input
    return ("text:"..text..":"..(tostring(mx))..":"..(tostring(my))..":"..color)
end
function linkButton (mx,my,text,color,name)
    return ("butt:"..text..":"..(tostring(mx))..":"..(tostring(my))..":"..color..":".."link:"..name)
end
-- special buttons handled in haskell for easier lua
function backButton (mx,my)
    return ("back:"..(tostring(mx))..":"..(tostring(my)))
end
function exitButton (mx,my)
    return ("exit:"..(tostring(mx))..":"..(tostring(my)))
end

return bit
