-- pages are made up of bits (ui elements)

-- bit function converts everything to a unified
-- string format so as to not have to deal with casting
function textBit (mx,my,text,color,name)
    -- TODO: sanitize input
    return ("text:"..text..":"..(tostring(mx))..":"..(tostring(my))..":"..color..":"..name)
end
function linkButton (mx,my,text,color,name)
    return ("butt:"..text..":"..(tostring(mx))..":"..(tostring(my))..":"..color..":".."link:"..name)
end

return bit
