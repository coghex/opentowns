-- pages are made up of bits (ui elements)

-- bit function converts everything to a unified
-- string format so as to not have to deal with casting

-- text is simple text, no functionality
function text (mx,my,text,color)
    -- TODO: sanitize input
    return ("text:"..text..":"..(tostring(mx))..":"..(tostring(my))..":"..color)
end
-- links follow on to pages
function linkButton (mx,my,text,color,name)
    return ("butt:"..text..":"..(tostring(mx))..":"..(tostring(my))..":"..color..":".."link:"..name)
end
-- special buttons handled in haskell for easier lua

-- back goes to the last page
function backButton (mx,my)
    return ("back:"..(tostring(mx))..":"..(tostring(my)))
end
-- exit exits everything
function exitButton (mx,my)
    return ("exit:"..(tostring(mx))..":"..(tostring(my)))
end
-- calling raw haskell functions
function funcButton (mx,my,text,color,func)
    return ("func:"..(tostring(mx))..":"..(tostring(my))..":"..text..":"..color..":"..func)
end
-- text buttons that allow output to label the button
-- and are really only intended for this specific game
function textButton (mx,my,text,color,args)
    return ("tBut:"..(tostring(mx))..":"..(tostring(my))..":"..text..":"..color..":"..args)
end

return bit
