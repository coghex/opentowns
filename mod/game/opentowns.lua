-- defines the basic game
require "mod/base/window"
require "mod/base/page"
require "mod/base/bit"
require "mod/base/game"
-- this runs once at the beginning
function initMod ()
    -- basic UI elements
    local win1 = window:new ()
    win1:initWindow("win1")
    local menu1 = page:new ()
    menu1:newBit(linkButton (1.0,1.0,"Tutorial", "0xFFFF00","menu2"))
    menu1:newBit(linkButton (1.0,1.5,"New game", "0xFFFF00","menu3"))
    menu1:newBit(linkButton (1.0,2.0,"Mods",     "0xFFFF00","menu4"))
    menu1:newBit(linkButton (1.0,2.5,"Servers",  "0xFFFF00","menu5"))
    menu1:newBit(linkButton (1.0,3.0,"Options",  "0xFFFF00","menu6"))
    menu1:newBit(linkButton (1.0,3.5,"Credits",  "0xFFFF00","menu7"))
    menu1:newBit(exitButton (1.0,4.5))
    menu1:initPage("menu1")
    win1:addPage(menu1)
    local menu2 = page:new ()
    menu2:newBit(text       (1.0,1.0,"Tutorial","0xFFFF00"))
    menu2:newBit(linkButton (1.0,2.0,"The basics","0xFFFF00","tut1"))
    menu2:newBit(linkButton (1.0,2.5,"Food and auto-production","0xFFFF00","tut2"))
    menu2:newBit(linkButton (1.0,3.0,"Population management","0xFFFFFF","tut3"))
    menu2:newBit(linkButton (1.0,3.5,"Soldiers and heroes","0xFFFFFF","tut4"))
    menu2:newBit(backButton (1.0,4.5))

    menu2:initPage("menu2")
    win1:addPage(menu2)
    local menu3 = page:new ()
    menu3:newBit(linkButton (1.0,2.5,"third menu","0xFFFF00","menu1"))
    menu3:initPage("menu3")
    win1:addPage(menu3)


    win1:goToPage("menu1")
    return 0
end
function runMod ()
end
