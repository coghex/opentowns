-- defines the basic game
require "mod/base/window"
require "mod/base/page"
require "mod/base/bit"
require "mod/base/game"
-- this runs once at the beginning
function initMod ()
    -- basic UI elements
    -- win1 is the collection of menus
    local win1 = window:new ()
    win1:initWindow("win1")
    -- win2 is the game window
    local win2 = window:new ()
    win2:initWindow("win2")
    -- menu1 is the start menu
    local menu1 = page:new ()
    -- links go to new pages
    menu1:newBit(linkButton (1.0,1.0,"Tutorial", "0xFFFF00","menu2"))
    menu1:newBit(linkButton (1.0,1.5,"New game", "0xFFFF00","menu3"))
    menu1:newBit(linkButton (1.0,2.0,"Mods",     "0xFFFF00","menu4"))
    menu1:newBit(linkButton (1.0,2.5,"Servers",  "0xFFFF00","menu5"))
    menu1:newBit(linkButton (1.0,3.0,"Options",  "0xFFFF00","menu6"))
    menu1:newBit(linkButton (1.0,3.5,"Credits",  "0xFFFF00","menu7"))
    -- exit quits everything
    menu1:newBit(exitButton (1.0,4.5))
    menu1:initPage("menu1")
    win1:addPage(menu1)

    local menu2 = page:new ()
    -- text is just text
    menu2:newBit(text       (1.0,1.0,"Tutorial","0xFFFFFF"))
    menu2:newBit(linkButton (1.0,2.0,"The basics","0xFFFFFF","tut1"))
    menu2:newBit(linkButton (1.0,2.5,"Food and auto-production","0xFFFFFF","tut2"))
    menu2:newBit(linkButton (1.0,3.0,"Population management","0xFFFFFF","tut3"))
    menu2:newBit(linkButton (1.0,3.5,"Soldiers and heroes","0xFFFFFF","tut4"))
    menu2:newBit(backButton (1.0,4.5))
    menu2:initPage("menu2")
    win1:addPage(menu2)

    local menu3 = page:new ()
    menu3:newBit(text       (1.0,1.0,"New Game","0xFFFFFF"))
    -- loads switch to a new window
    menu3:newBit(loadButton (1.0,2.0,"Normal Map","0xFFFF00","win2","normalmap"))
    menu3:newBit(loadButton (1.0,2.5,"Desert","0xFFFF00","win2","desertmap"))
    menu3:newBit(loadButton (1.0,3.0,"Jungle","0xFFFF00","win2","junglemap"))
    menu3:newBit(loadButton (1.0,3.5,"Mixed","0xFFFF00","win2","mixedmap"))
    menu3:newBit(loadButton (1.0,4.0,"Snow","0xFFFF00","win2","snowmap"))
    menu3:newBit(loadButton (1.0,4.5,"Mountains","0xFFFF00","win2","mountainsmap"))
    menu3:newBit(backButton (1.0,5.5))
    menu3:initPage("menu3")
    win1:addPage(menu3)

    local menu4 = page:new ()
    menu4:newBit(text       (1.0,1.0,"Mods","0xFFFFFF"))
    menu4:newBit(text       (1.0,2.0,"Mods folder [path]","0xFFFFFF"))
    menu4:newBit(text       (1.0,2.5,"There are no mods on the mods folder (sic)","0xFFFFFF"))
    menu4:newBit(backButton (1.0,3.0))
    menu4:initPage("menu4")
    win1:addPage(menu4)

    local menu5 = page:new ()
    menu5:newBit(text       (1.0,1.0,"Servers","0xFFFFFF"))
    menu5:newBit(text       (1.0,2.0,"Unknown server name []","0xFFFF00"))
    menu5:newBit(text       (1.0,2.5,"Remove server []","0xFFFFFF"))
    menu5:newBit(text       (1.0,3.0,"Add new Server","0xFFFFFF"))
    menu5:newBit(backButton (1.0,4.0))
    menu5:initPage("menu5")
    win1:addPage(menu5)

    local menu6 = page:new ()
    menu6:newBit(text       (1.0,1.0,"Options","0xFFFFFF"))
    menu6:newBit(linkButton (1.0,2.0,"Graphics","0xFFFF00","options1"))
    menu6:newBit(linkButton (1.0,2.5,"Audio","0xFFFF00","options2"))
    menu6:newBit(linkButton (1.0,3.0,"Game","0xFFFF00","options3"))
    menu6:newBit(linkButton (1.0,3.5,"Performance","0xFFFF00","options4"))
    menu6:newBit(linkButton (1.0,4.0,"Controls","0xFFFF00","options5"))
    menu6:newBit(linkButton (1.0,4.5,"Language","0xFFFF00","options6"))
    menu6:newBit(backButton (1.0,5.5))
    menu6:initPage("menu6")
    win1:addPage(menu6)

    local menu7 = page:new ()
    menu7:newBit(text       (1.0,1.0,"Credits","0xFFFFFF"))
    menu7:newBit(text       (1.0,2.0,"Code, design and composition","0xFFFF00"))
    menu7:newBit(text       (1.0,2.5,"blop blop","0xFFFFFF"))
    menu7:newBit(backButton (1.0,3.5))
    menu7:initPage("menu7")
    win1:addPage(menu7)

    local options1 = page:new ()
    options1:newBit(funcButton (1.0,1.0,"Toggle full screen","0xFFFFFF","toggleFullScreen"))
    options1:newBit(backButton (1.0,2.0))
    options1:initPage("options1")
    win1:addPage(options1)

    local options2 = page:new ()
    options2:newBit(text       (1.0,1.0,"Audio","0xFFFFFF"))
    options2:newBit(textButton (1.0,2.0,"Music","0xFFFFFF","ON"))
    options2:newBit(textButton (1.0,2.5,"Music-Volume","0xFFFFFF","100%"))
    options2:newBit(textButton (1.0,3.0,"FX","0xFFFFFF","ON"))
    options2:newBit(textButton (1.0,3.5,"FX-Volume","0xFFFFFF","100%"))
    options2:newBit(backButton (1.0,5.0))
    options2:initPage("options2")
    win1:addPage(options2)

    local options3 = page:new ()
    options3:newBit(text       (1.0,1.0,"Game","0xFFFFFF"))
    options3:newBit(textButton (1.0,2.0,"Mouse scroll","0xFFFFFF","ON"))
    options3:newBit(textButton (1.0,2.5
      ,"Allow mouse scroll while hovering the edge buttons","0xFFFFFF","OFF"))
    options3:newBit(textButton (1.0,3.0
      ,"Height cubes when 2D mouse is enabled","0xFFFFFF","ON"))
    options3:newBit(textButton (1.0,3.5
      ,"Newly built stockpiles/containers have all items disabled by default","0xFFFFFF","OFF"))
    options3:newBit(textButton (1.0,4.0
      ,"Pause the game when it starts","0xFFFFFF","OFF"))
    options3:newBit(textButton (1.0,4.5
      ,"Autosave","0xFFFFFF","Disabled"))
    options3:newBit(textButton (1.0,5.0
      ,"Sieges","0xFFFFFF","Normal"))
    options3:newBit(textButton (1.0,5.5
      ,"Pause the game when a siege starts","0xFFFFFF","OFF"))
    options3:newBit(textButton (1.0,6.0
      ,"Pause the game when a caravan comes","0xFFFFFF","OFF"))
    options3:newBit(textButton (1.0,6.5
      ,"Allow bury system","0xFFFFFF","ON"))
    options3:newBit(backButton (1.0,7.5))
    options3:initPage("options3")
    win1:addPage(options3)

    local options4 = page:new ()
    options4:newBit(text       (1.0,1.0,"Performance","0xFFFFFF"))
    options4:newBit(text       (1.0,2.0,"This option set the CPU usage for pathfinding (Default value 2)","0xAAAAAA"))
    options4:newBit(text       (1.0,2.5,"Set it to 1 if you have a lower end computer","0xAAAAAA"))
    options4:newBit(text       (1.0,3.0,"Set it to 6 if you have a strong computer (Warning It can produce lag)","0xAAAAAA"))
    options4:newBit(textButton (1.0,3.5 ,"CPU level usage for pathfinding","0xFFFFFF","2"))
    options4:newBit(backButton (1.0,4.5))
    options4:initPage("options4")
    win1:addPage(options4)

    local options5 = page:new ()
    options5:newBit(text       (1.0,1.0,"Controls","0xFFFFFF"))
    options5:newBit(text       (1.0,2.0,"Movement","0xFFFF00"))
    options5:newBit(keysButton (1.0,2.5,"Scroll up","0xFFFFFF","W,Up"))
    options5:newBit(keysButton (1.0,3.0,"Scroll down","0xFFFFFF","S,Down"))
    options5:newBit(keysButton (1.0,3.5,"Scroll left","0xFFFFFF","A,Left"))
    options5:newBit(keysButton (1.0,4.0,"Scroll right","0xFFFFFF","D,Right"))
    options5:newBit(keysButton (1.0,4.5,"Level up","0xFFFFFF",">"))
    options5:newBit(keysButton (1.0,5.0,"Level down","0xFFFFFF","<"))
    options5:newBit(text       (1.0,6.0,"Panels","0xFFFF00"))
    options5:initPage("options5")
    win1:addPage(options5)

    local options6 = page:new ()
    options6:newBit(text       (1.0,1.0,"Language","0xFFFFFF"))
    options6:newBit(funcButton (1.0,2.0,"English","0xFFFFFF","english"))
    options6:newBit(funcButton (1.0,2.5,"Espanol","0xFFFFFF","espanol"))
    options6:initPage("options6")
    win1:addPage(options6)

    local normalmap = page:new ()
    normalmap:newBit(text       (1.0,1.0,"Normal Map","0xFFFFFF"))
    normalmap:newBit(worldMap ("normalmap"))
    normalmap:newBit(funcButton (1.0,1.5,"Do not use buried towns","0xFFFFFF", "genMapNormalNoBuried"))
    normalmap:newBit(funcButton (1.0,2.0,"Use local buried towns","0xFFFFFF", "genMapNormalLocalBuried"))
    normalmap:newBit(funcButton (1.0,2.5,"Load a buried town from...","0xFFFFFF", "genMapNormalLoadBuried"))
    normalmap:newBit(backButton (1.0,3.0))
    normalmap:initPage("normalmap")
    win2:addPage(normalmap)

    win1:goToPage("menu1")
    return 0
end
function runMod ()
end
