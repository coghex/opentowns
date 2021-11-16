-- defines the basic game

-- this runs once at the beginning
local function initMod ()
    -- basic UI elements
    local win1 = window:new ()
    win1:initWindow("win1")
    local menu1 = page:new ()
    menu1:newBit(textBit (1.0,1.0,"Tutorial","0xFFFF00","menu2"))
    menu1:initPage("menu1")
    win1:addPage(menu1)
    local menu2 = page:new ()
    menu2:newBit(textBit (1.0,1.5,"New game","0xFFFF00","menu3"))
    menu2:initPage("menu2")
    win1:addPage(menu2)

    win1:goToPage("menu1")
    return 0
end
local function runMod ()
end
