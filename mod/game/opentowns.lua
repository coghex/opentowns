-- defines the basic game
require "mod/base/game"
require "mod/game/opentowns/menu"
-- this runs once at the beginning
function initMod ()
    -- basic UI elements
    initMenu ()
    return 0
end
function runMod ()
end
