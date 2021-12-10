-- base mod defines some basic gameplay
-- this data type represents a classlike
-- interface to the haskell objects
require "mod/base/page"
window = {}

function window:new (o)
    o = o or {}
    self.__index = self
    setmetatable (o, self)
    self.lwName = "NULL"
    return o
end

-- windows are identified by strings
function window:initWindow (n)
    self.lwName = n
    rawNewWindow (n)
end

function window:goToPage (n)
    rawGoToPage (self.lwName..":"..n)
end

function window:addPage (page)
    rawNewPage (self.lwName,page.pName)
    for i,b in pairs(page.pBits) do
        rawNewElem (self.lwName, page.pName,b)
    end
end

return window
