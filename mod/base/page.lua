-- pages represent different screens to be displayed
page = {}

function page:new (o)
    o = o or {}
    self.__index = self
    setmetatable (o,self)
    self.pName = "NULL"
    self.pBits = {}
    return o
end

function page:initPage (n)
    self.pName = n
end

function page:newBit (bit)
   table.insert (self.pBits,bit)
end

return page
