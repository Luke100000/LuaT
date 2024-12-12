---@class (exact) A
---@field param number
local A = setmetatable({}, {
    __call = function(self)
        local i = setmetatable({}, {__index = self})
        i:init()
        return i
    end
})

function A:init(param: number)
    self.param = param
end

--A is now complaining about the missing param
local a = A()
assert(a)