---@class (exact) A
---@field param number
local A = setmetatable({}, {
    __call = function(self)
        local i = setmetatable({}, {__index = self})
        i:init()
        return i
    end
})

---@param param number Comment
function A:init(param)
    self.param = param
end

--A is now complaining about the missing param
local a = A()
assert(a)