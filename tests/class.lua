---@class (exact) A
local a = {}

---@param a number Comment
---@return number
function a:func(a)
    return a
end

---@class (exact) B : A
local b = {}

---@override
function b:func(a)
    return #a
end

--Func still has the type and comment from its super, causing a type mismatch here
b:func("string!")