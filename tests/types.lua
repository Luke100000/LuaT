--Test inline types

local variable: string = ""

--Hmm, this causes VSCode to crash
local first: string, second: string = "", ""

local object = {}

object.injectedVariable: number = 42


---@param param2 string This is fancy
function Func(param1: string, param2) : number, number
    return 1, 2
end

local function func(param1: string, param2: number) : number, number
    return 1, 2
end

Func2 = function(param1: string, param2: number)

end

local func2 = function(param1: string, param2: number)

end

local function multiline(
    param1: string,
    param2: number
) : string
    return ""
end