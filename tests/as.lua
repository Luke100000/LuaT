-- Test fancy "as" syntax
local function test() : boolean
    return false
end

local function p(a: any, b: string?)

end

local casted = test() as number

p(casted as string)
p(casted as string, casted as string)
p(casted as ({ complex: number, simple: string }))

for a, b in string.gmatch("hello world", "()hello()") as (fun() : number, number) do
    assert(a and b)
end