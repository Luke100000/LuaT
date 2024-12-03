# LuaT

Some extra syntax for less bulkier
[Lua Language Server](https://github.com/LuaLS/lua-language-server) type
annotations.

## Installation

Call the lua module at runtime to enable the code parser. This is required to
ignore the extra typing syntax.

```lua
require("LuaT"):install()
```

Or require files explicitely:

```lua
local plugin = require("LuaT")
local yourModule = plugin:require("path")
```

Then add the [plugin](https://luals.github.io/wiki/plugins/) to your vscode
settings:

```json
{
  "Lua.misc.parameters": ["--develop=true"],
  "Lua.runtime.plugin": "plugin.lua"
}
```

The plugin can be configured, check the plugins

## Features

The features are implemented lazily and may break on edge cases. To exclude a
file, add `--NoLuaT` at the beginning.

With respect to the newline-centered style of LuaDoc comments, most
transformations uses newline as delimiter. For example,
`local foo: number = 5; local bar: string = ""` is not supported despite
technically being valid Lua.

Things also won't work in the very first line.

### Inline type declarations

Types can now be declared similar to TypeScript using colon:

```lua
local variable: string = ""
local first: string, second: string = "", ""

function func(param1: string, param2: number) : nil

end

object.foo: number = 42
```

Which gets resolved to their respective `---@param`, `---@type`, and
`---@return`'s.

To avoid false detections, only locals and non-nested dot notation is supported.

Globals are not supported (you should not use many anyway), and neither are
nested definitions (`a.b.c`) or table indexing `a["b"]`.

You can also mix declaration types, e.g.; to add description to certain fields:

```lua
---@param param2 string This is fancy description
function Func(param1: string, param2) : number, number
    return 1, 2
end
```

### Fancy casts

_Unreliable, disabled by default._

```lua
value as type
```

now gets resolved to

```lua
value ---[[@as type]]
```

To skip false positives, only following syntaxes are supported:

- Any type declaration except function types followed by `,`, `)`, `]`, `}`, or
  `do \n`
  - If the type contains spaces, the entire type must be encaplulated by
    parantheses `(<type>)`
- A Function type in parantheses followed by `do \n`
  - This covers the common case of casting iterators to ensure iterator
    variables are typed.

```lua
-- Casting the gmatch iterator to type its iterator variables.
for a, b in string.gmatch("hello world", "()hello()") as (fun() : number, number) do
    print(a, b)
end
```

### Overriding methodes

Overriding a methode using `---@override` now inherits types and links the
super.

```lua
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
  return a + 1
end

--Func still has the type and comment from its super, causing a type mismatch here
b:func("string!")
```

This works by injecting aliases types.

### Support for assignment operators

Not really part of this plugins scope but included are assignment operators:

```lua
a += 1
a -= 1
a *= 1
a /= 1
a %= 1
a ^= 1
a ..= 1
```

Which gets resolved to `a = a operator`.

This may cause unexpected behavior if a contain stateful expressions.

Subexpressions such as within function calls are not supported.

### Constructors

This is a very specific injection for my class library:

```lua
---@class (exact) A
---@field param number
local A = ... -- Class library logic

---@param param number Comment
function A:init(param)
    self.param = param
end

--A is now complaining about the missing param
local a = A()
assert(a)
```
