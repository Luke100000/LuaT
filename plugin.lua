---@class LuaTLibrary
local plugin = {
    print = print,
    debug = false,
    modules = {
        as = false,
        defs = true,
        operators = true,
        types = true,
    }
}

---@class SignatureParserState
---@field valid boolean
---@field params string[]
---@field returnTypes string[]

---@return SignatureParserState
local function getState()
    return {
        valid = false,
        params = {},
        returnTypes = {},
    }
end

---@param text string
---@param sep string?
---@return string[]
local function split(text, sep)
    local fields = {}
    local pattern = string.format("([^%s]+)", sep or ",")
    ---@diagnostic disable-next-line: no-unknown
    local _ = text:gsub(pattern, function(c)
        table.insert(fields, c)
    end)
    return fields
end

---@param str string
---@return string
local function trim(str)
    return str:match("^%s*(.-)%s*$")
end

---Concatenates multiple tables into one
---@vararg table[]
---@return table
local function concat(...)
    local result = {}
    for _, t in ipairs({ ... }) do
        for _, v in ipairs(t) do
            table.insert(result, v)
        end
    end
    return result
end


---Type-safe version of string.gmatch
---@generic T1
---@generic T2
---@generic T3
---@generic T4
---@generic T5
---@generic T6
---@generic T7
---@generic T8
---@generic T9
---@param text string
---@param pattern string
---@param t1 `T1`?
---@param t2 `T2`?
---@param t3 `T3`?
---@param t4 `T4`?
---@param t5 `T5`?
---@param t6 `T6`?
---@param t7 `T7`?
---@param t8 `T8`?
---@param t9 `T9`?
---@return fun():T1, T2, T3, T4, T5, T6, T7, T8, T9
local function tGmatch(text, pattern, t1, t2, t3, t4, t5, t6, t7, t8, t9)
    return string.gmatch(text, pattern)
end

---Returns the type and its start position
---@param rawTypeStr string
---@return string
---@return integer
local function getType(rawTypeStr)
    local typeStr = trim(rawTypeStr)
    local offset = #rawTypeStr - #typeStr

    -- Functions are more tricky, only accept # as comment indicator
    if typeStr:sub(1, 5) == "fun()" then
        local commentStart = typeStr:find("#")
        if commentStart then
            return string.sub(typeStr, 1, commentStart - 1), commentStart
        else
            return typeStr, offset + #typeStr
        end
    end

    local openParentheses = 0
    local openCurlies = 0
    local openBrackets = 0
    local openAngleBrackets = 0
    for i = 1, #typeStr do
        local c = typeStr:sub(i, i)
        if c == "(" then
            openParentheses = openParentheses + 1
        elseif c == ")" then
            openParentheses = openParentheses - 1
        elseif c == "{" then
            openCurlies = openCurlies + 1
        elseif c == "}" then
            openCurlies = openCurlies - 1
        elseif c == "[" then
            openBrackets = openBrackets + 1
        elseif c == "]" then
            openBrackets = openBrackets - 1
        elseif c == "<" then
            openAngleBrackets = openAngleBrackets + 1
        elseif c == ">" then
            openAngleBrackets = openAngleBrackets - 1
        elseif (c == " " or c == ",") and openParentheses == 0 and openCurlies == 0 and openBrackets == 0 and openAngleBrackets == 0 then
            return string.sub(typeStr, 1, i - 1), offset + i
        elseif openParentheses < 0 or openCurlies < 0 or openBrackets < 0 or openAngleBrackets < 0 then
            return "", offset
        end
    end
    return typeStr, offset + #typeStr
end

---@param input string
---@return diff[]
local function injectFunctionDefinitions(input)
    local diffs = {}
    local cursor = 0
    local state = getState()
    local lastClass = "global"
    local lastParentClass = "global"

    ---@type table<string, integer>
    local classConstructorOverrides = {}

    ---@type integer[]?
    local override

    for line in string.gmatch(input .. "\n", "(.-)\n") do
        local commentStart = line:find("---")
        if commentStart and #line:sub(1, commentStart - 1):gsub(" ", ""):gsub("\t", "") == 0 then
            local trimmedLine = line:sub(commentStart)
            if trimmedLine:sub(1, 9) == "---@param" then
                state.valid = true

                ---@type string | nil, string | nil, string | nil
                local name, isOptional, typeStr = string.match(trimmedLine:sub(11),
                    "%s*(%.?%.?%.?[%a_]?[%w_]*)(%??) *([^#\n]*)")
                typeStr = getType(typeStr or "nil")
                if isOptional == "?" then
                    typeStr = typeStr .. " | nil"
                end

                table.insert(state.params, {
                    name or ("arg" .. #state.params),
                    typeStr,
                })
            elseif trimmedLine:sub(1, 10) == "---@return" then
                state.valid = true
                local typeStr = string.match(trimmedLine:sub(11), "%s*([^#\n]*)") or "nil"
                local type = getType(typeStr)
                table.insert(state.returnTypes, type)
            elseif trimmedLine:sub(1, 9) == "---@class" then
                local c = split(trimmedLine:sub(11):gsub("%(exact%) ", ""), ":")
                lastClass = trim(c[1] or "global")
                lastParentClass = getType(c[2] or "global")
                classConstructorOverrides[lastClass] = cursor + #line + 1
            elseif trimmedLine:sub(1) == "---@override" then
                override = { cursor + commentStart, cursor + #line }
            elseif trimmedLine:sub(1, 3) == "---" then
                state.valid = true
            end
        elseif line:find("function *[%a_][%w_]*%:[%a_][%w_]* *%(") then
            local method = line:sub(
                (line:find(":") or 0) + 1,
                (line:find("%(") or 0) - 1
            )

            -- TODO: Currently only LuaDoc types are captured, but LuaT should be forwarded as well from previous transformations

            --Add a class overload to forward the init definition as the constructor
            local cd = classConstructorOverrides[lastClass]
            if method == "init" and cd then
                local parts = {}
                for _, var in ipairs(state.params) do
                    table.insert(parts, var[1] .. " : " .. var[2])
                end
                table.insert(diffs, {
                    start  = cd + 1,
                    finish = cd,
                    text   = "---@overload fun(" .. table.concat(parts, ",") .. "): " .. lastClass .. "\n",
                })
                table.insert(diffs, {
                    start  = cd + 1,
                    finish = cd,
                    text   = "---@field super fun(self: " .. lastClass .. "):" .. lastParentClass .. "\n",
                })
            end

            if override then
                -- This method is marked as an override
                if lastParentClass ~= "global" then
                    local text = "---@type def." .. lastParentClass .. "." .. method
                    local textSee = "---@see " .. lastParentClass .. "." .. method
                    table.insert(diffs, {
                        start  = override[1],
                        finish = override[2],
                        text   = textSee .. "\n" .. text,
                    })
                    override = nil
                end
            elseif state.valid and lastClass ~= "global" then
                -- This is the top function definition, remember it with an alias
                local parts = {}
                for _, var in ipairs(state.params) do
                    table.insert(parts, var[1] .. " : " .. var[2])
                end
                local text = "---@alias def." ..
                    lastClass .. "." .. method .. " " ..
                    "fun(self: " ..
                    lastClass ..
                    ", " ..
                    table.concat(parts, ", ") ..
                    ")" .. (#state.returnTypes == 0 and "" or " : ") .. table.concat(state.returnTypes, ", ")
                table.insert(diffs, {
                    start  = cursor,
                    finish = cursor - 1,
                    text   = "\n" .. text,
                })
            end
        elseif state.valid then
            state = getState()
        end

        ---@type integer
        cursor = cursor + #line + 1
    end

    return diffs
end

---Replaces inline type definitions with LuaDoc type definitions
---@param text string
---@param mask table<number, boolean>
---@param syntaxOnly boolean
---@return diff[] | nil
local function replaceInlineTypes(text, mask, syntaxOnly)
    local diffs = {}

    -- For locals, several variables separated by comma are allowed
    local localPattern = "\n+()%s*local%s+()([%a_][^:\n;=]*:[^\n;=]*)()"
    for lineStart, typeStart, typesString, typeEnd in tGmatch(text, localPattern, "number", "number", "string", "number") do
        if not mask[typeStart] then
            local parsedTypes = {}
            local names = {}

            while true do
                local name
                name, typesString = typesString:match("^%s*([%a_][%w_]*)%s*:%s(.*)")
                if not name or #typesString == 0 then
                    break
                end
                local type, endPos = getType(typesString)
                if #type == 0 then
                    -- Malformed, line is probably a false positive
                    names = nil
                    break
                end
                ---@type string
                typesString = typesString:sub(endPos):match("^%s*[,]?%s*(.*)")
                table.insert(names, name)
                table.insert(parsedTypes, trim(type))
            end

            if names and #names > 0 then
                -- Add LuaDoc type
                if not syntaxOnly then
                    table.insert(diffs, {
                        start  = lineStart,
                        finish = lineStart - 1,
                        text   = "---@type " .. table.concat(parsedTypes, ", ") .. "\n",
                    })
                end

                -- Replace with names only
                table.insert(diffs, {
                    start  = typeStart + 1,
                    finish = typeEnd - 1,
                    text   = table.concat(names, ", "):sub(2),
                })
            end
        end
    end

    -- For fields, only a name.:field: type is allowed
    local fieldPattern = "\n+()%s*[%a_][%w_]*%s*[%.:]%s*[%a_][%w_]*()%s*:%s*([^\n;=]*)()[^%.%+%-%*/%^%%]="
    for lineStart, typeStart, typesString, typeEnd in tGmatch(text, fieldPattern, "number", "number", "string", "number") do
        if not mask[typeStart] then
            -- Add LuaDoc type
            if not syntaxOnly then
                table.insert(diffs, {
                    start  = lineStart,
                    finish = lineStart - 1,
                    text   = "---@type " .. typesString .. "\n",
                })
            end

            -- Replace with name only
            table.insert(diffs, {
                start  = typeStart,
                finish = typeEnd - 1,
                text   = "",
            })
        end
    end

    return diffs
end

---Replaces function type definitions with LuaDoc type definitions
---@param text string
---@param mask table<number, boolean>
---@param syntaxOnly boolean
---@return diff[] | nil
local function replaceFunctionTypes(text, mask, syntaxOnly)
    local diffs = {}
    for _, pattern in ipairs({
        "\n+()%s*local [%a_][%w_%.]*%s*=%s*function%s*%(()([^%(]*)%)() *([^\n]*)()", -- Local assignment style
        "\n+()%s*[%a_][%w_%.]*%s*=%s*function%s*%(()([^%(]*)%)() *([^\n]*)()",       -- Assignment style
        "\n+()[^\n]*function%s*[%a_][%w_%.%:]*%s*%(()([^%(]*)%)() *([^\n]*)()",      -- Function style
    }) do
        for lineStart, argumentsStart, argumentsString, returnTypeStart, returnString, typeEnd in tGmatch(text, pattern, "number", "number", "string", "number", "string", "number") do
            if not mask[argumentsStart] and (#returnString == 0 or returnString:sub(1, 1) == ":") then
                -- Parse arguments
                ---@type [string, string, number, number][]
                local params = {}
                for name, typeStart, paramType, typeEnd in tGmatch(argumentsString, ",?([^%:]+)()%:([^,]*)()", "string", "number", "string", "number") do
                    if #paramType > 0 then
                        name = trim(name)
                        paramType = trim(paramType)
                        if #name > 0 and #paramType > 0 then
                            table.insert(params,
                                { name, paramType, argumentsStart + typeStart - 1, argumentsStart + typeEnd - 1 })
                        end
                    end
                end

                -- Parse return types
                ---@type string[]
                local returns = {}
                for _, returnType in ipairs(split(returnString:sub(2), ",")) do
                    returnType = trim(returnType)
                    if #returnType > 0 then
                        table.insert(returns, returnType)
                    end
                end

                -- Add LuaDoc type
                if not syntaxOnly then
                    for _, param in ipairs(params) do
                        table.insert(diffs, {
                            start  = lineStart,
                            finish = lineStart - 1,
                            text   = ("---@param %s %s\n"):format(param[1], param[2]),
                        })
                    end
                    for _, returnType in ipairs(returns) do
                        table.insert(diffs, {
                            start  = lineStart,
                            finish = lineStart - 1,
                            text   = ("---@return %s\n"):format(returnType),
                        })
                    end
                end

                -- Remove LuaT parameter types
                for _, param in ipairs(params) do
                    table.insert(diffs, {
                        start  = param[3],
                        finish = param[4] - 1,
                        text   = "",
                    })
                end

                -- Remove LuaT return type
                if #returns > 0 then
                    table.insert(diffs, {
                        start  = returnTypeStart,
                        finish = typeEnd - 1,
                        text   = "",
                    })
                end
            end
        end
    end

    return diffs
end

---Adds += operator support
---@param text string
---@param mask table<number, boolean>
---@return diff[] | nil
local function addOperators(text, mask)
    local diffs = {}

    for variable, operator, pos in tGmatch(text, "[\n;]%s*([%a_][%w_%[%]\"']*)%s*(%.?[%.%+%-%*/%^%%])()=", "string", "string", "number") do
        if not mask[pos] then
            table.insert(diffs, {
                start  = pos - #operator,
                finish = pos,
                text   = "=" .. variable .. operator,
            })
        end
    end

    return diffs
end

---Adds fancy "as" syntax support
---@param text string
---@param mask table<number, boolean>
---@return diff[] | nil
local function addAs(text, mask)
    local diffs = {}
    for _, pattern in ipairs({
        "()%s+as%s+([^%(][^%s;%)%]}]*)() *[,\n;%)%]}]", -- Inline
        "()%s+as%s+([^%(][^%s]*)() *do *\n",            -- Loop
        "()%s+as%s+%(([^%)]*)%)() *[,\n;%)%]}]",        -- Inline complex
        "()%s+as%s+%(([^\n]*)%)() *do *\n",             -- Loop complex
    }) do
        for startPos, typeString, endPos in tGmatch(text, pattern, "number", "string", "number") do
            if not mask[startPos] then
                table.insert(diffs, {
                    start  = startPos,
                    finish = endPos - 1,
                    text   = "--[=[@as " .. typeString .. "]=]",
                })
            end
        end
    end
    return diffs
end

---returns a set of character positions commented out
local function getMask(text)
    ---@type table<number, boolean>
    local mask = {}
    for from, to in tGmatch(text, "%-%-()[^\n]*()", "number", "number") do
        for i = from, to - 1 do
            mask[i] = true
        end
    end
    return mask
end

---@class diff
---@field start  integer # The number of bytes at the beginning of the replacement
---@field finish integer # The number of bytes at the end of the replacement
---@field text   string  # What to replace

---@param  uri  string # The uri of file
---@param  text string # The content of file
---@param  syntaxOnly boolean # Whether to transform only the syntax (not changing line numbers)
function OnSetText(uri, text, syntaxOnly)
    if text:sub(1, 8) == "--NoLuaT" then
        return
    end
    local mask = getMask(text)
    return concat(
        plugin.modules.defs and not syntaxOnly and injectFunctionDefinitions(text) or {},
        plugin.modules.operators and addOperators(text, mask) or {},
        plugin.modules.types and replaceInlineTypes(text, mask, syntaxOnly) or {},
        plugin.modules.types and replaceFunctionTypes(text, mask, syntaxOnly) or {},
        plugin.modules.as and addAs(text, mask) or {}
    )
end

---@param p string
---@return string | nil
local function getFile(p)
    if love and love.filesystem then
        if p:sub(1, 1) == "." then
            p = p:sub(2)
        end
        local content = love.filesystem.read(p)
        return content
    else
        local f = io.open(p, "r")
        if f then
            local content = f:read("*all")
            f:close()
            return content
        end
    end
end

---@return number
local function clock()
    return love and love.timer.getTime() or os.clock()
end

local oldRequire = require

---Applies the diffs to the content
---@param text string
---@param diffs diff[]
---@return string
function plugin:apply(text, diffs)
    table.sort(diffs, function(a, b)
        return a.start < b.start
    end)
    local cur = 1
    local buf = {}
    local delta = 0
    for _, diff in ipairs(diffs) do
        table.insert(buf, text:sub(cur, diff.start - 1))
        table.insert(buf, diff.text)
        cur   = diff.finish + 1
        delta = delta + #diff.text - (diff.finish - diff.start + 1)
    end
    table.insert(buf, text:sub(cur))
    return table.concat(buf)
end

---Transform and require a file
---@param path string
function plugin:require(path)
    if package.loaded[path] then
        return package.loaded[path]
    end
    for searchPath in tGmatch(package.path, "([^;]+)", "string") do
        local p = searchPath:gsub("%?", path:gsub("%.", "/"))
        local content = getFile(p)
        if content then
            local t = clock()
            local diffs = OnSetText(path, content, not self.debug)
            if diffs then
                content = self:apply(content, diffs)

                -- Save a copy for debugging
                if self.debug and love then
                    self.print(("Transforming %s with %d diffs in %d ms"):format(p, #diffs,
                        math.floor((clock() - t) * 1000)))
                    love.filesystem.createDirectory("parsed")
                    love.filesystem.write("parsed/" .. path .. ".lua", content)
                end

                local ok, msg = load(content)
                if ok then
                    ---@type any
                    local module = ok()
                    ---@diagnostic disable-next-line: no-unknown
                    package.loaded[path] = module
                    return package.loaded[path]
                else
                    self.print("Error parsing " .. p .. ": " .. msg)
                end
            end
        end
    end
    return oldRequire(path)
end

---Replace the inbuilt require
function plugin:install()
    require = function(path)
        return plugin:require(path)
    end
end

return plugin
