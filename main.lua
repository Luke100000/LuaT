-- Activate the plugin's parser
local plugin = require("plugin")
plugin.debug = true
plugin.modules.as = true
plugin:install()

-- Lua test suite
require("tests.external.z")

-- Own test suite
require("tests.as")
require("tests.class")
require("tests.constructor")
require("tests.operators")
require("tests.types")

os.exit(0)