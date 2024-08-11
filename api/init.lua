---@diagnostic disable: unused-local
--[[

Forgotten Lands API base.

This is written like this, so you can see the data using the Lua VSCode extension.

This will mimic the style I use in fortran.

]]


print("[LuaJIT API]: Initializing.")


--* TYPE DEFINITIONS. =================================================================================


--- @class Array<T>: { [integer] : T }


--- A block definition.
---
--- The texture array must contains 6 elements.
--- @class block_definition
--- @field name string
--- @field textures Array<string>
--- @field draw_type draw_type


--* APIS. =================================================================================


--? BLOCK. =================================================================================

--- The block library.
--- You use this to set and get block definitions.
block = {}


--- A block's drawtype.
---@enum draw_type
block.draw_type = {
  air = 0,
  normal = 1
}


--- Register a block into the engine.
--- @param name string The name of the block.
--- @param data_table block_definition A block definition.
block.register = function(name, data_table) end


--? WORLD. =================================================================================


world = {

}


--? ENTITY. =================================================================================


entity = {

}
