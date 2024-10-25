---@diagnostic disable: unused-local
--[[

Formine API base.

This is written like this, so you can see the data using the Lua VSCode extension.

This will mimic the style I use in fortran.

]]


print("[LuaJIT API]: Initializing.")


--* TYPE DEFINITIONS. =================================================================================


--- @class Array<T>: { [integer] : T }


--- Block definition.
---
--- The texture array must contains 6 elements.
---
--- Textures are in the array as follows:
--- [-Z, +Z, -X, +X, -Y, +Y]
---
--- @class block_definition
--- @field name string
--- @field description string
--- @field textures Array<string>
--- @field draw_type draw_type


--- Biome definition.
---
--- @class biome_definition
--- @field biome_name string
--- @field grass_layer string
--- @field dirt_layer string
--- @field stone_layer string
--- todo: ores.



--* APIS. =================================================================================


--? BLOCK. =================================================================================

--- The block library.
--- You use this to set and get block definitions.
block = {}


--- A block's drawtype.
--- This is a reflection of the Fortran draw_type parameters.
--- @enum draw_type
block.draw_type = {
  air = 0,
  normal = 1
}


--- Register a block into the engine.
--- @param definition block_definition A block definition.
block.register = function(definition) end


--? BIOME. =================================================================================


biome = {}


--- Register a biome into the engine.
--- @param definition biome_definition A biome definition.
biome.register = function(definition) end


--? ENTITY. =================================================================================


entity = {}
