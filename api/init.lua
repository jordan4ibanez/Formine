--[[

Forgotten Lands API base.

This is written like this, so you can see the data using the Lua VSCode extension.

]]

print("[LuAJIT API]: Initializing.")

blocks = {
  --- Register a block into the engine.
  ---@param name string The name of the block.
  ---@param data_table table A table containing block definition data.
  register_block = function(name, data_table) print("test") end,
  test = 5.5
}

world = {

}

entity = {

}
