require("mods.testing")

print("hi")

local x = 5

print(x)

-- The max we want to do with interop is 4 variables.
function my_function(a, b, c, d)
  -- print("lua function test")
  -- print("a:")
  -- print(a)
  a(false, "Hello")

  print("[" .. b .. "]")
  -- print("b:")
  -- print("[" .. b .. "]")
  -- print("c:")
  -- print(c)
  -- print("d:")
  -- print(d)
end

block.register("test", {
  name = "air",
  textures = { "test.png" },
  draw_type = block.draw_type.air
})
