-- ? BLOCKS.


block.register({
  name = "stone",
  description = "Stone",
  textures = { "default_stone.png", "default_stone.png", "default_stone.png", "default_stone.png", "default_stone.png", "default_stone.png" },
  draw_type = block.draw_type.normal,
})


block.register({
  name = "dirt",
  description = "Dirt",
  textures = { "default_dirt.png", "default_dirt.png", "default_dirt.png", "default_dirt.png", "default_dirt.png", "default_dirt.png" },
  draw_type = block.draw_type.normal,
})


block.register({
  name = "grass",
  description = "Grass",
  textures = { "default_grass.png", "default_grass.png", "default_grass.png", "default_grass.png", "default_grass.png", "default_grass.png" },
  draw_type = block.draw_type.normal,
})


block.register({
  name = "sand",
  description = "Sand",
  textures = { "default_sand.png", "default_sand.png", "default_sand.png", "default_sand.png", "default_sand.png", "default_sand.png" },
  draw_type = block.draw_type.normal,
})


block.register({
  name = "bedrock",
  description = "Bedrock",
  textures = { "default_bedrock.png", "default_bedrock.png", "default_bedrock.png", "default_bedrock.png", "default_bedrock.png", "default_bedrock.png" },
  draw_type = block.draw_type.normal,
})


-- ? BIOMES.

-- Note: Default biome.
biome.register({
  name = "grasslands",
  grass_layer = "grass",
  dirt_layer = "dirt",
  stone_layer = "stone",
  heat_min = 0,
  heat_max = 0
})


biome.register({
  name = "desert",
  grass_layer = "sand",
  dirt_layer = "sand",
  stone_layer = "stone",
  heat_min = 0.8,
  heat_max = 1.0
})
