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


biome.register({
  biome_name = "grasslands",
  grass_layer = "grass",
  dirt_layer = "dirt",
  stone_layer = "stone"
})


biome.register({
  biome_name = "desert",
  grass_layer = "sand",
  dirt_layer = "sand",
  stone_layer = "stone"
})
