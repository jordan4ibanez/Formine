#version 420

layout (location = 0) in vec3 position;
layout (location = 1) in vec2 texture_coordinate;
layout (location = 2) in vec3 color;

uniform mat4 camera_matrix;
uniform mat4 object_matrix;

out vec3 pixel_color;
out vec2 output_texture_coordinate;

void main() {
  gl_Position = camera_matrix * object_matrix * vec4(position, 1.0);

  pixel_color = color;

  output_texture_coordinate = texture_coordinate;
}