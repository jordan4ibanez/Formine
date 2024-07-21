#version 420

layout (location = 0) in vec3 position;
// layout (location = 1) in vec2 textureCoordinate;
layout (location = 1) in vec3 color;

out vec3 pixel_color;

uniform mat4 camera_matrix;
uniform mat4 object_matrix;

void main() {
  // camera_matrix * object_matrix *
  gl_Position =  vec4(position, 0.0);

  pixel_color = color;
}