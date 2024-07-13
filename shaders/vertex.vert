#version 420

layout (location = 0) in vec3 position;
// layout (location = 1) in vec2 textureCoordinate;
layout (location = 1) in vec4 color;

out vec4 pixel_color;
uniform mat4 objectMatrix;
uniform mat4 camera_matrix;


void main() {
  gl_Position = camera_matrix * objectMatrix * vec4(position, 0.0);

  pixel_color = color;
}