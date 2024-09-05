#version 410

in vec3 pixel_color;
in vec2 output_texture_coordinate;

uniform sampler2D texture_sampler;

out vec4 frag_color;

void main() {
   frag_color = texture(texture_sampler, output_texture_coordinate) * vec4(pixel_color, 1.0);
}