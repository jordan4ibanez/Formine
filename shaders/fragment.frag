#version 420

in vec3 pixel_color;

out vec4 frag_color;

void main() {
   frag_color = vec4(pixel_color, 1.0);
}