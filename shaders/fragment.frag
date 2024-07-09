#version 420

in vec4 pixel_color;

out vec4 frag_color;

void main() {
   frag_color = pixel_color;  
}