#version 450
#extension GL_ARB_separate_shader_objects : enable

layout(binding = 1) uniform sampler2D texSampler[3];

layout(location = 0) in vec4 fragColor;
layout(location = 1) in vec2 fragTexCoord;
layout(location = 2) flat in int fragTexIndex;

layout(location = 0) out vec4 outColor;

void main() {
  outColor = (fragColor) * (texture(texSampler[fragTexIndex], fragTexCoord));
  // outColor = (fragColor) * (texture(texSampler[fragTexIndex], fragTexCoord));
  // outColor = fragColor;
}
