#version 450

layout (set = 0, binding = 0) uniform UBOScene
{
	mat4 projection;
	mat4 view;
	vec4 lightPos;
	vec4 viewPos;
} uboScene;

layout (set = 1, binding = 0) uniform sampler2D samplerColorMap;
layout (set = 2, binding = 0) uniform sampler2D samplerNormalMap;

layout (location = 0) in vec3 inNormal;
layout (location = 1) in vec3 inColor;
layout (location = 2) in vec2 inUV;
layout (location = 3) in vec3 inViewVec;
layout (location = 4) in vec3 inLightVec;
layout (location = 5) in vec3 inTangent;
layout (location = 6) in vec3 inBittangent;

layout (location = 0) out vec4 outFragColor;

void main() 
{
	vec4 color = texture(samplerColorMap, inUV) * vec4(inColor, 1.0);
	// vec4 color = vec4(inColor, 1.0);
	vec3 normalTS = texture(samplerNormalMap, inUV).xyz;

	vec3 normalWS = inNormal;
	vec3 tangentWS = inTangent;
    vec3 bittangentWS = inBittangent;
	
	mat3 tangentToWorld = mat3(tangentWS.xyz, bittangentWS.xyz, normalWS.xyz);
	normalWS = tangentToWorld * normalTS;

    vec3 N = normalize(normalWS);
	vec3 L = normalize(inLightVec);
	vec3 V = normalize(inViewVec);
	vec3 R = reflect(L, N);
	vec3 diffuse = max(dot(N, L), 0.15) * inColor;
	vec3 specular = pow(max(dot(R, V), 0.0), 16.0) * vec3(0.75);
	outFragColor = vec4(diffuse * color.rgb + specular, 1.0);		
    // outFragColor = vec4(normalWS.xyz, 1.0);
}