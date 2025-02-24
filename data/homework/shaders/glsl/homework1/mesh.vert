#version 450

layout (location = 0) in vec3 inPos;
layout (location = 1) in vec3 inNormal;
layout (location = 2) in vec2 inUV;
layout (location = 3) in vec3 inColor;
layout (location = 4) in vec4 inTangent;
layout (location = 5) in vec4 inJointIndices;
layout (location = 6) in vec4 inJointWeights;

layout (set = 0, binding = 0) uniform UBOScene
{
	mat4 projection;
	mat4 view;
	vec4 lightPos;
	vec4 viewPos;
} uboScene;

layout(push_constant) uniform PushConsts {
	mat4 model;
} primitive;

layout (location = 0) out vec3 outNormal;
layout (location = 1) out vec3 outColor;
layout (location = 2) out vec2 outUV;
layout (location = 3) out vec3 outViewVec;
layout (location = 4) out vec3 outLightVec;
layout (location = 5) out vec3 outTangent;
layout (location = 6) out vec3 outBittangent;

void main() 
{
	outNormal = inNormal;
	outColor = inColor;
	outUV = inUV;
	
    gl_Position = uboScene.projection * uboScene.view * primitive.model * vec4(inPos.xyz, 1.0);

    float sign = inTangent.w;
    vec3 normalWS = mat3(uboScene.view) * mat3(primitive.model) * inNormal;
    vec3 tangentWS = mat3(uboScene.view) * mat3(primitive.model) * inTangent.xyz;
    vec3 bittangentWS = cross(normalWS, tangentWS) * sign;
	
    outNormal = normalWS;
    outTangent = tangentWS;
    outBittangent = bittangentWS;
	
	vec4 pos = uboScene.view * vec4(inPos, 1.0);
	vec3 lPos = mat3(uboScene.view) * uboScene.lightPos.xyz;
	outLightVec = uboScene.lightPos.xyz - pos.xyz;
	outViewVec = uboScene.viewPos.xyz - pos.xyz;	
}