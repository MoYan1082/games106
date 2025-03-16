#version 450

layout (set = 0, binding = 0) uniform UBOScene
{
	mat4 projection;
	mat4 view;
	vec4 lightPos;
	vec4 viewPos;
} uboScene;

layout (set = 1, binding = 0) uniform sampler2D samplerColorMap;
layout (set = 1, binding = 1) uniform sampler2D samplerNormalMap;
layout (set = 1, binding = 2) uniform sampler2D samplerMRMap;

layout (location = 0) in vec3 inNormal;
layout (location = 1) in vec3 inColor;
layout (location = 2) in vec2 inUV;
layout (location = 3) in vec3 inViewVec;
layout (location = 4) in vec3 inLightVec;
layout (location = 5) in vec3 inTangent;
layout (location = 6) in vec3 inBittangent;

layout (location = 0) out vec4 outFragColor;

const float PI = 3.14159265359;

// Normal Distribution function --------------------------------------
float D_GGX(float dotNH, float roughness)
{
	float alpha = roughness * roughness;
	float alpha2 = alpha * alpha;
	float denom = dotNH * dotNH * (alpha2 - 1.0) + 1.0;
	return (alpha2)/(PI * denom*denom); 
}

// Geometric Shadowing function --------------------------------------
float G_SchlicksmithGGX(float dotNL, float dotNV, float roughness)
{
	float r = (roughness + 1.0);
	float k = (r*r) / 8.0;
	float GL = dotNL / (dotNL * (1.0 - k) + k);
	float GV = dotNV / (dotNV * (1.0 - k) + k);
	return GL * GV;
}

// Fresnel function ----------------------------------------------------
vec3 F_Schlick(float cosTheta, float metallic, vec3 materialcolor)
{
	vec3 F0 = mix(vec3(0.04), materialcolor, metallic); // * material.specular
	vec3 F = F0 + (1.0 - F0) * pow(1.0 - cosTheta, 5.0); 
	return F;    
}

// Specular BRDF composition --------------------------------------------

vec3 BRDF(vec3 L, vec3 V, vec3 N, float metallic, float roughness, vec3 materialcolor)
{
	// Precalculate vectors and dot products	
	vec3 H = normalize (V + L);
	float dotNV = clamp(dot(N, V), 0.0, 1.0);
	float dotNL = clamp(dot(N, L), 0.0, 1.0);
	float dotLH = clamp(dot(L, H), 0.0, 1.0);
	float dotNH = clamp(dot(N, H), 0.0, 1.0);

	// Light color fixed
	vec3 lightColor = vec3(1.0);

	vec3 color = vec3(0.0);

	if (dotNL > 0.0)
	{
		float rroughness = max(0.05, roughness);
		// D = Normal distribution (Distribution of the microfacets)
		float D = D_GGX(dotNH, roughness); 
		// G = Geometric shadowing term (Microfacets shadowing)
		float G = G_SchlicksmithGGX(dotNL, dotNV, rroughness);
		// F = Fresnel factor (Reflectance depending on angle of incidence)
        vec3 F = F_Schlick(dotNV, metallic, materialcolor);

		vec3 spec = D * F * G / (4.0 * dotNL * dotNV);

		color += spec * dotNL * lightColor;
	}

	return color;
}

// ----------------------------------------------------------------------------

void main() 
{
	vec4 color = texture(samplerColorMap, inUV) * vec4(inColor, 1.0);
	vec4 mr = texture(samplerMRMap, inUV);

	float metallic = mr.b;
	float roughness = mr.g;

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
    return;
	// =====================================================================
    metallic = 1;
    roughness = 0.5;
	
	vec3 Lo = vec3(0.0);
	Lo += BRDF(L, V, N, metallic, roughness, color.xyz);

    color.xyz = color.xyz * 0.02;
    color.xyz = Lo;
    // color.xyz = vec3(roughness, roughness, roughness);
    // color.xyz = pow(color.xyz, vec3(0.4545));
    outFragColor = vec4(color.rgb, 1.0);
	// outFragColor = vec4(diffuse * color.rgb + specular, 1.0);		
    // outFragColor = vec4(normalWS.xyz, 1.0);
}