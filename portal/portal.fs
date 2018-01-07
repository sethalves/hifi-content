#line 2

const float PI = 3.14159;
const float TAU = 2.0 * PI;

float getProceduralColors(inout vec3 diffuse, inout vec3 specular, inout float shininess) {
    vec3 v = normalize(_position.xyz);
    vec3 cameraPosition = inverse(getTransformCamera()._view)[3].xyz;
    vec3 entityPosition = iWorldPosition;
    vec3 cameraOffset = cameraPosition - entityPosition;
    cameraOffset.y = 0.0;
    cameraOffset = normalize(cameraOffset);

    float cameraSpin = atan(cameraOffset.z, cameraOffset.x);
    float theta = atan(v.z, v.x) - cameraSpin + PI;
    while (theta < 0.0) {
        theta += TAU;
    }
    while (theta > TAU) {
        theta -= TAU;
    }
    float phi = acos(v.y);

    vec2 uv = vec2(theta / TAU, phi / PI);

    vec2 warpUV = 2.0 * uv;
    float d = length(warpUV);
    float speed = 2.0;
    vec2 st = warpUV * 0.1 + 0.2 * vec2(cos(0.071 * iGlobalTime * speed + d),
                                        sin(0.073 * iGlobalTime * speed - d));
    vec3 warpedCol = texture(iChannel0, st).xyz * 2.0;
    float w = max(warpedCol.r, 0.85);


    vec2 offset = 0.01 * cos(warpedCol.rg * 3.14159);
    vec2 texCoord = uv + offset;
    vec3 col = texture(iChannel0, texCoord).rgb * vec3(0.8, 0.8, 1.5);
    col *= w * 1.2;

    diffuse = mix(col, texture(iChannel0, texCoord).rgb, 0.5);


    return 1.0;
}
