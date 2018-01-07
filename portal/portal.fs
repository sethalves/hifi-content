#line 2

const float PI = 3.14159;
const float TAU = 2.0 * PI;
const float HALF_PI = PI / 2.0;


float getProceduralColors(inout vec3 diffuse, inout vec3 specular, inout float shininess) {
    vec3 v = normalize(_position.xyz);

    mat4 cameraView = getTransformCamera()._view;

    // cameraView[3] = vec4(0.0, 0.0, 0.0, 1.0);
    // vec4 zAxis = cameraView * vec4(0.0, 0.0, 1.0, 0.0);
    // // cancel out the roll and pitch
    // vec3 newZ, newY, newX;
    // if (zAxis.x == 0 && zAxis.z == 0.0) {
    //     newZ = vec3(1.0, 0.0, 0.0);
    // } else {
    //     newZ = normalize(vec3(zAxis.x, 0.0, zAxis.z));
    // }
    // newX = cross(vec3(0.0, 1.0, 0.0), newZ);
    // newY = cross(newZ, newX);
    // cameraView = mat4(
    //     vec4(newX, 0.0),
    //     vec4(newY, 0.0),
    //     vec4(newZ, 0.0),
    //     vec4(0.0, 0.0, 0.0, 1.0));
    // v = vec3(cameraView * vec4(v, 0.0));


    vec3 cameraOffset = normalize(cameraView[3].xyz - iWorldPosition);
    float cameraSpin = atan(cameraOffset.x, cameraOffset.z);

    float theta = mod(atan(v.x, v.z) + cameraSpin, TAU);
    float phi = acos(v.y);

    vec2 uv = vec2(theta / TAU, phi / PI);
    // uv.y = sin(uv.y * HALF_PI);

    vec2 warpUV = 2.0 * uv;

    float d = length(warpUV);
    float speed = 2.0;
    vec2 st = warpUV * 0.1 + 0.2 * vec2(cos(0.071 * iGlobalTime * speed + d),
                                        sin(0.073 * iGlobalTime * speed - d));

    vec3 warpedCol = texture(iChannel0, st).xyz * 2.0;
    float w = max(warpedCol.r, 0.85);

    vec2 offset = 0.01 * cos(warpedCol.rg * 3.14159);
    vec2 texCoord = uv + offset;
    // texCoord.y = ((texCoord.y - 0.5) * 0.5) + 0.5; // because the portal is twice as tall as wide
    vec3 col = texture(iChannel0, texCoord).rgb * vec3(0.8, 0.8, 1.5);
    col *= w * 1.2;

    diffuse = mix(col, texture(iChannel0, texCoord).rgb, 0.5);


    return 1.0;
}
