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

    // float cameraSpin = atan(cameraOffset.z, cameraOffset.x);
    float theta = atan(v.z, -v.x); // - cameraSpin + PI;
    while (theta < 0.0) {
        theta += TAU;
    }
    while (theta > TAU) {
        theta -= TAU;
    }
    float phi = acos(v.y);

    vec2 texCoord = vec2(theta / TAU, phi / PI);
    vec3 col = texture(iChannel0, texCoord).rgb;

    diffuse = mix(col, texture(iChannel0, texCoord).rgb, 0.5);

    return 1.0;
}
