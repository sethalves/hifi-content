#line 2

const float PI = 3.14159;
const float TAU = 2.0 * PI;
const float HALF_PI = PI / 2.0;

float getProceduralColors(inout vec3 diffuse, inout vec3 specular, inout float shininess) {
    vec3 v = normalize(_position.xyz);
    float theta = atan(v.x, v.z);
    float phi = acos(v.y);

    vec2 uv = vec2(theta / TAU, phi / PI);
    diffuse = texture(iChannel0, uv).rgb;
    // diffuse = vec3(uv, 0.0);
    // diffuse = vec3(1.0, 1.0, 0.0);

    return 1.0;
}
