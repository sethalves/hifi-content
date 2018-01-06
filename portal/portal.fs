#line 2

const float PI = 3.14159;
const float TAU = 2.0 * PI;
const float HALF_PI = PI / 2.0;
const float QUARTER_PI = HALF_PI / 2.0;

float getProceduralColors(inout vec3 diffuse, inout vec3 specular, inout float shininess) {
    vec3 v = normalize(_position.xyz);

    mat4 cameraView = getTransformCamera()._view;

    v = vec3(cameraView * vec4(v, 0.0));
    float theta = atan(v.x, v.z) + PI;
    float phi = acos(v.y);

    vec2 uv = vec2(theta / TAU, phi / PI);
    // diffuse = texture(iChannel0, uv).rgb;

    float w = (0.5 - (uv.x)) * (iResolution.x / iResolution.y);
    float h = 0.5 - uv.y;
    float distanceFromCenter = sqrt(w * w + h * h);
    float sinArg = distanceFromCenter * 10.0 - iGlobalTime * 1.2;
    float slope = cos(sinArg) ;
    diffuse = vec3(texture(iChannel0, uv + normalize(vec2(w, h)) * slope * 0.05));

    return 1.0;
}
