varying vec2 vTex;

uniform sampler2D sampler;
uniform vec4 color;

void main() {
    vec4 tc = texture2D(sampler, vec2(vTex.s,vTex.t));
    gl_FragColor = vec4(color.r,color.g,color.b,tc.r);
}

