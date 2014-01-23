varying vec2 vTex;
varying vec4 vColor;

uniform sampler2D sampler;
uniform bool isTextured;

void main() {
    if (isTextured) {
        gl_FragColor = texture2D(sampler, vec2(vTex.s, vTex.t));
    } else {
        gl_FragColor = vColor;
    }
}

