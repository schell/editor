attribute vec2 position;
attribute vec2 uv;

varying vec2 vTex;

uniform mat4 modelview;
uniform mat4 projection;

void main () {
    vTex = uv;
    gl_Position = projection * modelview * vec4(position, 0.0, 1.0);
}

