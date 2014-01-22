attribute vec2 position;
attribute vec4 color;
attribute vec2 uv;

varying vec2 vTex;
varying vec4 vColor;

uniform mat4 modelview;
uniform mat4 projection;

void main () {
    vTex = uv;
    vColor = color;
    gl_Position = projection * modelview * vec4(position, 0.0, 1.0);
}
