attribute vec2 a_position;
uniform vec2 u_windowSize;
uniform float u_extraData[5];
varying vec2 vDrawCoord;
void main() {
  vec2 pos = vec2(u_extraData[1],u_extraData[2]);
  float size = u_extraData[3];
  vDrawCoord = vec2(a_position);
  vec2 scaled_pos = vec2(1.0,-1.0) * (2.0 * (size * a_position + pos)/u_windowSize - vec2(1,1));
  gl_Position = vec4(scaled_pos, 0, 1);
}
