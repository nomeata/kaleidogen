#define M_PI 3.1415926535897932384626433832795
// precision mediump float;
uniform float u_extraData[5];
varying vec2 vDrawCoord;
void main() {
  float extraData = u_extraData[0];
  float anim = u_extraData[4];
  if (length(vDrawCoord) > 1.0) { gl_FragColor = vec4(0.0,0.0,0.0,0.0); return; }
vec2 pos0 = vDrawCoord;
  if (extraData > 0.5) {
    if (length(vDrawCoord) > 0.9) {
      if (extraData > 1.5) {
        gl_FragColor = vec4(0,0,1.0,1.0);
      } else {
        gl_FragColor = vec4(0,0,0,0.0);
      };
      return;
    }
    pos0 = vDrawCoord / 0.9;
    }
float sub11 = clamp(anim * 2.0, 0.0, 1.0);
float sub22 = clamp(anim * 2.0 - 1.0, 0.0, 1.0);
float anim3 = smoothstep(0.0,1.0,sub22);
float phase4 = atan(pos0.x, pos0.y) + anim3 * 1.9634954084936207;
vec2 pos5 = (length(pos0) * vec2(cos(phase4),sin(phase4)));
float sub16 = clamp(sub11 * 2.0, 0.0, 1.0);
float sub27 = clamp(sub11 * 2.0 - 1.0, 0.0, 1.0);
float anim8 = smoothstep(0.0,1.0,sub27);
float len9 = length(pos5);
float phase10 = atan(pos5.x, pos5.y) + anim8 * (1.0 - len9) * -0.125;
vec2 pos11 = (len9 * vec2(cos(phase10),sin(phase10)));
float sub112 = clamp(sub16 * 3.0, 0.0, 1.0);
float sub213 = clamp(sub16 * 3.0 - 1.0, 0.0, 1.0);
float sub314 = clamp(sub16 * 3.0 - 2.0, 0.0, 1.0);
float anim15 = smoothstep(0.0,1.0,sub213);
float anim16 = smoothstep(0.0,1.0,sub112);
vec3 col17 = mix(vec3(1.0,1.0,1.0),vec3(0.9686274509803922,0.7764705882352942,8.627450980392164e-2),anim16);
float anim18 = smoothstep(0.0,1.0,sub314);
vec3 col19 = mix(vec3(1.0,1.0,1.0),vec3(0.9568627450980393,0.6431372549019608,5.098039215686275e-2),anim18);
vec2 tmp20 = 4.0* (vec2(1.0,0.0) + pos11);
vec3 col21 = ( abs(mod(tmp20.x + 1.0, 2.0) - 1.0) + abs(mod(tmp20.y, 2.0) - 1.0) < anim15 ? col19 : col17 );
  gl_FragColor = vec4(col21, 1.0);
}

