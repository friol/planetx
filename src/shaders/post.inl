#pragma data_seg(".shader")
const char* post =
 "#version 130\n"
 "uniform sampler2D v;"
 "out vec4 r;"
 "float t(float v)"
 "{"
   "return fract(sin(dot(v,12.9898))*43758.5);"
 "}"
 "void main()"
 "{"
   "r=vec4(1,1,1,1);"
 "}";
