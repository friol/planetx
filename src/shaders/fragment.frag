
#version 130
out vec4 o;

#define PI 3.1415
#define Velocity .0015
#define StarGlow .015
#define StarSize 1.
#define ss smoothstep 

float noise(vec3 x) {
    vec3 p = floor(x);
    vec3 f = fract(x);
    f = f*f*(3.-2.*f);
	
    float n = p.x + p.y*157. + 113.*p.z;
    
    vec4 v1 = fract(753.5453123*sin(n + vec4(0., 1., 157., 158.)));
    vec4 v2 = fract(753.5453123*sin(n + vec4(113., 114., 270., 271.)));
    vec4 v3 = mix(v1, v2, f.z);
    vec2 v4 = mix(v3.xy, v3.zw, f.y);
    return mix(v4.x, v4.y, f.x);
}

float field(vec3 p) {
   // random rotation reduces artifacts
   mat3 M = mat3(0.28862355854826727, 0.6997227302779844, 0.6535170557707412,
                 0.06997493955670424, 0.6653237235314099, -0.7432683571499161,
                 -0.9548821651308448, 0.26025457467376617, 0.14306504491456504);
   vec3 p1 = M*p;
   vec3 p2 = M*p1;
   float n1 = noise(p1*5.);
   float n2 = noise(p2*10.);
   float n3 = noise(p1*20.);
   float n4 = noise(p1*40.);
   float rocky = 0.1*n1*n1 + 0.05*n2*n2 + 0.02*n3*n3 + 0.01*n4*n4;
   float sph_dist = length(p) - 1.035;
   return sph_dist + (sph_dist < 0.1 ? rocky*0.22 : 0.);
}

vec3 getNormal(vec3 p, float value, mat3 rot) {
    vec3 n = vec3(field(rot*vec3(p.x+.005,p.y,p.z)),
                  field(rot*vec3(p.x,p.y+.005,p.z)),
                  field(rot*vec3(p.x,p.y,p.z+.005)));
    return normalize(n - value);
}

vec2 mod289v2(vec2 x)
{
  return x - floor(x * (1.0 / 289.0)) * 289.0;
}

vec3 mod289(vec3 x)
{
  return x - floor(x * (1.0 / 289.0)) * 289.0;
}

vec3 permute(vec3 x) 
{
  return mod289(((x*34.0)+1.0)*x);
}

float snoise(vec2 v)
{
  const vec4 C = vec4(0.211324865405187,  // (3.0-sqrt(3.0))/6.0
                      0.366025403784439,  // 0.5*(sqrt(3.0)-1.0)
                     -0.577350269189626,  // -1.0 + 2.0 * C.x
                      0.024390243902439); // 1.0 / 41.0
// First corner
  vec2 i  = floor(v + dot(v, C.yy) );
  vec2 x0 = v -   i + dot(i, C.xx);

// Other corners
  vec2 i1;
  //i1.x = step( x0.y, x0.x ); // x0.x > x0.y ? 1.0 : 0.0
  //i1.y = 1.0 - i1.x;
  i1 = (x0.x > x0.y) ? vec2(1.0, 0.0) : vec2(0.0, 1.0);
  // x0 = x0 - 0.0 + 0.0 * C.xx ;
  // x1 = x0 - i1 + 1.0 * C.xx ;
  // x2 = x0 - 1.0 + 2.0 * C.xx ;
  vec4 x12 = x0.xyxy + C.xxzz;
  x12.xy -= i1;

// Permutations
  i = mod289v2(i); // Avoid truncation effects in permutation
  vec3 p = permute( permute( i.y + vec3(0.0, i1.y, 1.0 ))
		+ i.x + vec3(0.0, i1.x, 1.0 ));

  vec3 m = max(0.5 - vec3(dot(x0,x0), dot(x12.xy,x12.xy), dot(x12.zw,x12.zw)), 0.0);
  m = m*m ;
  m = m*m ;

  vec3 x = 2.0 * fract(p * C.www) - 1.0;
  vec3 h = abs(x) - 0.5;
  vec3 ox = floor(x + 0.5);
  vec3 a0 = x - ox;

  m *= 1.79284291400159 - 0.85373472095314 * ( a0*a0 + h*h );

  vec3 g;
  g.x  = a0.x  * x0.x  + h.x  * x0.y;
  g.yz = a0.yz * x12.xz + h.yz * x12.yw;
  return 130.0 * dot(m, g);
}

vec2 rand2(vec2 p)
{
    p = vec2(dot(p, vec2(12.9898,78.233)), dot(p, vec2(26.65125, 83.054543))); 
    return fract(sin(p) * 43758.5453);
}

float rand(vec2 p)
{
    return fract(sin(dot(p.xy ,vec2(54.90898,18.233))) * 4337.5453);
}

vec3 hsv2rgb(vec3 c)
{
    vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
    vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);
    return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);
}

float fractalNoise(in vec2 coord, in float persistence, in float lacunarity)
{    
    float n = 0.;
    float frequency = 1.;
    float amplitude = 1.;
    for (int o = 0; o < 5; ++o)
    {
        n += amplitude * snoise(coord * frequency);
        amplitude *= persistence;
        frequency *= lacunarity;
    }
    return n;
}

vec3 fractalNebula(in vec2 coord, vec3 color, float transparency)
{
    float n = fractalNoise(coord, .5, 2.);
    return n * color * transparency;
}

float Star(vec2 uv, float flare)
{
    float d = length(uv);
  	float m = sin(StarGlow*1.2)/d;  
    float rays = max(0.01, .5-abs(uv.x*uv.y*1000.)); 
    m += (rays*flare)*.1;
    m *= ss(1., .1, d);
    return m;
}

float Hash21(vec2 p){
    p = fract(p*vec2(123.34, 456.21));
    p += dot(p, p+45.32);
    return fract(p.x*p.y);
}


vec3 StarLayer(vec2 uv,float iTime)
{
    vec3 col = vec3(0);
    vec2 gv = fract(uv);
    vec2 id = floor(uv);
    for(int y=-1;y<=1;y++){
        for(int x=-1; x<=1; x++){
            vec2 offs = vec2(x,y);
            float n = Hash21(id+offs);
            float size = fract(n);
            float star = Star(gv-offs-vec2(n, fract(n*34.))+.5, ss(.1,.9,size)*.46);
            vec3 color = sin(vec3(.12,.513,.79)*fract(n*2345.2)*2.0*PI)*.25+.75;
            color = color*vec3(.69,.59,.49+size);
            star *= sin(iTime*.6+n*2.0*PI)*.5+.5;
            col += star*size*color;
        }
    }
    return col;
}

void main() 
{
    float iTime = gl_TexCoord[0].x/1000;
    vec2 iResolution=vec2(gl_TexCoord[0].y,gl_TexCoord[0].z);

    vec2 uv = (gl_FragCoord.xy-.5*iResolution.xy)/iResolution.y;
	vec2 M = vec2(0);
    //M -= vec2(M.x+sin(iTime*0.22), M.y-cos(iTime*0.22));
    float t = iTime*Velocity; 
    vec3 col = vec3(0);  

    vec3 nebulaColor1 = hsv2rgb(vec3(.15+.15*sin(iTime*.001), 0.15, .25));
	vec3 nebulaColor2 = hsv2rgb(vec3(.25+.25*sin(iTime*.0021), 0.1, .25));
    col += fractalNebula(uv + vec2(.1, .1), nebulaColor1, 1.);
    col += fractalNebula(uv + vec2(0., .2), nebulaColor2, .5);
    col*=vec3(.74*sin(iTime/16.),.12,.861*cos(iTime/32.));

    for(float i=0.; i<1.; i+=1./5.){
        float depth = fract(i+t);
        float scale = mix(20., .5, depth);
        float fade = depth*ss(1.,.9,depth);
        col += StarLayer(uv*scale+i*453.2-iTime*.05+M,iTime)*fade;
        }   
        
    // planet-x
    
    vec3 src = vec3(2. * (gl_FragCoord.xy - 0.5*iResolution.xy) / iResolution.yy, 2.0);
    src.y+=3.1-(iTime/32.0);
    vec3 dir = vec3(0., 0., -1.);
    
    float ang = iTime*0.032;
    mat3 rot = mat3(-sin(ang),0.0,cos(ang),0.,1.,0.,cos(ang),0.,sin(ang));
    
    float atmos = 0.0;

    vec3 loc = src;
    float value;
    int steps = 0;
    for (int i=0; i < 256; i++) 
    {
        steps++;
        loc = src + t*dir;
        if (loc.z < -1.) break;
        value = field(rot*loc*0.54);
        if (value <= .00001) break;
        if (value > 0.00001) atmos += 0.02+0.01*abs(sin(iTime/16.0));
        t += value*.5;
    }

    // ambient glow
    float ambient = clamp(field(rot*(loc - 0.5 * dir))/0.5*1.2, 0., 1.);

    if (value > .00001) o = vec4(col,1.0);
    else {
      vec3 normal = getNormal(loc, value, rot);
      float light = dot(normal, normalize(vec3(0.,3.,1.)));

      float totalLight = mix(ambient, 1.0*max(0.,light), 0.7)*0.1;
    
      vec3 color = mix(vec3(0.61,.73,0.5), vec3(0.15,.16,0.72), 1.-(1.2-length(loc))*2.);
      o = vec4(1.0*color*totalLight, 1.0);
    }
	vec2 p = 2.0* (gl_FragCoord.xy / iResolution.yy - vec2(0.5 /  iResolution.y * iResolution.x, 0.5));
	float q = max(0.51, min(1.0, dot(vec3(p, sqrt(1. - dot(p,p))), vec3(0., 0., 0.))));
    o += q * vec4(max(0.0, pow(dot(normalize(src), normalize(vec3(0.,2., 1))),1.0)) * pow(atmos,1.5) * vec3(0.5, 0.5, 0.9), 1.0);

    if (iTime<3.)
    {
        o*=(iTime/3);
    }
}
