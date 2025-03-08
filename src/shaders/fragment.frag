#version 130
out vec4 o;
#define nm normalize
#define glf gl_FragCoord
#define ss smoothstep 
#define PI 3.14
#define fr fract

float iTime = gl_TexCoord[0].x/1000;
vec2 iResolution=vec2(gl_TexCoord[0].y,gl_TexCoord[0].z);

float noise(vec3 x) 
{
    vec3 p = floor(x);
    vec3 f = fr(x);
    f = f*f*(3.-2.*f);
    float n = p.x + p.y*157. + 113.*p.z;
    vec4 v3 = mix(fr(753.5*sin(n + vec4(0., 1., 157., 158.))), fr(753.5*sin(n + vec4(113., 114., 270., 271.))), f.z);
    vec2 v4 = mix(v3.xy, v3.zw, f.y);
    return mix(v4.x, v4.y, f.x);
}

float field(vec3 p) {
   mat3 M = mat3(0.28, 0.6, 0.65,0.06, 0.6, -0.74,-0.9, 0.26, 0.14);
   vec3 p1 = M*p;
   float n1 = noise(p1*5.);
   float n2 = noise(M*p1*10.);
   float n3 = noise(p1*20.);
   float n4 = noise(p1*40.);
   float rocky = 0.1*n1*n1 + 0.05*n2*n2 + 0.02*n3*n3 + 0.01*n4*n4;
   float sph_dist = length(p) - 1.035;
   return sph_dist + (sph_dist < 0.1 ? rocky*0.22 : 0.);
}

vec3 getNormal(vec3 p, float value, mat3 rot) {
    vec3 n = vec3(field(rot*vec3(p.x+.5,p.y,p.z)),
                  field(rot*vec3(p.x,p.y+.5,p.z)),
                  field(rot*vec3(p.x,p.y,p.z+.5)));
    return nm(n - value);
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
  const vec4 C = vec4(0.21,0.36,-0.57,0.03);
  vec2 i  = floor(v + dot(v, C.yy) );
  vec2 x0 = v -   i + dot(i, C.xx);

  vec2 i1;
  i1 = (x0.x > x0.y) ? vec2(1.0, 0.0) : vec2(0.0, 1.0);
  vec4 x12 = x0.xyxy + C.xxzz;
  x12.xy -= i1;
  i = mod289v2(i);
  vec3 p = permute( permute( i.y + vec3(0.0, i1.y, 1.0 ))
		+ i.x + vec3(0.0, i1.x, 1.0 ));
  vec3 m = max(0.5 - vec3(dot(x0,x0), dot(x12.xy,x12.xy), dot(x12.zw,x12.zw)), 0.0);
  m = m*m*m*m ;

  vec3 x = 2.0 * fr(p * C.www) - 1.0;
  vec3 h = abs(x) - 0.5;
  vec3 ox = floor(x + 0.5);
  vec3 a0 = x - ox;

  m*=1.79-0.85*(a0*a0+h*h);

  vec3 g;
  g.x  = a0.x  * x0.x  + h.x  * x0.y;
  g.yz = a0.yz * x12.xz + h.yz * x12.yw;
  return 130.0 * dot(m, g);
}

float fractalNoise(vec2 coord,float persistence,float lacunarity)
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

vec3 fractalNebula(vec2 coord, vec3 color, float transparency)
{
    return fractalNoise(coord, .5, 2.) * color * transparency;
}

float Star(vec2 uv, float flare)
{
    float d = length(uv);
  	float m = sin(.015*1.2)/d;  
    float rays = max(0.01, .5-abs(uv.x*uv.y*1000.)); 
    m += (rays*flare)*.1;
    m *= ss(1., .1, d);
    return m;
}

float Hash21(vec2 p){
    p = fr(p*vec2(123.34, 456.21));
    p += dot(p, p+45.32);
    return fr(p.x*p.y);
}

vec3 StarLayer(vec2 uv,float iTime)
{
    vec3 col = vec3(0);
    vec2 id = floor(uv);
    for(int y=-1;y<=1;y++){
        for(int x=-1; x<=1; x++){
            vec2 offs = vec2(x,y);
            float n = Hash21(id+offs);
            float size = fr(n);
            float star = Star(fr(uv)-offs-vec2(n, fr(n*34.))+.5, ss(.1,.9,size)*.46);
            vec3 color = sin(vec3(.12,.513,.79)*fr(n*2.2)*2.0*PI)*.25+.75;
            color = color*vec3(.69,.59,.49+size);
            star *= sin(iTime*.6+n*2.0*PI)*.5+.5;
            col += star*size*color;
        }
    }
    return col;
}

vec3 doStarBackground(float t)
{
    vec2 uv = (glf.xy-.5*iResolution.xy)/iResolution.y;
    vec3 col = fractalNebula(uv + vec2(.1, .1), vec3(0.3,0.3,0.2), 1.);
    col += fractalNebula(uv + vec2(0., .2), vec3(0.2,0.5,0.4), .5);
    col*=vec3(.74*sin(iTime/16.),.12,.861*cos(iTime/32.));

    for(float i=0.; i<1.; i+=.2)
    {
        float depth = fr(i+t);
        col += StarLayer(uv*mix(20., .5, depth)+i*453.2-iTime*.05,iTime)*depth*ss(1.,.9,depth);
    }   

    return col;
}

vec3 doPlanetX(float t,vec3 col)
{
    vec3 src = vec3(2. * (glf.xy - 0.5*iResolution.xy) / iResolution.yy, 2.0);
    src.y+=3.1-(iTime/32.0);
    vec3 dir = vec3(0,0,-1);
    
    float ang = iTime*0.032;
    mat3 rot = mat3(-sin(ang),0.0,cos(ang),0.,1.,0.,cos(ang),0.,sin(ang));

    if (iTime>78) 
    {
        src = vec3(8. * (glf.xy - 0.5*iResolution.xy) / iResolution.yy, 3.0);
        dir = vec3(0., 0., -1.21);
    }

    float atmos=0;
    vec3 loc;
    float value;
    for (int i=0; i < 256; i++) 
    {
        loc = src + t*dir;
        if (loc.z < -1.) break;
        value = field(rot*loc*0.54);
        if (value <= .00001) break;
        else atmos += 0.02+0.01*abs(sin(iTime/16.0));
        t += value*.5;
    }

    // ambient glow
    if (value > .00001) o = vec4(col,1);
    else 
    {
      float light = dot(getNormal(loc, value, rot), nm(vec3(0.,3.,1.)));
      float totalLight = mix(clamp(field(rot*(loc - 0.5 * dir))/0.5*1.2, 0., 1.), 1.0*max(0.,light), 0.7)*0.1;
      o = vec4(1.0*mix(vec3(0.61,.73,0.5), vec3(0.15,.16,0.72), 1.-(1.2-length(loc))*2.)*totalLight, 1.0);
    }

	vec2 p = 2.0* (glf.xy / iResolution.yy - vec2(0.5 /  iResolution.y * iResolution.x, 0.5));
	float q = max(0.51, min(1.0, dot(vec3(p, sqrt(1. - dot(p,p))), vec3(0., 0., 0.))));
    o += q * vec4(max(0.0, pow(dot(nm(src), nm(vec3(0.,2., 1))),1.0)) * pow(atmos,1.5) * vec3(0.5, 0.5, 0.9), 1.0);

    return col;
}

void main() 
{
    float t = iTime*.0015; 
    vec3 col=doStarBackground(t);
    col=doPlanetX(t,col);

    // fades
    if (iTime<3.)
    {
        o*=(iTime/3);
    }
    else if (iTime>=88)
    {
        o*=((89/4.-iTime/4.))>0?((89/4.-iTime/4.)):0;
    }
}
