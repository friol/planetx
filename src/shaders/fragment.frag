#version 130
out vec4 o;
float iTime = gl_TexCoord[0].x/1000;
vec2 ir=vec2(gl_TexCoord[0].y,gl_TexCoord[0].z);

// bsky

vec2 bskygrad( ivec2 z )
{
    int n = z.x+z.y*21111;
    n = (n<<13)^n;
    float g=(n*(n*n*15+21)+19)>>16;
    return vec2(cos(g),sin(g));
}

float bskynoise(vec2 p)
{
    ivec2 i = ivec2(floor( p ));
    vec2 f=fract( p );
	vec2 u = f*f*(3.0-2.0*f);
    return mix( mix( dot( bskygrad( i+ivec2(0,0) ), f-vec2(0.0,0.0) ), 
                     dot( bskygrad( i+ivec2(1,0) ), f-vec2(1.0,0.0) ), u.x),
                mix( dot( bskygrad( i+ivec2(0,1) ), f-vec2(0.0,1.0) ), 
                     dot( bskygrad( i+ivec2(1,1) ), f-vec2(1.0,1.0) ), u.x), u.y);
}

vec4 bskyFun()
{
    vec2 p = (2.*gl_FragCoord.xy - ir) / ir.y;
    
    vec3 c; float f,w; vec2 fp;
    for(float i = 0.; i < 70.;i++) {        
      
        fp = 16. * p*i*.002/(p.y+1.8);
        fp.x += iTime*.5;

        mat2 m = mat2( 1.6,  1.2, -1.2,  1.6 );
        f  = .5*bskynoise(fp); fp = m*fp*1.1;
		f += .25*bskynoise(fp); fp*=m;
        f += .125*bskynoise(fp); fp*=m;
        f = smoothstep(-.04,.7,f);
        w += f;
    }
    w /= 59.5;
    
    vec3 col = mix(vec3(smoothstep(-.11,1.1,w)),vec3(6.,2.,.5), smoothstep(.05,1.,w));  
    vec2 uv = gl_FragCoord.xy/ir;
    vec2 n = uv*(1. - uv) * 6.;
    col *= pow(n.x*n.y,.5);
    return vec4(col*2.,0)*vec4(0.82,1.5,2.5,1.0);
}

//
// part 1 stuff
//

float noise(vec3 x) 
{
    vec3 p = floor(x);
    vec3 f = fract(x);
    f = f*f*(3.-2.*f);
    float n = p.x + p.y*157. + 113.*p.z;
    vec4 v3 = mix(fract(753.5*sin(n + vec4(0., 1., 157., 158.))), fract(753.5*sin(n + vec4(113., 114., 270., 271.))), f.z);
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

vec3 getNormal(vec3 p, float value, mat3 rot) 
{
    return normalize(vec3(field(rot*vec3(p.x+.5,p.y,p.z)),field(rot*vec3(p.x,p.y+.5,p.z)),field(rot*vec3(p.x,p.y,p.z+.5))) - value);
}

/*
vec3 mod289(vec3 x)
{
  return x - floor(x*(1.0 / 289.0)) * 289.0;
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
  i-=floor(i*.0034)*289.0;
  vec3 p = permute( permute( i.y + vec3(0.0, i1.y, 1.0 ))
		+ i.x + vec3(0.0, i1.x, 1.0 ));
  vec3 m = max(0.5 - vec3(dot(x0,x0), dot(x12.xy,x12.xy), dot(x12.zw,x12.zw)), 0.0);
  m = m*m*m*m ;

  vec3 x = 2.0 * fract(p * C.www) - 1.0;
  vec3 h = abs(x) - 0.5;
  vec3 ox = floor(x + 0.5);
  vec3 a0 = x - ox;

  m*=1.79-0.85*(a0*a0+h*h);

  vec3 g;
  g.x  = a0.x  * x0.x  + h.x  * x0.y;
  g.yz = a0.yz * x12.xz + h.yz * x12.yw;
  return 130.0 * dot(m, g);
}
*/

vec3 fractalNebula(vec2 coord, vec3 color, float transparency)
{
    float n = 0.;
    float frequency = 1.;
    float amplitude = 1.;
    for (int o = 0; o < 5; ++o)
    {
        //n += amplitude * snoise(coord * frequency);
        n += amplitude * bskynoise(coord * frequency);
        amplitude/=2;
        frequency*=2;
    }
    return n*color*transparency;
}

float Star(vec2 uv, float flare)
{
    float d = length(uv);
  	float m = sin(.015*1.2)/d;  
    float rays = max(0.01, .5-abs(uv.x*uv.y*1000.)); 
    m += (rays*flare)*.1;
    m *= smoothstep(1., .1, d);
    return m;
}

float Hash21(vec2 p)
{
    p = fract(p*vec2(123.34, 456.21));
    p += dot(p, p+45.32);
    return fract(p.x*p.y);
}

vec3 StarLayer(vec2 uv,float iTime)
{
    vec3 col = vec3(0);
    vec2 id = floor(uv);
    for(int y=-1;y<=1;y++){
        for(int x=-1; x<=1; x++){
            vec2 offs = vec2(x,y);
            float n = Hash21(id+offs);
            float size = fract(n);
            float star = Star(fract(uv)-offs-vec2(n, fract(n*34.))+.5, smoothstep(.1,.9,size)*.46);
            vec3 color = sin(vec3(.12,.513,.79)*fract(n*2.2)*6.28)*.25+.75;
            color = color*vec3(.69,.59,.49+size);
            star *= sin(iTime*.6+n*6.28)*.5+.5;
            col += star*size*color;
        }
    }
    return col;
}


vec3 doStarBackground(float t)
{
    vec2 uv = (gl_FragCoord.xy-.5*ir)/ir.y;
    vec3 col = fractalNebula(uv + vec2(.1, .1), vec3(0.3,0.3,0.2), 1.);
    col += fractalNebula(uv + vec2(0., .2), vec3(0.2,0.5,0.4), .5);
    col*=vec3(.74*sin(iTime/16.),.12,.861*cos(iTime/32.));

    for(float i=0.; i<1.; i+=.2)
    {
        float depth = fract(i+t);
        col += StarLayer(uv*mix(20., .5, depth)+i*453.2-iTime*.05,iTime)*depth*smoothstep(1.,.9,depth);
    }   

    return col;
}

vec3 doPlanetX(float t,vec3 col)
{
    vec3 src = vec3(2. * (gl_FragCoord.xy-.5*ir) / ir.yy, 2.0);
    src.y+=3.1-(iTime/32.0);
    vec3 dir = vec3(0,0,-1);
    
    float ang = iTime*0.032;
    mat3 rot = mat3(-sin(ang),0.0,cos(ang),0.,1.,0.,cos(ang),0.,sin(ang));

    if (iTime>78) 
    {
        src = vec3(8. * (gl_FragCoord.xy-.5*ir) / ir.yy, 3.0);
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
      float light = dot(getNormal(loc, value, rot), normalize(vec3(0.,3.,1.)));
      float totalLight = mix(clamp(field(rot*(loc - 0.5 * dir))/0.5*1.2, 0., 1.), 1.0*max(0.,light), 0.7)*0.1;
      o = vec4(1.0*mix(vec3(0.61,.73,0.5), vec3(0.15,.16,0.72), 1.-(1.2-length(loc))*2.)*totalLight, 1.0);
    }

	vec2 p = 2.0* (gl_FragCoord.xy / ir.yy - vec2(0.5 /  ir.y * ir.x, 0.5));
	float q = max(0.51, min(1.0, dot(vec3(p, sqrt(1. - dot(p,p))), vec3(0., 0., 0.))));
    o += q * vec4(max(0.0, pow(dot(normalize(src), normalize(vec3(0.,2., 1))),1.0)) * pow(atmos,1.5) * vec3(0.5, 0.5, 0.9), 1.0);

    return col;
}

//
// part 2-3
//


// ship

float opSmoothUnion( float d1, float d2, float k )
{
    float h = max(k-abs(d1-d2),0.0);
    return min(d1, d2) - h*h*5.;
}

float ndot(vec2 a, vec2 b ) { return a.x*b.x - a.y*b.y; }
float sdRhombus(vec3 p)
{
  p = abs(p);
  vec2 b = vec2(.53,.33);
  float f = clamp( (ndot(b,b-2.0*p.xz))/dot(b,b), -1.0, 1.0 );
  vec2 q = vec2(length(p.xz-0.5*b*vec2(1.0-f,1.0+f))*sign(p.x*b.y+p.z*b.x-b.x*b.y)-.2, p.y-.015);
  return min(max(q.x,q.y),0.0) + length(max(q,0.0));
}

float sdEllipsoid( vec3 p, vec3 r )
{
  float k0 = length(p/r);
  float k1 = length(p/(r*r));
  return k0*(k0-1)/k1;
}

float map(vec3 pos)
{
	vec3 q = pos;

    //LinkBetweenReactors
	float link = sdRhombus(q);
 
	//Core 
	q = pos + vec3(0.0,0.,-0.4);
	float core1 = sdEllipsoid(q, vec3(0.2,0.15,1.));
	link = opSmoothUnion(core1, link, 0.05);

    //Cockpit
	q = pos + vec3(0.0,-0.1,-0.12);
	float cockpit = sdEllipsoid(q, vec3(0.1,0.1,0.42));
	link = min(cockpit, link);
	
	q = pos + vec3(0.75,0.0,0.);
	float leftengine = sdEllipsoid(q, vec3(0.07,0.07,0.5));
    link = opSmoothUnion(leftengine, link,0.05);

	q = pos + vec3(-0.75,0.0,0.);
	float rightengine = sdEllipsoid(q, vec3(0.07,0.07,0.5));
	link = opSmoothUnion(rightengine, link,0.05);

    return link;
}

vec2 RayMarch(vec3 ro, vec3 rd) {
	float dO=0;
    float dM=256;
    for(int i=0; i<300; i++) {
    	vec3 p = ro + rd*dO;
        float dS = map(p);
        if(dS<dM) dM = dS;
        dO += dS;
        if(dO>256 || abs(dS)<.001) break;
    }
    
    return vec2(dO, dM);
}

vec3 GetNormal(vec3 p) 
{
    vec2 e = vec2(.001,0);
    return normalize(map(p) - vec3(map(p-e.xyy),map(p-e.yxy),map(p-e.yyx)));
}

vec3 R(vec2 uv, vec3 p, vec3 l, float z) {
    vec3 f = normalize(l-p),
        r = normalize(cross(vec3(0,1,0), f)),
        u = cross(f,r),
        c = p+f*z,
        i = c + uv.x*r + uv.y*u,
        d = normalize(i-p);
    return d;
}

// lscape

mat2 rot(float a) 
{
	return mat2(cos(a),-sin(a),sin(a),cos(a));	
}

vec2 lscapefield(in vec3 p) 
{
    vec3 l = vec3(1.);
	float s=2.,e,f,o;
	for(e=f=p.y;s<8e2;s*=1.6)
            p.xz*=rot(s),
            e+=abs(dot(sin(p*s)/s,.4*l)),
            f+=abs(dot(sin(p.xz*s*.5)/s,l.xz));
	o = 1.+ (f>.001?e:-exp(-f*f));
    return vec2(max(o,0.),min(f,max(e,.07)));
}

vec4 landScape( in vec3 ro, vec3 rd )
{
    float t = 2.5;
    float dt = .031;
    vec3 col;
    for( int i=0; i<80; i++ )
	{                
        vec2 v = lscapefield(ro+t*rd);  
        float c=v.x, f=v.y;
        t+=dt*f;
        dt *= 1.03;
        col = .95*col+ .09*vec3(c*c*c,c*c,c);	
    }
    return vec4(col,t);
}

vec4 doLandscape(vec2 p,vec2 q,vec3 rols)
{
    vec3 ta;
    vec3 ww = normalize( ta - rols );
    vec3 uu = normalize( cross(ww,vec3(0,1,0) ) );
    vec3 vv = normalize( cross(uu,ww));
    vec3 rd = normalize( p.x*uu + p.y*vv + 2.0*ww );
    rols.z -=iTime*.4;

    vec4 lss=landScape(rols,rd);
    vec3 lscapeCol = landScape(rols,rd).xyz;
	
    lscapeCol=.75*(log(.25+lscapeCol));
    return vec4(lscapeCol,lss.w);
}

/*
// lens flare

float rnd(float w)
{
    return fract(sin(w)*1000.);
}

float regShape(vec2 p,float N)
{
    float a=atan(p.x,p.y)+.2;
    float b=6.2/N;
    return smoothstep(.5,.51, cos(floor(.5+a/b)*b-a)*length(p.xy));
}

vec3 circle(vec2 p, float size, float decay, vec3 color,vec3 color2, float dist, vec2 mouse)
{
    float l = length(p + mouse*(dist*4.))+size/2.;
    
    //l2 is used in the rings as well...somehow...
    float l2 = length(p + mouse*(dist*4.))+size/3.;
    
    ///these are circles, big, rings, and  tiny respectively
    float c = max(0.01-pow(length(p + mouse*dist), size*1.4), 0.0)*50.;
    float c1 = max(0.001-pow(l-0.3, 1./40.)+sin(l*30.), 0.0)*3.;
    float c2 =  max(0.04/pow(length(p-mouse*dist/2. + 0.09)*1., 1.), 0.0)/20.;
    float s = max(0.01-pow(regShape(p*5. + mouse*dist*5. + 0.9, 6),1), 0.0)*5.;
    
   	color = .5*sin(color);
    color = cos(vec3(0.44, .24, .2)*8. + dist*4.)*0.5+1;
 	return c*color + c1*color+ c2*color+ s*color-0.01;
}

#define part2start 36.
#define part3start 42.

void doFlare( out vec4 fragColor, vec2 fragCoord )
{
	vec2 uv = fragCoord.xy / ir-0.5;
    uv.x*=ir.x/ir.y;
    
    float si=sin(iTime);
    vec2 mm = vec2(-.6+.02*si,.3+.02*si);
    if (iTime>=part3start) mm = vec2(-.6+.02*si,.6+.02*si);
    
    vec3 color = mix(vec3(0.3, 0.2, 0.02)/0.9, vec3(0.2, 0.5, 0.8), uv.y)*3.-0.52*si;
    
    //this calls the function which adds three circle types every time through the loop based on parameters I
    //got by trying things out. rnd i*2000. and rnd i*20 are just to help randomize things more
    for(float i=0.;i<10.;i++){
        color += circle(uv, pow(rnd(i*2000.)*1.8, 2.)+1.41, 0.0, vec3(0.9, 0.2, 0.1)+i , vec3(0.3, 0.1, 0.9)+i, rnd(i*20.)*3.+0.2-.5, mm);
    }
    //get angle and length of the sun (uv - mouse)
    float a = atan(uv.y-mm.y, uv.x-mm.x);
    float l = max(1.0-length(uv-mm)-0.84, 0.0);
    
    //add the sun with the frill things
    color += max(0.1/pow(length(uv-mm)*5., 5.), 0.0)*abs(sin(a*5.+cos(a*9.)))/20.;
    color += max(0.1/pow(length(uv-mm)*10., 1./20.), .0)+abs(sin(a*3.+cos(a*9.)))/8.*(abs(sin(a*9.)))/1.;
    //add another sun in the middle (to make it brighter)  with the20color I want, and bright as the numerator.
    color += (max(.1/pow(length(uv-mm)*4., 1./2.), 0.0)*4.)*vec3(0.8, 0.84, 1.2);
    	
    //multiply by the exponetial e^x ? of 1.0-lt which kind of masks the brightness more so that
    //there is a sharper roll of of the light decay from the sun. 
    color*= exp(1.0-length(uv-mm))/5.;
	fragColor = vec4(color,1.0);
}
*/

// new flare

float sdHex(vec2 p)
{
    p = abs(p);
	vec2 q = vec2(p.x*2.0*0.57,p.y+p.x*0.57);
	return dot(step(q.xy,q.yx), 1.0-q.yx);
}

//fakes x^n for specular effects (k is 0-1)
float fpow(float x, float k){
    return x > k ? pow((x-k)/(1.0-k),2.0) : 0.0;
}

vec3 renderhex(vec2 uv, vec2 p, float s, vec3 col){
    uv -= p;
    if (abs(uv.x) < 0.2*s && abs(uv.y) < 0.2*s){
        return mix(vec3(0),mix(vec3(0),col,0.1 + fpow(length(uv/s),0.1)*10.0),smoothstep(0.0,0.3,sdHex(uv*5.0/s)));
    }
    return vec3(0);
}

vec3 renderLensFlare(vec2 uv, vec2 light)
{
    vec3 col = vec3(0);
    //ghosts
    col += renderhex(uv, -light*0.25, 1.4, vec3(0.25,0.75,0));
    col += renderhex(uv, light*0.25, 0.5, vec3(1,0.5,0.5));
    col += renderhex(uv, light*0.1, 1.6, vec3(1,1,1));
    col += renderhex(uv, light*1.8, 2.0, vec3(0,0.5,0.75));
    col += renderhex(uv, light*1.25, 0.8, vec3(1,1,0.5));
    col += renderhex(uv, -light*1.25, 5.0, vec3(0.5,0.5,0.25));
    //circular ghost
    col += fpow(1.0 - abs(distance(light*0.8,uv) - 0.7),0.985)*vec3(0.1,0.05,0);
    //flare
    //col += vec3(1.0,0.6,0.4)*fpow(textureLod(iChannel0,normalize(light-uv)*0.25,0.0).r,0.3)*0.04/distance(light,uv);
    //bloom
    col += vec3(1.0,0.6,0.4)*fpow(max(1.0 - distance(light,uv),0.0),0.5);
    return col/(1.0 + distance(uv,light));
}

//
//
//

#define part2start 36.
#define part3start 42.

void main() 
{
    // bluesky, part3
    if (iTime<0) { iTime=-iTime; o=bskyFun(); return; }

    // part 1,stars+planet
    if ((iTime<32)||((iTime>=78)&&(iTime<200)))
    {
        float t = iTime*.0015; 
        vec3 col=doStarBackground(t);
        col=doPlanetX(t,col);
    }
    // part 2
    else
    {
        if (iTime>200) { iTime-=200; }

        vec2 q = gl_FragCoord.xy / ir;
        vec2 pl=-1.0 + 2.0 * q;
        if (iTime>=part2start) pl=-1.620 +2.725 * q;
        pl.x *= ir.x/ir.y;

        vec3 rols = vec3(.0,2.,2.);
        if (iTime>=part2start) rols= vec3(2.1,1.1,-2.);
        if (iTime>=part3start) rols= vec3(0.0,3.1,0.1);

        o=doLandscape(pl,q,rols);

        // ship
    
        vec2 m = vec2(0.5,0.68); // p1
        if (iTime>=part2start) m= vec2(-.44,.49);
        if (iTime>=part3start) m= vec2(0.,-0.85);

        vec3 ro = vec3(0, 17, -1.82);
        ro.yz *= rot(-m.y*3.14+1.);
        ro.xz *= -rot(-m.x*6.2831);    
        if ((iTime>=part2start)&&(iTime<part3start))
        {
            ro = vec3(0, 1, -1)*2.;
            ro.yz *= rot(-m.y*3.14+1.);
            ro.xz *= rot(-m.x*6.2831);
        }
        else if (iTime>=part3start) 
        {
            ro.y+=7.0;
        }
        ro.y+=0.2+(sin(iTime))*0.04;
        ro.z+=0.05*sin(iTime);
    
        for(int y=0; y<2; y++) 
        {
            vec2 offs = vec2(0, y)/float(2) -.5;

            vec2 uv = (gl_FragCoord.xy+offs-.5*ir)/ir.y;
            vec3 rd = R(uv, ro, vec3(0,0,0), 1.);

            if (iTime<part2start) rd.z+=(32.0-iTime)*0.2; // 1st part, ship floating forwards

            float dist = RayMarch(ro, rd).x;
            vec3 p = ro + rd * dist;

            if(dist<256) 
            {
                vec3 n = normalize(GetNormal(p));
                vec3 ref = reflect(rd,n);
                float diff = length(sin(ref * 6.) * .5 + .5);
                o=(pow((vec4(.7) * diff * .6 + pow(diff * .4, 5.) * 5.), vec4(.8))*clamp(0.5*n.y,.0,1.))*doLandscape(pl,q,(-n*0.1)-(ref*6.1))*2.44;
            }
        }

        //vec4 col;
        //doFlare(col,gl_FragCoord.xy);
        //o=(o*0.55+col*0.63);
        vec2 uvlf = (gl_FragCoord.xy-ir*.5)/ir.y*2;
        o=(o*0.95+vec4(renderLensFlare(uvlf,vec2(-0.98,0.6)),1));
    }

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
