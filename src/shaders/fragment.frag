#version 130
out vec4 o;
float iTime = gl_TexCoord[0].x/1000;
vec2 ir=vec2(gl_TexCoord[0].y,gl_TexCoord[0].z);
vec2 xy=gl_FragCoord.xy-.5*ir;

//
// part 3
//

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

vec3 bskyFun()
{
    vec2 p = xy/ir.y;
    
    float f,w,i;
    for (i=0;i <70;i++)
    {        
        vec2 fp = p*i*.1/(2*p.y+1.8);
        fp.x -= iTime*.5;
        mat2 m = mat2( 1.6,  1.2, -1.2,  1.6 );
        f  = .5*bskynoise(fp); fp = m*fp*1.1;
		f += .25*bskynoise(fp); fp*=m;
        f += .125*bskynoise(fp); fp*=m;
        f = smoothstep(-.04,.7,f);
        w += f;
    }
    w /= 59.5;
    
    vec2 uv=xy/ir+.5;
    vec2 n = uv*(1. - uv) * 6.;
    vec3 col = pow(n.x*n.y,.5)*mix(vec3(smoothstep(-.11,1.1,w)),vec3(6.,2.,.5), smoothstep(.05,1.,w));
    return col*2*vec3(0.82,1.5,2.5);
}

//
// part 1 stuff
//

vec3 fractalNebula(vec2 coord, vec3 color, float transparency)
{
    float n = 0.;
    float frequency = 1.;
    float amplitude = 1.;
    for (int o = 0; o < 5; ++o)
    {
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
    m += (max(0.01,.5-abs(uv.x*uv.y*1000.))*flare)*.1;
    return m*smoothstep(1., .1, d);
}

float Hash21(vec2 p)
{
    p = fract(p*vec2(123.3,456.2));
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
    vec2 uv = xy/ir.y;
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

// planetx
/*
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

vec3 doPlanetX(float t,vec3 col)
{
    vec3 src = vec3(2*xy/ir.yy,2);
    src.y+=3.1-(iTime/32.0);
    vec3 dir = vec3(0,0,-1);
    
    float ang = iTime*0.032;
    mat3 rot = mat3(-sin(ang),0.0,cos(ang),0.,1.,0.,cos(ang),0.,sin(ang));

    if (iTime>78) 
    {
        src = vec3(8. * xy/ ir.yy, 3.0);
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
*/
//

#define caz(z) exp(- 1./vec4(1,.6,.4,1) * .31/(z) )        // spectral transmittance for optical depth z
void doPlanetY()
{
    vec2 R = ir.xy,
         U = xy/R.y,
         M = R/R.x;
    if (iTime<78) U.y+=1.55-iTime/128.0;
    else U*=3;

    float H = .3,                                          // atmosphere thickness ralative to radius
          r = dot(U,U),
          d = dot(M,M),
          //t = -1.15*iTime,
          l = dot( vec3(U, sqrt(1.-r)),                    // hit point in Sun frame
                     //vec3(M,sqrt(abs(.2-d))*sign(1.-d))  // Sun direction
                     vec3(.01,.52,-.7)
                 );
   
    o = r < 1?( 1.- caz( sqrt(1.-r) )  )                  // atmosphere opacity along ray
      * smoothstep(-sqrt(2.*H),0.,l) * smoothstep(1.,1.-16./R.y,r) // up to terminator + AA
                + mix( l * vec4(.01,.5,1,0) ,               // ocean color
                       .5*caz(max(0.,l)),                   // Sun color seen by clouds and reflected to space
                       smoothstep(1.61,0.,abs(U.y))    // cloud layer
                     )
               //: r<1.2?o+abs(.2-length(.3+r-d))*vec4(0.2,0.4,1.0,1.0):o;  
               : o;
}

/*
const float planetSize                =   0.4;
const float planetShadeIntensity      =  16.0;
const float planetHighlightIntensity  =  32.0;

const vec3  atmColor                  =  vec3(0.3,0.42,1.0);
const vec2  atmOffset                 =  vec2(0, 0.009);
const float atmSize                   =   0.6;
const float atmTightness              =  15.0;
const float atmIntensity              =   2.4;
const float atmAmbientValue           =   0.15;

void doPlanetY(vec3 col)
{
    vec2 uv = gl_FragCoord.xy / ir;
    uv.y+=1.0-iTime/64;
    uv -= 0.5; // Center it
    float aspect = ir.x / ir.y;
    uv.x *= aspect; // Make it round
    
    float dist  = length(uv);
    float shape = smoothstep(planetSize + 0.01, planetSize, dist); // White round shape on a black backround
    vec3 pixel=vec3(0.2,0.4,.86)*0.1*shape;
    
    // Some shade for the planet
    float dist2 = length(uv - atmOffset * planetShadeIntensity);
    float shade = smoothstep(planetSize + 0.15, planetSize - 0.15, dist2) + atmAmbientValue;
    pixel *= shade;
    
    // A bit of highlight for the planet
    float dist3 = length(uv - atmOffset * -planetHighlightIntensity);
    float highlight = smoothstep(planetSize, planetSize + 0.35, dist3) * shape;
    
    // Atmosphere
    float dist4 = length(uv - atmOffset);
    float atmosphereShape = smoothstep(planetSize + atmSize, planetSize, dist4);
    atmosphereShape = pow(atmosphereShape, atmTightness) * atmIntensity;
    vec3 atmosphere = atmColor * atmosphereShape+0.2*bskynoise(uv*16*(dist4));
    
    vec3 combinedColor = mix(pixel, atmosphere, (1.0 - shape) * shade + highlight);
   
    if (dist<0.41) o= vec4(combinedColor, 1.0);
    else o.rgb=col*0.65+combinedColor*0.35;
}
*/

/*void doPlanetY(vec3 col)
{
    vec2 c=gl_FragCoord.xy;
    vec2 r = ir.xy,
         u = (c+c - r) / r.y;
    vec3 l = cos(iTime) * vec3(1, .4, tan(iTime)),
         n = vec3(u, sqrt(1. - dot(u, u))),
         d = max(dot(n, l), 0.) * vec3(0, 2, 4);

    if (length(u)>0.5) o.rgb=col;
    else o.rgb = 1. - exp(-(d*d + vec3(8, 2, 1) *
            pow(reflect(-l, n).z, 3.)) *
            smoothstep(.9,.5, length(u)));
}*/

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
//#define ndot(a,b) float(a.x*b.x - a.y*b.y)

float sdRhombus(vec3 p)
{
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
	float link = sdRhombus(abs(q));
 
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

#define rot(a) mat2(cos(a),-sin(a),sin(a),cos(a))

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

// new flare

float sdHex(vec2 p)
{
    p = abs(p);
	vec2 q = vec2(p.x*2.0*0.57,p.y+p.x*0.57);
	return dot(step(q.xy,q.yx), 1.0-q.yx);
}

#define fpow(x,k) float(x > k ? pow((x-k)/(1.0-k),2.0) : 0.0)

vec3 renderhex(vec2 uv, vec2 p, float s, vec3 col){
    uv -= p;
    if (abs(uv.x) < 0.2*s && abs(uv.y) < 0.2*s){
        return mix(vec3(0),mix(vec3(0),col,0.1 + fpow(length(uv/s),0.1)*10.0),smoothstep(0.0,0.3,sdHex(uv*5.0/s)));
    }
    return vec3(0);
}

vec3 renderLensFlare(vec2 uv, vec2 light)
{
    vec3 col;
    //ghosts
    col = renderhex(uv, -light*0.25, 1.4, vec3(0.25,0.75,0));
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
    col += vec3(1.0,0.6,0.4)*fpow(max(1.0 - distance(uv,light),0.0),0.5);
    return col/(1.0 + distance(uv,light));
}

//
//
//

void main() 
{
    // bluesky, part3
    if (iTime<0) { o=vec4(bskyFun(),1); return; }

    // part 1,stars+planet
    if ((iTime<32)||((iTime>=78)&&(iTime<200)))
    {
        o=vec4(doStarBackground(iTime*.0015),1);
        //col=doPlanetX(t,col);
        doPlanetY();
    }
    // part 2
    else
    {
        if (iTime>200) { iTime-=200; }

        vec2 q = gl_FragCoord.xy / ir;
        vec2 pl=-1.0 + 2.0 * q;
        if (iTime>=36) pl=-1.62 +2.725 * q;
        pl.x *= ir.x/ir.y;

        vec2 m = vec2(0.5,0.68); // p1
        vec3 rols = vec3(.0,2.,2.);
        if (iTime>=36) { rols= vec3(2.1,1.1,-2.); m=vec2(-.44,.49); }
        if (iTime>=42) { rols= vec3(0.0,3.1,0.1); m=vec2(0.,-0.85); }

        o=doLandscape(pl,q,rols);

        // ship
    
        vec3 ro = vec3(0, 17, -1.82);
        ro.yz *= rot(1-m.y*3.14);
        ro.xz *= -rot(-m.x*6.2);    
        if ((iTime>=36)&&(iTime<42))
        {
            ro = vec3(0, 1, -1)*2.;
            ro.yz *= rot(-m.y*3.14+1.);
            ro.xz *= rot(-m.x*6.2);
        }
        else if (iTime>=42) 
        {
            ro.y+=7.0;
        }
        //ro.y+=0.2+(sin(iTime))*0.04;
        //ro.z+=0.05*sin(iTime);
    
        vec3 rd = R((xy+vec2(0,0)-.5)/ir.y, ro, vec3(0,0,0), 1.);

        if (iTime<36) rd.z+=(32.0-iTime)*0.2; // 1st part, ship floating forwards

        float dist = RayMarch(ro, rd).x;
        if(dist<256) 
        {
            vec3 n = normalize(GetNormal(ro+rd*dist));
            vec3 ref = reflect(rd,n);
            float diff = length(sin(ref * 6.) * .5 + .5);
            o=(pow((vec4(.7) * diff * .6 + pow(diff * .4, 5.) * 5.), vec4(.8))*clamp(0.5*n.y,.0,1.))*doLandscape(pl,q,(-n*0.1)-(ref*6.1))*2.44;
        }

        //vec4 col;
        //doFlare(col,gl_FragCoord.xy);
        //o=(o*0.55+col*0.63);
        o=(o*0.95+vec4(renderLensFlare(xy/ir.y*2,vec2(-0.98,0.6)),1));
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
