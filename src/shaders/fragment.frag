#version 130
out vec4 o;
float ie=gl_TexCoord[0].x/1000;
vec2 ir=vec2(gl_TexCoord[0].y,gl_TexCoord[0].z),xy=gl_FragCoord.xy-.5*ir,ggg=xy+.5*ir;

//
// part 3
//

vec2 bskygrad( ivec2 z )
{
    int n=z.x+z.y*21111;
    n=n*n*(n<<13)^n;
    float g=(n*15+21+19)>>16;
    return vec2(cos(g),sin(g));
}

float newnoise(vec2 p)
{
    ivec2 i=ivec2(floor(p));
    vec2 f=fract(p),u=f*f*(3-2*f);
    return mix( mix( dot( bskygrad( i+ivec2(0,0) ), f-vec2(0.0,0.0) ), 
                     dot( bskygrad( i+ivec2(1,0) ), f-vec2(1.0,0.0) ), u.x),
                mix( dot( bskygrad( i+ivec2(0,1) ), f-vec2(0.0,1.0) ), 
                     dot( bskygrad( i+ivec2(1,1) ), f-vec2(1.0,1.0) ), u.x), u.y);
}

vec3 bskyFun()
{
    vec2 p = xy/ir.y;
    
    float f,w=0,i=0;
    for (;i<80;i++)
    {        
        vec2 fp = p*i*.1/(2*p.y+1.8);
        fp.x += ie*.5;
        mat2 m = mat2( 1.6,  1.2, -1.2,  1.6 );
        f  = .5*newnoise(fp); fp = m*fp*1.1;
		f += .25*newnoise(fp); fp*=m;
        f += .125*newnoise(fp); fp*=m;
        f = smoothstep(-.04,.7,f);
        w += f;
    }
    w /= 60;
    
    vec2 uv=xy/ir+.5;
    uv=ie<60?uv:ie<72?2*mod(uv,.25):2*mod(uv,.125);
    vec2 n = uv*(1. - uv) * 5;
    return pow(n.x*n.y,.5)*mix(vec3(smoothstep(-.11,1.1,w)),vec3(6.,2.,.5), smoothstep(.05,1.,w))*2*vec3(.82,1.5,2.5);
}

//
// part 1 stuff
//

// starback&planetxyz

#define rot(x) mat2(cos(x+vec4(0,33,11,0)))

void doStarBackgroundAndPlanetZ(vec2 I)
{
    // starback
    vec2 p,U=xy/ir.y;
    for(float i=0.,f; i++<12;
        o += (1e1-f)/max(length(p=mod((I+I-ir)/ir.y*f*
        mat2(rot(i)),3.)-1.)
        -vec4(.01),.01)/3e2)
        f = mod(i-ie*.1,10);

    // planetZ
    vec4 oo=o;
    I.y+=ie<32?15e2-ie*9:0;
    I -= o.xy = ir.xy*.5; // Center
    o = .8-sqrt(max(o-o,1.-dot(I/=ie<32?o.y*2:o.y*.5,I))); // Depth
	o = (1.-o)/3.2*min(o+I.x+I.y*.8, -.1) + // Lighting
        vec4(.5,.2,.7+I.x, 0)/dot(I*.5,I*1.3)*o; // Radiant light
    o*=ie<32?newnoise(U*1.7):1;
    
    o.rgb=ie<32?length(vec2(U.x,1.6+U.y-(ie*.007)))<1.05?o.rgb*.4:(.3*oo.rgb)+(.7*o.rgb):length(U)<.25?o.rgb:(.3*oo.rgb)+(.7*o.rgb);
}

//
// part 2-3
//

// new flare

float sdHex(vec2 p)
{
	vec2 q = vec2(p.x*2.0*0.6,p.y+p.x*0.6);
	return dot(step(q.xy,q.yx), 1-q.yx);
}

#define fpow(x,k) float(x > k ? pow((x-k)/(1.0-k),2) : 0)

vec3 renderhex(vec2 uv, vec2 p, float s, vec3 col){
    uv -= p;
    if (abs(uv.x) < 0.2*s && abs(uv.y) < 0.2*s){
        return mix(vec3(0),mix(vec3(0),col,0.1 + fpow(length(uv/s),0.1)*10.0),smoothstep(0.0,0.3,sdHex(abs(uv*5.0/s))));
    }
    return vec3(0);
}

// lscape

vec2 lscapefield(vec3 p) 
{
    vec3 l = vec3(1.2);
	float s=.2,e,f,n;
	for(e=f=p.y;s<8e2;s*=1.6)
            p.xz*=rot(s),
            e+=abs(dot(sin(p*s)/s,.3*l)),
            f+=abs(dot(sin(p.xz*s*.5)/s,l.xz));
	n = 1.+ (f>.01?e:-exp(-f*f));
    return vec2(max(n,0),min(f,max(e,.02)));
}

vec3 landScape(vec3 ro, vec3 rd )
{
    float t=2,dt=.1,c,f,i;
    vec3 col=vec3(0);
    for (i=0;i<80;i++)
    {                
        vec2 v = lscapefield(ro+t*rd);  
        c=v.x, f=v.y;
        t+=dt*f;
        dt*=1.03;
        col=.95*col+.09*vec3(c*c*c,c*c,c);	
    }
    return col;
}

void doLandscapeAndTri(vec2 p2,vec2 q2,vec3 rols,vec2 c)
{
    vec2 light=vec2(-1,.6),uv=xy/ir.y*2;
    vec3 ta=vec3(0),ww = normalize( ta - rols ),
    uu = normalize( cross(ww,vec3(0,1,0) ) ),
    vv = normalize( cross(uu,ww)),
    rd = normalize( p2.x*uu + p2.y*vv + 2.0*ww ),p,
    q=vec3(0,ie<36?36-ie:0,9),a,s=(vec4(c,0,1).rgb*2.-ir.xyx)/ir.x,
    col=
    renderhex(uv, -light*0.25, 1.4, vec3(0.25,0.75,0))+
    renderhex(uv, light*0.25, 0.5, vec3(1,.5,.5));//+renderhex(uv, light*0.1, 1.6, vec3(1,1,1));
    col+=fpow(1-abs(distance(light*.8,uv)-.7),.9)*vec3(.1,0.05,0);
    col+=vec3(.7)*fpow(max(1-distance(uv,light),0),.5);

    rols.z-=ie*.4;
    o.rgb=.55*(log(.35+landScape(rols,rd)));

    // flower
    float w,d,i=0;
    for(;i<80.;i++,q+=.5*s*d)
        for(d=w=1.+ie;w>ie;w-=1.2)
        {
            p=q;p.xz*=rot(w);p.xy*=rot(1);
            for(a=abs(p);a.z<a.x||a.z<a.y;p=p.zxy,a=a.zxy);
            d=min(d,length(p.xz-vec2(p.y,ie<42?1:.1)*a.y/p.z)*.7-.1);
        }
    o.bgr=d<.1?fwidth(q*5)*vec3(.1):o.bgr;

    // flare
    o.rgb+=col/(1+distance(uv,light));
}

//
// hexa
//

// st.IQ's hex
float hex(vec2 p, float r) 
{
  p.xy = p.yx;
  vec3 k = vec3(-sqrt(.75),.5,1.0/sqrt(3));
  p = abs(p);
  p -= 2*min(dot(k.xy,p),0.0)*k.xy;
  p -= vec2(clamp(p.x, -k.z*r, k.z*r), r);
  return length(p);
}

vec2 hextile(inout vec2 p) 
{
  vec2 sz = vec2(1,sqrt(3)),hsz = sz/2,p1 = mod(p, sz)-hsz,p2 = mod(p - hsz, sz)-hsz,p3 = dot(p1, p1) < dot(p2, p2) ? p1 : p2,n = ((p3 - p + hsz)/sz);
  p = p3;
  return n-vec2(.5);
}

vec3 hexTransition(vec2 p, vec3 from, vec3 to, float m) 
{
  m = clamp(m,0,1);
  float hz = .25;
  vec2 hp = 4*p;
  vec2 hn = hextile(hp)*hz*-vec2(-1., sqrt(3));
  float r = sdHex(hn+.25);
  
  float fi = smoothstep(0.0, 0.1, m);
  float fo = smoothstep(0.9, 1.0, m);

  float sz = .25+.25*tanh(((r+hn.x + hn.y-4+m*8)));
  
  float mm = smoothstep(0,0, -(hex(hp, sz)-1.*sz)*hz);
  mm = mix(0.0, mm, fi);
  mm = mix(mm, 1.0, fo);
  
  return mix(from, to, mm);
}

// triangle trans

float fEquilateralTriangle(vec2 p,float r )
{
    float k = sqrt(3);
    return -p.y-.5*k*max(abs(p.x)-k*p.y,0) + r*(1/k);
}

//
//
//

// #define fade(t,dt) smoothstep(0.,dt,abs(ie-t))

void main() 
{
    vec2 q=xy/ir+.5,pl=ie<36?-1+2*q:-1.62 +2.72*q;
    pl.x*=ir.x/ir.y;

    // part1/tri crossfade
    if ((ie>=28)&&(ie<32))
    {
        doStarBackgroundAndPlanetZ(ggg);

	    if (fEquilateralTriangle(pl,(ie-28)*2)>0)
        {
            doLandscapeAndTri(pl,q,vec3(0,2,2),ggg);
        }
    }
    // part 1/4,stars+planet
    else if ((ie<32)||(ie>=78))
    {
        doStarBackgroundAndPlanetZ(ggg);
    }
    else if ((ie>=54)&&(ie<78))
    {
        o.rgb=bskyFun();
    }
    // part 2
    else
    {
        doLandscapeAndTri(pl,q,ie<36?vec3(0,2,2):ie>=42?vec3(0,3.1,.1):vec3(2.1,1.1,-2),ggg);

        if ((ie>=50)&&(ie<54))
        {
            o.rgb = hexTransition(pl,o.rgb, bskyFun(), (ie-50)*.25);
        }
    }

    // fadein
    o=ie<3?o*(ie/3):o;
}
