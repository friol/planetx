#version 430

//uniform int iTime,xrez,yrez;

layout (location = 0) uniform int iTime;
layout (location = 1) uniform int xrez;
layout (location = 2) uniform int yrez;

out vec4 o;
float ie;
vec2 ir,xy,ggg;

//
// part 3
//

vec2 bskygrad(ivec2 z)
{
    int n=z.x+z.y*21111;
    n=n*n*(n<<13)^n;
    float g=(n*15+40)>>16;
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

vec3 sphereHelper(vec3 O,vec2 I,int ver)
{
    float i=0,z;
    for(;i<1;i+=.005)
    {
        vec2 v=ir.xy,p=(I-v)/v.y*i;
        z=max((ver==1?.05:1)-dot(p,p),0);
        
        p/=.2+sqrt(z);
        p+=ie*(ver>0?.2:.1);

        v=abs(fract(p*newnoise(p*sin(ie*.01)))-.5)*newnoise(p*3);
        vec3 spherecol=ie<60?vec3(1,2,4):ie<66?vec3(4,2,1):ie<72?vec3(2,2,5):vec3(2,3,1);
        O+=spherecol/(ver==1?5e2:4e3)*z/(abs(max(v.x*.5+v,v).y-.01)+.1-i*.1);
    }

    O=clamp(O,0,2);
    return ver>0?O:tanh(O*O);
}

//
// part 1 stuff
//

// starback&planetxyz

#define rot(x) mat2(cos(x+vec4(0,33,11,0)))
//#define circle(uv,blur,a) smoothstep(0., blur, a - length(uv))

float circle(vec2 uv,float blur,float a) 
{
    return smoothstep(0,blur,a-length(uv));
}

void doStarBackgroundAndPlanetZ(vec2 I)
{
    // starback
    //vec4 oo=vec4(0);
    vec2 p,U=xy/ir.y;
    for(float i=0,f=0;i++<12;
        o+=(9-f)/max(length(p=mod((I+I-ir)/ir.y*f*mat2(rot(i)),2)-1)-vec4(.01),.01)/3e2)
        f = mod(i-ie*.1,10);
    /*for(float i=0.,f; i++<12;
            o+= (1e1-f)/max(length(p=mod((I+I-ir)/ir.y*f*
            mat2(rot(i)),3.)-1.)
            -vec4(.01),.01)/3e2)
            f = mod(i-ie*.1,10);*/

    // planetZ

    U.y-=ie<32?-2.1+ie*.01:0;
    float a=ie<32?4:1,c = circle(vec2(U*2.4),1.,a)*1.1-circle(vec2(U*2.8),.7,a)*.2;
    c-=circle(vec2(U.x - sin(3)*.8,1.8*U.y-cos(3)*.6)*.8,1,a)*.1;
    vec3 col = vec3(c) * vec3(1., 0., 0.5)+vec3(smoothstep(.1,.7,c))*vec3(1,1,0)+vec3(smoothstep(.4,.5,c));
    o.rgb=col+o.rgb*.2+(o.rgb*vec3(0.7*newnoise(U*1.1),0.0*newnoise(U*1.7),0.9*newnoise(U*2.1)));
}

//
// part 2-3
//

// new flare

float sdHex(vec2 p)
{
	vec2 q = vec2(p.x*2*.6,p.y+p.x*.6);
	return dot(step(q.xy,q.yx), 1-q.yx);
}

#define fpow(x,k) float(x>k?pow((x-k)/(1-k),2):0)

vec3 renderhex(vec2 uv, vec2 p, float s, vec3 col)
{
    uv -= p;
    if (abs(uv.x)<.2*s&&abs(uv.y)<.2*s) return mix(vec3(0),mix(vec3(0),col,.1 + fpow(length(uv/s),.1)*10),smoothstep(0.0,0.3,sdHex(abs(uv*5.0/s))));
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
    for (i=0;i++<80;)
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
    vec2 light=ie<36?vec2(0,-.8+(ie-32)*.2):vec2(0),
    uv=xy/ir.y*2;
    vec3 ta=vec3(0),ww = normalize( ta - rols ),
    uu = normalize( cross(ww,vec3(0,1,0) ) ),
    vv = normalize( cross(uu,ww)),
    rd = normalize( p2.x*uu + p2.y*vv + 2.0*ww ),p,
    q=vec3(0,ie<36?36-ie:0,9),a,s=(vec4(c,0,1).rgb*2.-ir.xyx)/ir.x,
    col=
    renderhex(uv, -light*.25, 1.4, vec3(.25,.75,0))+
    renderhex(uv, light*.25, .5, vec3(1,.5,.5));//+renderhex(uv, light*0.1, 1.6, vec3(1,1,1));
    col+=fpow(1-abs(distance(light*.8,uv)-.7),.9)*vec3(.1,0.05,0);
    col+=vec3(.7)*fpow(max(1-distance(uv,light),0),.5);

    rols.z-=ie*.4;
    o.rgb=.55*(log(.35+landScape(rols,rd)));

    c.y+=ie<36?650-(ie-28)*75:0;

    // sphere+flare
    o.rgb=sphereHelper(o.rgb,2*c,1)+col/(1+distance(uv,light));
}

//
// hexa
//

// st.IQ's hex
float hex(vec2 p, float r) 
{
  p.xy = p.yx;
  vec3 k = vec3(-.86,.5,1/sqrt(3));
  p = abs(p);
  p -= 2*min(dot(k.xy,p),0)*k.xy;
  p -= vec2(clamp(p.x, -k.z*r, k.z*r), r);
  return length(p);
}

vec2 hextile(inout vec2 p) 
{
  vec2 sz = vec2(1,sqrt(3)),hsz = sz/2,p1 = mod(p, sz)-hsz,p2 = mod(p - hsz, sz)-hsz,p3 = dot(p1, p1) < dot(p2, p2) ? p1 : p2,n = ((p3 - p + hsz)/sz);
  p = p3;
  return n-vec2(.5);
}

vec3 hexTransition(vec2 p,float m) 
{
  vec2 hp = 4*p,hn = hextile(hp)*.25*-vec2(-1., sqrt(3));
  float r = sdHex(hn+.25),sz=.25+.25*tanh(((r+hn.x + hn.y-4+m*8))),mm = smoothstep(0,0, -(hex(hp, sz)-sz)*.25);
  mm = mix(0, mm, smoothstep(0.0, 0.1, m));
  mm = mix(mm, 1, smoothstep(0.9, 1.0, m));
  return mix(o.rgb, sphereHelper(vec3(0),ggg,0), mm);
}

// triangle trans

float fEquilateralTriangle(vec2 p,float r )
{
    return r*(1/sqrt(3))-p.y-.5*sqrt(3)*max(abs(p.x)-sqrt(3)*p.y,0);
}

//
//
//

// #define fade(t,dt) smoothstep(0.,dt,abs(ie-t))

void main() 
{
    ie=iTime/1e3;
    ir=vec2(xrez,yrez);
    xy=gl_FragCoord.xy-.5*ir;
    ggg=xy+.5*ir;
    
    o=vec4(0);

    vec2 q=xy/ir+.5,pl=ie<36?-1+2*q:-1.62 +2.72*q;
    pl.x*=ir.x/ir.y;

    if (ie<28)
    {
        doStarBackgroundAndPlanetZ(ggg);
    }
    else if (ie<32)
    {
        doStarBackgroundAndPlanetZ(ggg);

	    if (fEquilateralTriangle(pl,(ie-28)*2)>0)
        {
            doLandscapeAndTri(pl,q,vec3(0,2,2),ggg);
        }
    }
    else if (ie<54)
    {
        doLandscapeAndTri(pl,q,ie<36?vec3(0,2,2):ie<42?vec3(2,1,-2):ie<48?vec3(-3,.2,1):vec3(0,3,.1),ggg);

        if ((ie>=50)&&(ie<54))
        {
            o.rgb = hexTransition(pl,(ie-50)*.25);
        }
    }
    else if (ie<78)
    {
        o.rgb=sphereHelper(vec3(0),ggg,0);
    }
    else
    {
        doStarBackgroundAndPlanetZ(ggg);
    }

    // fadein
    o=ie<3?o*(ie/3):ie>=86?o*smoothstep(1,0,abs(ie-86)):o;
}
