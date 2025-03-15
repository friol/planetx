#version 130
out vec4 o;
float ie=gl_TexCoord[0].x/1000;
vec2 ir=vec2(gl_TexCoord[0].y,gl_TexCoord[0].z),xy=gl_FragCoord.xy-.5*ir,ggg=xy+.5*ir;

//
// part 3
//

vec2 bskygrad( ivec2 z )
{
    int n = z.x+z.y*21111;
    n=n*n*(n<<13)^n;
    float g=(n*15+21+19)>>16;
    return vec2(cos(g),sin(g));
}

float newnoise(vec2 p)
{
    ivec2 i = ivec2(floor(p));
    vec2 f=fract(p),u=f*f*(3-2*f);
    return mix( mix( dot( bskygrad( i+ivec2(0,0) ), f-vec2(0.0,0.0) ), 
                     dot( bskygrad( i+ivec2(1,0) ), f-vec2(1.0,0.0) ), u.x),
                mix( dot( bskygrad( i+ivec2(0,1) ), f-vec2(0.0,1.0) ), 
                     dot( bskygrad( i+ivec2(1,1) ), f-vec2(1.0,1.0) ), u.x), u.y);
}

vec3 bskyFun()
{
    vec2 p = xy/ir.y;
    
    float f,w,i;
    for (i=0;i<80;i++)
    {        
        vec2 fp = p*i*.1/(2*p.y+1.8);
        fp.x -= ie*.5;
        mat2 m = mat2( 1.6,  1.2, -1.2,  1.6 );
        f  = .5*newnoise(fp); fp = m*fp*1.1;
		f += .25*newnoise(fp); fp*=m;
        f += .125*newnoise(fp); fp*=m;
        f = smoothstep(-.04,.7,f);
        w += f;
    }
    w /= 59.5;
    
    vec2 uv=xy/ir+.5,n = uv*(1. - uv) * 6;
    vec3 col = pow(n.x*n.y,.5)*mix(vec3(smoothstep(-.11,1.1,w)),vec3(6.,2.,.5), smoothstep(.05,1.,w));
    return col*2*vec3(.82,1.5,2.5);
}

//
// part 1 stuff
//

// starback&planetxyz

//#define rot(a) mat2(cos(a),-sin(a),sin(a),cos(a))
#define rot(x) mat2(cos(x+vec4(0,33,11,0)))

void doStarBackgroundAndPlanetZ(vec2 I)
{
    // starback
    //vec2 p,r=ir,U=ie<32?vec2(0):xy/ir.y;
    vec2 p,r=ir,U=xy/ir.y;
    for(float i=0.,f; i++<12;
        o += (1e1-f)/max(length(p=mod((I+I-r)/r.y*f*
        mat2(rot(i)),3.)-1.)
        -vec4(5,6,9,0)/116e1,.01)/3e2)
        f = mod(i-ie*.1,1e1);

    // planetZ
    vec4 oo=o;
    I.y+=ie<32?15e2-ie*9:0;
    I -= o.xy = ir.xy*.5; // Center
    o = .8-sqrt(max(o-o,1.-dot(I/=ie<32?o.y*2:o.y*.5,I))); // Depth
	o = (1.-o)/3.2*min(o+I.x+I.y*.8, -.1) + // Lighting
        vec4(.5,.2,.7+I.x, 0) / dot(I*.5,I*1.3)*o; // Radiant light
    
    o.rgb=ie<32?length(vec2(U.x,1.6+U.y-(ie*.007)))<1.05?o.rgb*.4:(.5*oo.rgb)+(.5*o.rgb):length(U)<.25?o.rgb:(.5*oo.rgb)+(.5*o.rgb);
}

//
// part 2-3
//

// lscape

vec2 lscapefield(vec3 p) 
{
    vec3 l = vec3(1.2);
	float s=2.,e,f,o;
	for(e=f=p.y;s<8e2;s*=1.6)
            p.xz*=rot(s),
            e+=abs(dot(sin(p*s)/s,.3*l)),
            f+=abs(dot(sin(p.xz*s*.5)/s,l.xz));
	o = 1.+ (f>.01?e:-exp(-f*f));
    return vec2(max(o,0),min(f,max(e,.02)));
}

vec3 landScape( in vec3 ro, vec3 rd )
{
    float t=2,dt=.1,c,f;
    vec3 col;
    for (int i=0;i<80;i++)
    {                
        vec2 v = lscapefield(ro+t*rd);  
        c=v.x, f=v.y;
        t+=dt*f;
        dt*=1.03;
        col=.95*col+.09*vec3(c*c*c,c*c,c);	
    }
    return col;
}

vec4 doLandscape(vec2 p,vec2 q,vec3 rols)
{
    vec3 ta,ww = normalize( ta - rols ),uu = normalize( cross(ww,vec3(0,1,0) ) ),vv = normalize( cross(uu,ww)),rd = normalize( p.x*uu + p.y*vv + 2.0*ww );
    rols.z-=ie*.4;
    return vec4(.55*(log(.35+landScape(rols,rd))),1);
}

// new flare

float sdHex(vec2 p)
{
	vec2 q = vec2(p.x*2.0*0.57,p.y+p.x*0.57);
	return dot(step(q.xy,q.yx), 1-q.yx);
}

#define fpow(x,k) float(x > k ? pow((x-k)/(1.0-k),2.0) : 0.0)

vec3 renderhex(vec2 uv, vec2 p, float s, vec3 col){
    uv -= p;
    if (abs(uv.x) < 0.2*s && abs(uv.y) < 0.2*s){
        return mix(vec3(0),mix(vec3(0),col,0.1 + fpow(length(uv/s),0.1)*10.0),smoothstep(0.0,0.3,sdHex(abs(uv*5.0/s))));
    }
    return vec3(0);
}

vec3 renderLensFlare(vec2 uv, vec2 light)
{
    vec3 col=renderhex(uv, -light*0.25, 1.4, vec3(0.25,0.75,0))+
    renderhex(uv, light*0.25, 0.5, vec3(1,.5,.5))+
    renderhex(uv, light*0.1, 1.6, vec3(1,1,1));
    col += fpow(1-abs(distance(light*.8,uv)-.7),.9)*vec3(.1,0.05,0);
    col+=vec3(.7)*fpow(max(1-distance(uv,light),0),.5);
    return col/(1+distance(uv,light));
}

//

void doSpike(vec2 c)
{
    vec3 p,q=vec3(0,ie<36?36-ie:0,9),a,s=(vec4(c,0,1).rgb*2.-ir.xyx)/ir.x;
    float w,d,i=0.;
    for(;i<80.;i++,q+=.5*s*d)
        for(d=w=1.+ie;w>ie;w-=1.2)
        {
            p=q;p.xz*=rot(w);p.xy*=rot(1);
            for(a=abs(p);a.z<a.x||a.z<a.y;p=p.zxy,a=a.zxy);
            d=min(d,length(p.xz-vec2(p.y,ie<42?1:.1)*a.z/p.z)*.6-.1);
        }
    o.rgb=d<.1?fwidth(q*10.)*vec3(.4,.2,.1):o.rgb;
}

//
//
//

void main() 
{
    // bluesky, part3
    if (ie<0) { o=vec4(bskyFun(),1); return; }

    // part 1,stars+planet
    if ((ie<32)||((ie>=78)&&(ie<200)))
    {
        doStarBackgroundAndPlanetZ(ggg);
        //doPlanetZ(xy+.5*ir);
    }
    // part 2
    else
    {
        ie=ie>200?ie-200:ie;

        vec2 q=xy/ir+.5,pl=ie<36?-1+2*q:-1.62 +2.72*q;
        pl.x*=ir.x/ir.y;

        o=doLandscape(pl,q,ie<36?vec3(0,2,2):ie>=42?vec3(0,3.1,.1):vec3(2.1,1.1,-2));
        doSpike(ggg);
        o+=renderLensFlare(xy/ir.y*2,vec2(-1,.6)).xyzz;
    }

    // fadein
    o=ie<3?o*(ie/3):o;
}
