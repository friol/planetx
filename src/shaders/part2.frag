#version 130
out vec4 o;
#define nm normalize
#define glf gl_FragCoord
#define ss smoothstep
#define lt length

float iTime = gl_TexCoord[0].x/1000;
vec2 iResolution=vec2(gl_TexCoord[0].y,gl_TexCoord[0].z);

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
    vec2 f =       fract( p );
	vec2 u = f*f*(3.0-2.0*f);
    return mix( mix( dot( bskygrad( i+ivec2(0,0) ), f-vec2(0.0,0.0) ), 
                     dot( bskygrad( i+ivec2(1,0) ), f-vec2(1.0,0.0) ), u.x),
                mix( dot( bskygrad( i+ivec2(0,1) ), f-vec2(0.0,1.0) ), 
                     dot( bskygrad( i+ivec2(1,1) ), f-vec2(1.0,1.0) ), u.x), u.y);
}

vec4 bskyFun()
{
    vec2 p = (2.*glf.xy - iResolution.xy) / iResolution.y;
    
    vec3 c; float f,w; vec2 fp;
    for(float i = 0.; i < 70.;i++) {        
      
        fp = 16. * p*i*.002/(p.y+1.8);
        fp.x += iTime*.5;

        mat2 m = mat2( 1.6,  1.2, -1.2,  1.6 );
        f  = .5*bskynoise(fp); fp = m*fp*1.1;
		f += .25*bskynoise(fp); fp*=m;
        f += .125*bskynoise(fp); fp*=m;
        f = ss(-.04,.7,f);
        w += f;
    }
    w /= 59.5;
    
    vec3 col = mix(
        vec3(ss(-.11,1.1,w)),
        vec3(6.,2.,.5), ss(.05,1.,w));  
 
    vec2 uv = glf.xy/iResolution.xy;
    vec2 n = uv*(1. - uv) * 6.;
    col *= pow(n.x*n.y,.5);
    return vec4(col*2.,0)*vec4(0.82,1.5,2.5,1.0);
}


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
  vec2 q = vec2(lt(p.xz-0.5*b*vec2(1.0-f,1.0+f))*sign(p.x*b.y+p.z*b.x-b.x*b.y)-.2, p.y-.015);
  return min(max(q.x,q.y),0.0) + lt(max(q,0.0));
}

float sdEllipsoid( vec3 p, vec3 r )
{
  float k0 = lt(p/r);
  float k1 = lt(p/(r*r));
  return k0*(k0-1.0)/k1;
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
    vec3 n = map(p) - vec3(
        map(p-e.xyy),
        map(p-e.yxy),
        map(p-e.yyx));
    return nm(n);
}

vec3 R(vec2 uv, vec3 p, vec3 l, float z) {
    vec3 f = nm(l-p),
        r = nm(cross(vec3(0,1,0), f)),
        u = cross(f,r),
        c = p+f*z,
        i = c + uv.x*r + uv.y*u,
        d = nm(i-p);
    return d;
}

// lscape

mat2 rot(float a) 
{
	return mat2(cos(a),-sin(a),sin(a),cos(a));	
}

vec2 field(in vec3 p) 
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
        vec2 v = field(ro+t*rd);  
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
    vec3 ww = nm( ta - rols );
    vec3 uu = nm( cross(ww,vec3(0,1,0) ) );
    vec3 vv = nm( cross(uu,ww));
    vec3 rd = nm( p.x*uu + p.y*vv + 2.0*ww );
    rols.z -=iTime*.4;

    vec4 lss=landScape(rols,rd);
    vec3 lscapeCol = landScape(rols,rd).xyz;
	
    lscapeCol =  .75*(log(.25+lscapeCol));
    return vec4(lscapeCol,lss.w);
}

// lens flare

float rnd(float w)
{
    return fract(sin(w)*1000.);
}

float regShape(vec2 p,float N)
{
    float a=atan(p.x,p.y)+.2;
    float b=6.2/N;
    return ss(.5,.51, cos(floor(.5+a/b)*b-a)*lt(p.xy));
}

vec3 circle(vec2 p, float size, float decay, vec3 color,vec3 color2, float dist, vec2 mouse)
{
    float l = lt(p + mouse*(dist*4.))+size/2.;
    
    //l2 is used in the rings as well...somehow...
    float l2 = lt(p + mouse*(dist*4.))+size/3.;
    
    ///these are circles, big, rings, and  tiny respectively
    float c = max(0.01-pow(lt(p + mouse*dist), size*1.4), 0.0)*50.;
    float c1 = max(0.001-pow(l-0.3, 1./40.)+sin(l*30.), 0.0)*3.;
    float c2 =  max(0.04/pow(lt(p-mouse*dist/2. + 0.09)*1., 1.), 0.0)/20.;
    float s = max(0.01-pow(regShape(p*5. + mouse*dist*5. + 0.9, 6),1), 0.0)*5.;
    
   	color = .5*sin(color);
    color = cos(vec3(0.44, .24, .2)*8. + dist*4.)*0.5+1;
 	return c*color + c1*color+ c2*color+ s*color-0.01;
}

#define part3start 42.

void doFlare( out vec4 fragColor, vec2 fragCoord )
{
	vec2 uv = fragCoord.xy / iResolution.xy-0.5;
    uv.x*=iResolution.x/iResolution.y;
    
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
    float l = max(1.0-lt(uv-mm)-0.84, 0.0);
    
    //add the sun with the frill things
    color += max(0.1/pow(lt(uv-mm)*5., 5.), 0.0)*abs(sin(a*5.+cos(a*9.)))/20.;
    color += max(0.1/pow(lt(uv-mm)*10., 1./20.), .0)+abs(sin(a*3.+cos(a*9.)))/8.*(abs(sin(a*9.)))/1.;
    //add another sun in the middle (to make it brighter)  with the20color I want, and bright as the numerator.
    color += (max(.1/pow(lt(uv-mm)*4., 1./2.), 0.0)*4.)*vec3(0.8, 0.84, 1.2);
    	
    //multiply by the exponetial e^x ? of 1.0-lt which kind of masks the brightness more so that
    //there is a sharper roll of of the light decay from the sun. 
    color*= exp(1.0-lt(uv-mm))/5.;
	fragColor = vec4(color,1.0);
}

//
//
//

#define part2start 36.

void main()
{
    // bluesky
    if (iTime<0) { iTime=-iTime; o=bskyFun(); return; }

    vec2 q = glf.xy / iResolution.xy;
    vec2 pl=-1.0 + 2.0 * q;
    if (iTime>=part2start) pl=-1.620 +2.725 * q;
    pl.x *= iResolution.x/iResolution.y;

    vec3 rols = vec3(.0,2.,2.);
    if (iTime>=part2start) rols= vec3(2.1,1.1,-2.);
    if (iTime>=part3start) rols= vec3(0.0,3.1,0.1);

    o=doLandscape(pl,q,rols);

    //
    
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
    
    for(int y=0; y<2; y++) {
        vec2 offs = vec2(0, y)/float(2) -.5;

        vec2 uv = (glf.xy+offs-.5*iResolution.xy)/iResolution.y;
        vec3 rd = R(uv, ro, vec3(0,0,0), 1.);

        if (iTime<part2start) rd.z+=(32.0-iTime)*0.2; // 1st part

        float dist = RayMarch(ro, rd).x;
        vec3 p = ro + rd * dist;

        if(dist<256) 
        {
            vec3 n = nm(GetNormal(p));
            vec3 ref = reflect(rd,n);
            float diff = lt(sin(ref * 6.) * .5 + .5);
            o=(pow((vec4(.7) * diff * .6 + pow(diff * .4, 5.) * 5.), vec4(.8))*clamp(0.5*n.y,.0,1.))*doLandscape(pl,q,(-n*0.1)-(ref*6.1))*2.44;
        }
    }

    vec4 col;
    doFlare(col,glf.xy);
    o=(o*0.55+col*0.63);
}
