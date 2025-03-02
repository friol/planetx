#version 130
out vec4 o;
#define ITER 70.

// ship

#define PI 3.141592
#define MAX_STEPS 300
#define MAX_DIST 30.
#define SURF_DIST .001

mat2 shipRot(float a) {
    float s = sin(a);
    float c = cos(a);
    return mat2(c, -s, s, c);
}

float rounding( in float d, in float h )
{
    return d - h;
}


float opUnion( float d1, float d2 )
{
    return min(d1,d2);
}


float opSmoothUnion( float d1, float d2, float k )
{
    float h = max(k-abs(d1-d2),0.0);
    return min(d1, d2) - h*h*0.25/k;
}

float sdCircle( in vec3 p, in float r )
{
	return length(p)-r;
}
float sdCappedCylinder( vec3 p, float h, float r )
{
  vec2 d = abs(vec2(length(p.xz),p.y)) - vec2(h,r);
  return min(max(d.x,d.y),0.0) + length(max(d,0.0));
}
float ndot(vec2 a, vec2 b ) { return a.x*b.x - a.y*b.y; }
float sdRhombus(vec3 p, float la, float lb, float h, float ra)
{
  p = abs(p);
  vec2 b = vec2(la,lb);
  float f = clamp( (ndot(b,b-2.0*p.xz))/dot(b,b), -1.0, 1.0 );
  vec2 q = vec2(length(p.xz-0.5*b*vec2(1.0-f,1.0+f))*sign(p.x*b.y+p.z*b.x-b.x*b.y)-ra, p.y-h);
  return min(max(q.x,q.y),0.0) + length(max(q,0.0));
}

float sdEllipsoid( vec3 p, vec3 r )
{
  float k0 = length(p/r);
  float k1 = length(p/(r*r));
  return k0*(k0-1.0)/k1;
}

float createReactor(vec3 p, float rad, float len){

	p = vec3(p.x, p.y, abs(p.z)-0.5);
	float reactor1 = sdCappedCylinder(p, rad-0.02, len);
	reactor1 = rounding(reactor1, 0.02);
	vec3 q = p;

	q += vec3(.0,-len,.0);
	float fire = sdCircle(q, 0.6 * rad);
	reactor1 = opUnion(reactor1, fire);
	return reactor1;
}


float map(in vec3 pos, out int material)
{
	//Reactor
	vec3 q = pos;

    //LinkBetweenReactors
	float link = sdRhombus(q, 0.53, 0.23, 0.015, 0.2 );
	//float link = opSmoothUnion(core, reactor, 0.1);
 
	//Core 
	q = pos + vec3(0.0,0.,-0.4);
	float core1 = sdEllipsoid(q, vec3(0.2,0.15,1.));
	link = opSmoothUnion(core1, link, 0.05);

    //Cockpit
	q = pos + vec3(0.0,-0.1,-0.12);
	float cockpit = sdEllipsoid(q, vec3(0.1,0.1,0.42));
	link = opUnion(cockpit, link);
	
	q = pos + vec3(0.75,0.0,0.);
	float leftengine = sdEllipsoid(q, vec3(0.1,0.1,0.5));
    link = opSmoothUnion(leftengine, link,0.05);

	q = pos + vec3(-0.75,0.0,0.);
	float rightengine = sdEllipsoid(q, vec3(0.1,0.1,0.5));
	link = opSmoothUnion(rightengine, link,0.05);

    return link;
}

vec2 RayMarch(vec3 ro, vec3 rd, out int mat) {
	float dO=0.;
    float dM=MAX_DIST;
    for(int i=0; i<MAX_STEPS; i++) {
    	vec3 p = ro + rd*dO;
        float dS = map(p,mat);
        if(dS<dM) dM = dS;
        dO += dS;
        if(dO>MAX_DIST || abs(dS)<SURF_DIST) break;
    }
    
    return vec2(dO, dM);
}

vec3 GetNormal(vec3 p) {
    int mat = 0;
	float d = map(p,mat);
    vec2 e = vec2(.001, 0);
    vec3 n = d - vec3(
        map(p-e.xyy,mat),
        map(p-e.yxy,mat),
        map(p-e.yyx,mat));
    
    return normalize(n);
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



//


vec2 grad( ivec2 z )
{
    int n = z.x+z.y*21111;
    n = (n<<13)^n;
    n = (n*(n*n*15+21)+19)>>16;
    return vec2(cos(float(n)),sin(float(n)));
}

float noise( in vec2 p )
{
    ivec2 i = ivec2(floor( p ));
     vec2 f =       fract( p );
	
	vec2 u = f*f*(3.0-2.0*f); // feel free to replace by a quintic smoothstep instead

    return mix( mix( dot( grad( i+ivec2(0,0) ), f-vec2(0.0,0.0) ), 
                     dot( grad( i+ivec2(1,0) ), f-vec2(1.0,0.0) ), u.x),
                mix( dot( grad( i+ivec2(0,1) ), f-vec2(0.0,1.0) ), 
                     dot( grad( i+ivec2(1,1) ), f-vec2(1.0,1.0) ), u.x), u.y);
}

vec2 hash22(vec2 p)
{
	vec3 p3 = fract(vec3(p.xyx) * vec3(.1031, .1030, .0973));
    p3 += dot(p3, p3.yzx+33.33);
    return fract((p3.xx+p3.yz)*p3.zy);
}

void main()
{
    float iTime = gl_TexCoord[0].x/1000;
    vec2 iResolution=vec2(gl_TexCoord[0].y,gl_TexCoord[0].z);
    vec2 p = (2.*gl_FragCoord.xy - iResolution.xy) / iResolution.y;
    
    vec3 c; float f,w = 0.; vec2 fp = vec2(0.);
    for(float i = 0.; i++ < ITER;) {        
      
        fp = 16. * p*i*.002/(p.y+1.8);
        fp.x += iTime*.5;

        mat2 m = mat2( 1.6,  1.2, -1.2,  1.6 );
        f  = 0.5000*noise( fp ); fp = m*fp*1.1;
		f += 0.2500*noise( fp ); fp = m*fp;
        f += 0.1250*noise( fp ); fp = m*fp;
        f = smoothstep(-.04,.7,f);
        w += f;

    }
    w /= ITER*.85;
    
    vec3 col = mix(
        vec3(smoothstep(-.11,1.1,w)),
        vec3(6.,2.,.5), smoothstep(.05,1.,w));  
 
    vec2 uv = gl_FragCoord.xy/iResolution.xy;
    vec2 n = uv*(1. - uv) * 6.;
    col *= pow(n.x*n.y,.5) + .5;
     
    o = vec4(col*2.,0)*vec4(0.82,1.5,2.5,1.0);
    
    //
    
    vec2 m = vec2(-.44,.49);//iMouse.xy/iResolution.xy;
    vec4 colz = vec4(0);
    
    vec3 ro = vec3(-1.2, 1.+(0.1*sin(iTime)), -1)*2.;
    ro.yz *= shipRot(-m.y*3.14+1.);
    ro.xz *= shipRot(-m.x*6.2831);
    ro.y = max(ro.y, -.9)-2.0;
    
    for(int y=0; y<2; y++) {
        vec2 offs = vec2(0, y)/float(2) -.5;

        vec2 uv = (gl_FragCoord.xy+offs-.5*iResolution.xy)/iResolution.y;
        uv.y+=.2;
        if (iTime<(48+2.0*PI)) uv.x-=-2.5+2.*sin(iTime/4.0);
        else uv.x+=0.5;

        vec3 rd = R(uv, ro, vec3(0,0,0), 1.);

        int mat = 0;
        float dist = RayMarch(ro, rd, mat).x;

        vec3 p = ro + rd * dist;
        vec3 f0;

        if(dist<MAX_DIST) {
            vec3 n = GetNormal(p);
            n=normalize(n);
            p=normalize(p)*dist;
            vec3 ref = reflect(p,n);
            float dif = clamp(0.5*n.y,.0,1.);
            float diff = length(sin(ref * 6.) * .5 + .5);
            float spec = pow(diff * .4, 5.) * 5.;
            vec4 alb = vec4(.7);
            colz = (alb * diff * .6 + spec) * 1.0;
            colz = pow(colz, vec4(.7))*dif*2.0*vec4(0.43,0.67,0.9,1.0)+w;
            o=colz;
        }
    }
}
