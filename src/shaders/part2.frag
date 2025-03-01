#version 130
out vec4 o;
#define nm normalize

#define PI 3.141592
#define MAX_STEPS 300
#define MAX_DIST 256.
#define SURF_DIST .001

mat2 shipRot(float a) 
{
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
	vec3 q = pos;

    //LinkBetweenReactors
	float link = sdRhombus(q, .53, 0.33, 0.015, 0.2 );
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
	float leftengine = sdEllipsoid(q, vec3(0.07,0.07,0.5));
    link = opSmoothUnion(leftengine, link,0.05);

	q = pos + vec3(-0.75,0.0,0.);
	float rightengine = sdEllipsoid(q, vec3(0.07,0.07,0.5));
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

// lscape

mat2 rot(float a) 
{
	return mat2(cos(a),sin(a),-sin(a),cos(a));	
}

vec3 l = vec3(1.);

vec2 field(in vec3 p) 
{
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
    float dt = .031;//+0.02*sin(iTime);
    vec3 col= vec3(0.);
    for( int i=0; i<80; i++ )
	{                
        vec2 v = field(ro+t*rd);  
        float c=v.x, f=v.y;
        t+=dt*f;
        dt *= 1.03;
        col = .95*col+ .09*vec3(c*c*c, c*c, c);	
    }
    
    return vec4(col,t);
}

vec4 doLandscape(vec2 p,vec2 q,vec3 rols)
{
    float iTime = gl_TexCoord[0].x/1000;
    vec3 ta = vec3( 0.0 , 0.0, 0.0 );
    vec3 ww = normalize( ta - rols );
    vec3 uu = normalize( cross(ww,vec3(0.0,1.0,0.0) ) );
    vec3 vv = normalize( cross(uu,ww));
    vec3 rd = normalize( p.x*uu + p.y*vv + 2.0*ww );
    rols.z -=iTime*.4;

    vec4 lss=landScape(rols,rd);
    vec3 lscapeCol = landScape(rols,rd).xyz;
	
    lscapeCol =  .75*(log(.25+lscapeCol));
    return vec4(lscapeCol,lss.w);
}

// lens flare


float rnd(vec2 p)
{
    float f = fract(sin(dot(p, vec2(12.1234, 72.8392) )*45123.2));
 return f;   
}

float rnd(float w)
{
    float f = fract(sin(w)*1000.);
 return f;   
}

float regShape(vec2 p, int N)
{
 float f;
    
    
float a=atan(p.x,p.y)+.2;
float b=6.28319/float(N);
f=smoothstep(.5,.51, cos(floor(.5+a/b)*b-a)*length(p.xy));
    
    
    return f;
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
    float s = max(0.01-pow(regShape(p*5. + mouse*dist*5. + 0.9, 6) , 1.), 0.0)*5.;
    
   	color = 0.5+0.5*sin(color);
    color = cos(vec3(0.44, .24, .2)*8. + dist*4.)*0.5+.5;
 	vec3 f = c*color ;
    f += c1*color;
    
    f += c2*color;  
    f +=  s*color;
    return f-0.01;
}

float sun(vec2 p, vec2 mouse)
{
 float f;
    
    vec2 sunp = p+mouse;
    float sun = 1.0-length(sunp)*8.;
    return f;
}

#define part3start 42.

void doFlare( out vec4 fragColor, in vec2 fragCoord )
{
    float iTime = gl_TexCoord[0].x/1000;
    vec2 iResolution=vec2(gl_TexCoord[0].y,gl_TexCoord[0].z);
	vec2 uv = fragCoord.xy / iResolution.xy-0.5;
    //uv=uv*2.-1.0;
    uv.x*=iResolution.x/iResolution.y;
    
    vec2 mm = vec2(-.6+.02*sin(iTime),.3+.02*sin(iTime));
    if (iTime>=part3start) mm = vec2(-.6+.02*sin(iTime),.6+.02*sin(iTime));
    
    vec3 circColor = vec3(0.9, 0.2, 0.1);
    vec3 circColor2 = vec3(0.3, 0.1, 0.9);
    
    vec3 color = mix(vec3(0.3, 0.2, 0.02)/0.9, vec3(0.2, 0.5, 0.8), uv.y)*3.-0.52*sin(iTime);
    
    //this calls the function which adds three circle types every time through the loop based on parameters I
    //got by trying things out. rnd i*2000. and rnd i*20 are just to help randomize things more
    for(float i=0.;i<10.;i++){
        color += circle(uv, pow(rnd(i*2000.)*1.8, 2.)+1.41, 0.0, circColor+i , circColor2+i, rnd(i*20.)*3.+0.2-.5, mm);
    }
    //get angle and length of the sun (uv - mouse)
        float a = atan(uv.y-mm.y, uv.x-mm.x);
    	float l = max(1.0-length(uv-mm)-0.84, 0.0);
    
    float bright = 0.1;//+0.1/abs(sin(iTime/3.))/3.;//add brightness based on how the sun moves so that it is brightest
    //when it is lined up with the center
    
    //add the sun with the frill things
    color += max(0.1/pow(length(uv-mm)*5., 5.), 0.0)*abs(sin(a*5.+cos(a*9.)))/20.;
    color += max(0.1/pow(length(uv-mm)*10., 1./20.), .0)+abs(sin(a*3.+cos(a*9.)))/8.*(abs(sin(a*9.)))/1.;
    //add another sun in the middle (to make it brighter)  with the20color I want, and bright as the numerator.
    color += (max(bright/pow(length(uv-mm)*4., 1./2.), 0.0)*4.)*vec3(0.2, 0.21, 0.3)*4.;
       // * (0.5+.5*sin(vec3(0.4, 0.2, 0.1) + vec3(a*2., 00., a*3.)+1.3));
    	
    //multiply by the exponetial e^x ? of 1.0-length which kind of masks the brightness more so that
    //there is a sharper roll of of the light decay from the sun. 
        color*= exp(1.0-length(uv-mm))/5.;
	fragColor = vec4(color,1.0);
}

//
//
//

#define part2start 36.

void main()
{
    float iTime = gl_TexCoord[0].x/1000;
    vec2 iResolution=vec2(gl_TexCoord[0].y,gl_TexCoord[0].z);

    vec2 q = gl_FragCoord.xy / iResolution.xy;
    vec2 pl=-1.0 + 2.0 * q;
    if (iTime>=part2start) pl=-1.620 +2.725 * q;
    pl.x *= iResolution.x/iResolution.y;

    vec3 rols = vec3(.0,2.,2.);
    if (iTime>=part2start) rols= vec3(2.1,1.1,-2.);
    if (iTime>=part3start) rols= vec3(0.0,3.1,0.1);

    vec4 lscapeCol=doLandscape(pl,q,rols);
    
    vec2 m = vec2(0.5,0.68); // p1
    if (iTime>=part2start) m= vec2(-.44,.49);
    if (iTime>=part3start) m= vec2(0.,-0.85);

    vec4 col;
    
    vec3 ro = vec3(0, 17, -1.82);
    ro.yz *= shipRot(-m.y*3.14+1.);
    ro.xz *= -shipRot(-m.x*6.2831);    
    if ((iTime>=part2start)&&(iTime<part3start))
    {
        ro = vec3(0, 1, -1)*2.;
        ro.yz *= shipRot(-m.y*3.14+1.);
        ro.xz *= shipRot(-m.x*6.2831);
    }
    else if (iTime>=part3start) 
    {
        ro.y+=7.0;
    }
    ro.y+=0.2+(sin(iTime))*0.04;
    ro.z+=0.05*sin(iTime);
    
    int ship=0;
    for(int y=0; y<2; y++) {
        vec2 offs = vec2(0, y)/float(2) -.5;

        vec2 uv = (gl_FragCoord.xy+offs-.5*iResolution.xy)/iResolution.y;
        vec3 rd = R(uv, ro, vec3(0,0,0), 1.);

        if (iTime<part2start) rd.z+=(32.0-iTime)*0.2; // 1st part

        int mat = 0;
        float dist = RayMarch(ro, rd, mat).x;

        vec3 p = ro + rd * dist;

        if(dist<MAX_DIST) {
            vec3 n = GetNormal(p);
            n=normalize(n);
            p=normalize(p);
            vec3 ref = reflect(rd,n);
            float dif = clamp(0.5*n.y,.0,1.);
            float diff = length(sin(ref * 6.) * .5 + .5);
            float spec = pow(diff * .4, 5.) * 5.;
            vec4 alb = vec4(.7);
            col = (alb * diff * .6 + spec) * 1.0;
            col = pow(col, vec4(.8))*dif*2.0;
            vec4 ls=doLandscape(pl,q,(-n*0.1)-(ref*6.1));
            col*=ls*1.22;
            o = col;
            ship=1;
        }
    }
    
    if (ship==0) 
    {
        o=lscapeCol;
    }

    vec4 flareCol;
    doFlare(flareCol,gl_FragCoord.xy);
    o=(o*0.55+flareCol*0.63);
}
