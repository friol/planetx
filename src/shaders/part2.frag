#version 130
out vec4 o;
#define nm normalize

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

vec3 raycast( in vec3 ro, vec3 rd )
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
    
    return col;
}

void main()
{
    float iTime = gl_TexCoord[0].x/1000;
    vec2 iResolution=vec2(gl_TexCoord[0].y,gl_TexCoord[0].z);

	float t = iTime;
    vec2 q = gl_FragCoord.xy / iResolution.xy;
    vec2 p = -1.0 + 2.0 * q;
    p.x *= iResolution.x/iResolution.y;

    // camera

    vec3 ro = vec3(.0,2.,2.);
   
    vec3 ta = vec3( 0.0 , 0.0, 0.0 );
    vec3 ww = nm( ta - ro );
    vec3 uu = nm( cross(ww,vec3(0.0,1.0,0.0) ) );
    vec3 vv = nm( cross(uu,ww));
    vec3 rd = nm( p.x*uu + p.y*vv + 2.0*ww );
    ro.z -=t*1.5;

	// raymarch 
    vec3 col = raycast(ro,rd);
	
	// shade
    col =  .75*(log(.25+col));
    o = vec4( col, 1.0 );
}
