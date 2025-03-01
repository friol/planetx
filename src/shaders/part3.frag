#version 130
out vec4 o;
#define ITER 70.

vec2 grad( ivec2 z )  // replace this anything that returns a random vector
{
    // 2D to 1D  (feel free to replace by some other)
    int n = z.x+z.y*11111;

    // Hugo Elias hash (feel free to replace by another one)
    n = (n<<13)^n;
    n = (n*(n*n*15731+789221)+1376312589)>>16;

    // simple random vectors
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
    p.y -= .4;
    
    vec3 c; float f,w = 0.; vec2 fp = vec2(0.);
    for(float i = 0.; i++ < ITER;) {        
      
        fp = 20. * p*i*.004/(p.y+2.);
        fp.y -= iTime*.5;

        mat2 m = mat2( 1.6,  1.2, -1.2,  1.6 );
        f  = 0.5000*noise( fp ); fp = m*fp*1.1;
		f += 0.2500*noise( fp ); fp = m*fp;
        f += 0.1250*noise( fp ); fp = m*fp;
        f = smoothstep(-.04,.7,f);
        w += f;

    }
    w /= ITER*.75;
    
    vec3 col = mix(
        vec3(smoothstep(-.11,1.2,w)),
        vec3(6.,2.,.5), smoothstep(.05,1.,w));  
 
    vec2 uv = gl_FragCoord.xy/iResolution.xy;
    vec2 n = uv*(1. - uv) * 6.;
    col *= pow(n.x*n.y,.5) + .5;
     
    o = vec4(col*2.,0)*vec4(1.2,1.4,1.5,1.0);
}