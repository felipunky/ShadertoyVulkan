#version 450
#extension GL_ARB_separate_shader_objects : enable

layout( binding = 0 ) uniform UniformBufferObject
{

	vec2 iResolution;
	vec2 iStampResolution;
	vec2 iMove;
	float iSpeed;
	float iGlow;
	float iTransparency;
	float iTime;

} ubo;

layout(binding = 1) uniform sampler2D texSampler[2];

layout( location = 0 ) in vec3 fragColor;
layout( location = 0 ) out vec4 outColor;

#define EPS 0.001
#define FAR 30.0
#define MAX_ITER 1200
//#define SPEED 2.1
#define HASHSCALE1 .1031
#define PI acos( -1.0 )
#define Smooth

vec3 glow = vec3( 0 );

mat2 rot( float a )
{

    return mat2( cos( a ), -sin( a ),
                 sin( a ),  cos( a )
               );

}

float hash13(vec3 p3)
{
	p3  = fract(p3 * HASHSCALE1);
    p3 += dot(p3, p3.yzx + 19.19);
    return fract((p3.x + p3.y) * p3.z);
}

// power smooth min (k = 8);
float smin( float a, float b, float k )
{
    a = pow( a, k ); b = pow( b, k );
    return pow( (a*b)/(a+b), 1.0/k );
}

// Taken from cabibo's https://www.shadertoy.com/view/Ml2XDw
float smax( float a, float b, float k )
{
    return log( exp( k * a ) + exp( k * b ) ) / k;
}

vec3 turn( vec3 p )
{

    p.z -= ubo.iTime * ubo.iSpeed;
    p.x += 2.2 * sin( p.z * 0.4 ) + 1.5 * cos( p.z * 0.3 );
    p.y += 0.1 * cos( p.z * 0.7 );
    
    return p;

}

vec3 dip( vec3 p )
{

    p.y -= 0.25 + 0.25 * sin( ( p.x + p.z ) + ubo.iTime * ubo.iSpeed * 2.0 );
    p.z += 1.0 + ubo.iTime * ubo.iSpeed;
    //p.x = abs( p.x );
    //p.x -= 0.8;
    //p.z = mod( p.z, 1.2 ) - 0.6;
    
    return p;

}

float sdCylinder( vec3 p, vec2 h )
{
    
    vec2 d = abs( vec2( length( p.xz ), p.y ) ) - h;
    return min( max( d.x, d.y ), 0.0 ) + length( max( d, 0.0 ) );
    
}

float sdBox( vec3 p, vec3 b )
{
    
  	vec3 q = abs( p ) - b;
  	return length( max( q, 0.0 ) ) + min( max( q.x, max( q.y, q.z ) ), 0.0 );

}

float sdBox( vec2 p, vec2 b )
{
    
  	vec2 q = abs( p ) - b;
  	return length( max( q, 0.0 ) ) + min( max( q.x, q.y ), 0.0 );

}

float sp( vec3 p, float a )
{

	return length( p - vec3( 0, -0.7, 0 ) ) - a;

}

float tri( float amp, float dis, float fre, float spe, float dec )
{

    return amp * sin( fre * dis - ubo.iTime * spe ) / ( dec + dis * dis );
    
}

// Shane's
// Tri-Planar blending function. Based on an old Nvidia tutorial.
vec3 tex3D( sampler2D tex, in vec3 p, in vec3 n ){
    
    //return cellTileColor(p);
  
    n = max((abs(n) - 0.2)*7., 0.001); // n = max(abs(n), 0.001), etc.
    n /= (n.x + n.y + n.z ); 
	return (texture(tex, p.yz)*n.x + texture(tex, p.zx)*n.y + texture(tex, p.xy)*n.z).xyz;
}

// Texture bump mapping. Four tri-planar lookups, or 12 texture lookups in total. I tried to 
// make it as concise as possible. Whether that translates to speed, or not, I couldn't say.
vec3 texBump( sampler2D tx, in vec3 p, in vec3 n, float bf){
   
    const vec2 e = vec2(0.002, 0);
    
    //p.z += iTime * 0.2;
    // Three gradient vectors rolled into a matrix, constructed with offset greyscale texture values.    
    mat3 m = mat3( tex3D(tx, p - e.xyy, n), tex3D(tx, p - e.yxy, n), tex3D(tx, p - e.yyx, n));
    
    vec3 g = vec3(0.299, 0.587, 0.114)*m; // Converting to greyscale.
    g = (g - dot(tex3D(tx,  p , n), vec3(0.299, 0.587, 0.114)) )/e.x; g -= n*dot(n, g);
                      
    return normalize( n + g*bf ); // Bumped normal. "bf" - bump factor.
	
}

vec2 map( vec3 p )
{
    
    p = turn( p );
    //p.xy *= 1.2;
    
    float tu =  max( -( length( p.xy ) - 2.0 ), length( p.xy ) - 2.5 );
    
    vec3 pRoo = p;
    pRoo.y -= 2.0;
    float roo = length( pRoo.xy ) - 1.0;
    
    vec2 tun = vec2( max( -roo, tu ), 0.0 );
    
    pRoo.y -= 2.1;
    
    vec2 roof = vec2( length( pRoo.xy ) - 2.0, 1.0 );
    
    if( roof.x < tun.x )
    {
    
        tun = roof;
    
    }
    
    vec3 floP = dip( p );
    vec2 flo = vec2( smin( p.y + 0.8 + tri( 0.5, length( floP.xz ), 10.0, 3.0, 35.0 + 30.0 * sin( ( floP.x + floP.z ) - ubo.iTime * ubo.iSpeed ) ), sp( floP, 0.2 ), 0.2 ), 2.0 );
    //vec2 flo = vec2( smin( p.y + 0.8, sp( floP, 0.2 ), 0.2 ), 2.0 );
    
    if( tun.x < flo.x ) 
    {
    
        flo = tun;
    
    }
    
    vec3 pro = p;
    
    pro.y += 0.1;
    pro.zy *= rot( 1.5708 );
    
    pro.y = mod( pro.y, 2.0 ) - 1.0;
    
    pro.y = abs( pro.y );
    pro.y -= 0.1;
    pro.y = abs( pro.y );
    pro.y -= 0.025;
    
    pro.y = abs( pro.y );
    pro.y -= 0.1;
    pro.y = abs( pro.y );
    pro.y -= 0.025;
    
    pro.y = abs( pro.y );
    pro.y -= 0.1;
    pro.y = abs( pro.y );
    pro.y -= 0.025;
    
    pro.y = abs( pro.y );
    pro.y -= 0.1;
    pro.y = abs( pro.y );
    pro.y -= 0.025;
    
    pro.y = abs( pro.y );
    pro.y -= 0.1;
    pro.y = abs( pro.y );
    pro.y -= 0.025;
    
    vec3 boxP = pro;
    boxP.z += 0.45;
    boxP.x = abs( boxP.x );
    boxP.x -= 1.95;
    
    vec2 cyl = vec2( min( sdBox( boxP, vec3( 0.01, 0.005, 0.07 ) ), max( -p.y + 0.4, max( -sdCylinder( pro, vec2( 1.98, 0.01 ) ), sdCylinder( pro, vec2( 2.03, 0.005 ) ) ) ) ), 3.0 );
   
    
    if( cyl.x < flo.x )
    {
    
        flo = cyl;
    
    }
    
    vec3 cylP = p;
    cylP.y -= 2.1;
    cylP.x = abs( cylP.x );
    cylP.x -= 0.6;
    
    vec2 cylC = vec2( length( cylP.xy ) - 0.1, 4.0 );
    
    if( cylC.x < flo.x )
    {
    
        flo = cylC;
    
    }
    
    vec3 pbo = p;
    pbo.y += 0.7;
    pbo.x = abs( pbo.x );
    pbo.x -= 1.85;
    
    vec2 box = vec2( sdBox( pbo.xy, vec2( 0.1, 0.1 ) ) - 0.05, 5.0 );
    
    if( box.x < flo.x )
    {
    
        flo = box;
    
    }
    
    //float tileSize = 1.5;
    
    //vec3 id = vec3( int( p.x / tileSize ), int( p.y / tileSize ), int( p.z / tileSize ) );
    
    vec3 tub = p;
    tub.y += 0.29;
    vec3 tubO = tub;
    tub.x = abs( tub.x );
    tub.x -= 1.995;
    tubO.x = abs( tubO.x );
    tubO.x -= 2.11;
    tubO += 0.15;
    tub.y += 0.05 * cos( tub.z * 1.0 ) * sin( tub.z * 0.4 );
    tubO.y += 0.025 * cos( 20.0 + tubO.z * 1.0 ) * -sin( tubO.z * 0.6 );
    
    vec2 sid = vec2( min( length( tubO.xy ) - 0.024, length( tub.xy ) - 0.025 ), 6.0 );
    
    if( flo.x < sid.x )
    {
    
        sid = flo;
    
    }
    
    // This one is quite fun!
    //vec2 sph = vec2( flo.x - 0.004, 7.0 );
    //vec2 sph = vec2( smin( p.y + 0.801, sp( p, 0.201 ), 0.2 ), 7.0 );
    /*vec2 sph = vec2( sp( floP, 0.207 ), 7.0 );
    
    if( sph.x < sid.x )
    {
    
        sid = sph;
    
    }*/
    
    return sid;
    
}

vec3 norm( vec3 p )
{

    vec2 e = vec2( EPS, 0 );
    
    return normalize( 
           				vec3( 
                        		map( p + e.xyy ).x - map( p - e.xyy ).x,
                            	map( p + e.yxy ).x - map( p - e.yxy ).x,
                            	map( p + e.yyx ).x - map( p - e.yyx ).x
                            
                        	) 
    				);

}

float ray( vec3 ro, vec3 rd, out float d )
{

    float t = 0.0;
    
    for( int i = 0; i < MAX_ITER; ++i )
    {
    
        d = 0.5 * map( ro + rd * t ).x;
        
        if( d < EPS || t > FAR )
        {
        
            break;
        
        }
        
        t += d;
        glow += ubo.iGlow;
    
    }
    
    return t;

}

/*
float calcSoftshadow( in vec3 ro, in vec3 rd, in float mint, in float tmax, int technique )
{
	float res = 1.0;
    float t = mint;
    float ph = 1e10; // big, such that y = 0 on the first iteration
    
    for( int i=0; i<32; i++ )
    {
		float h = map( ro + rd*t ).x;

        // traditional technique
        if( technique==0 )
        {
        	res = min( res, 10.0*h/t );
        }
        // improved technique
        else
        {
            // use this if you are getting artifact on the first iteration, or unroll the
            // first iteration out of the loop
            //float y = (i==0) ? 0.0 : h*h/(2.0*ph); 

            float y = h*h/(2.0*ph);
            float d = sqrt(h*h-y*y);
            res = min( res, 10.0*d/max(0.0,t-y) );
            ph = h;
        }
        
        t += h;
        
        if( res<0.0001 || t>tmax ) break;
        
    }
    return clamp( res, 0.0, 1.0 );
}
*/

vec3 sha( vec3 ro, vec3 rd, float t, vec2 d )
{

    vec3 p = ro + rd * t;
    vec3 n = norm( p );
    
    vec3 col = vec3( 0 );
    
    if( map( p ).y == 0.0 ) 
    {
    
    	n = texBump( texSampler[0], turn( p ), n, 0.005 );
        col += 0.1;
    
    }
    
    vec3 ref = reflect( rd, n );
    
    vec3 lig = ro - vec3( 0.1, 0.5, -1.4 );
    lig.x -= turn( lig ).x;
    lig = normalize( lig );
    //lig.x -= turn( lig ).x;
    //lig = normalize( lig );
    vec3 blig = -lig;
    
    //vec2 d = map( p );
    
    
    //float amb = 0.5 + 0.5 * n.y; 
    float dif = max( 0.0, dot( n, lig ) );
    float bac = max( 0.0, dot( n, blig ) );
    
    float spe = pow( clamp( dot( lig, ref ), 0.0, 1.0 ), 4.0 ); 
    //float sha = calcSoftshadow( p, lig, 0.01, 3.0, 1 );
    
    // Taken from tekf's awesome KIFS https://www.shadertoy.com/view/lssSWM.
    vec3 ambient = mix( vec3( 0.03, 0.05, 0.08 ), vec3( 0.1 ), ( -n.y + 1.0 ) ); // ambient
    // ambient occlusion, based on my DF Lighting: https://www.shadertoy.com/view/XdBGW3
	float aoRange = t / 20.0; 
	float occlusion = max( 0.0, 1.0 - map( p + n * aoRange ).x / aoRange ); // can be > 1.0
	occlusion = exp2( -3.0 * pow( occlusion, 2.0 ) ); // tweak the curve
	ambient *= occlusion;
    
    col += vec3( 0.2, 0.2, 0.3 ) * ambient;
    //col += vec3( 0.2, 0.2, 0.3 ) * amb;
    col += vec3( 0.4, 0.4, 0.2 ) * spe;
    col += vec3( 0.2, 0.1, 0.1 ) * dif;
    col += vec3( 0.1, 0.1, 0.2 ) * bac;
    
    //col += vec3( 0.4, 0.4, 0.2 ) * spe;
    
    //col += 0.6 * texture( iChannel0, ref ).rgb;
    
    /*
    if( d.y == 0.0 )
    {
    
        col += tex3D( iChannel1, p, n );
    
    }
    
    
    if( d.y == 1.0 )
    {
    
        col *= vec3( 10 );
    
    }
    
    if( d.y == 2.0 )
    {
    
        col *= vec3( 0, 1, 1 );
    
    }
    
    if( d.y == 3.0 )
    {
    
        col *= vec3( 0, 1, 0 );
    
    }
    
    if( d.y == 4.0 )
    {
    
        col *= vec3( 0, 0, 1 );
    
    }*/
    
    if( d.y == 1.0 )
    {
    
        col += vec3( 0.2, 0.14, 0.1 ) * ( 0.5 + 0.5 * -n.y ) * occlusion;
        col += 2.0 * spe;
    
    }
    
    if( d.y == 2.0 )
    {
    
        col *= vec3( 0.5, 0.45, 0.4 );
    
    }
    
    /*if( d.y == 3.0 )
    {
    
        col *= 0.5;
    
    }*/
    
    //col *= 0.5 + t * t * 0.3;
    col = mix( col, vec3( 0.9, 0.88, 0.8 ), t * 0.08 );
    
    col += glow;
    
    return pow( col, vec3( 0.4545 ) );

}

void main()
{ 

	vec2 fragCoord = vec2( gl_FragCoord.x, ubo.iResolution.y - gl_FragCoord.y );
	vec2 iResolution = vec2( ubo.iResolution.x, ubo.iResolution.y );
	
	vec2 uv = ( -iResolution.xy + 2.0 * fragCoord.xy ) / iResolution.y;

    vec3 ro = vec3( 0, -0.3, 0 );
    vec3 rd = normalize( vec3( uv, -1 ) );
    vec3 tur = turn( ro );
    ro.x -= tur.x;
    //ro.zy *= rot( iMouse.y * 0.05 );
    //rd.zy *= rot( iMouse.y * 0.05 );
    rd.xz *= rot( tur.x * 0.2 );
    
    float d = 0.0, t = ray( ro, rd, d );
    vec3 p = ro + rd * t;
    vec3 n = norm( p );
    
    vec2 m = map( p );
    
    vec3 shad = d < EPS ? sha( ro, rd, t, m ) : vec3( 1 );
    
    if( ( m.y == 2.0 || m.y == 3.0 || m.y == 4.0 || m.y == 6.0 ) && d < EPS )
    {

    	rd = normalize( reflect( rd, n ) );
   	 	ro = p + rd * EPS;

        shad = sha( ro, rd, t, m );
        
    }

	outColor = vec4( shad, 1 );

}