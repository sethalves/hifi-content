#version 3.7;

//
// +kfi0 +kff5
//

global_settings {
  assumed_gamma 1.0
}


#declare camloc = <0,0,0>;
camera {
 location camloc
 right x
 up y
 direction z

 angle 90

// #switch(frame_number)
// #case (0)
//   look_at camloc + x
//  #case(1)
//   look_at camloc + -x
//  #case(2)
//   look_at camloc + y
//  #case(3)
//   look_at camloc + -y
//  #case(4)
//   look_at camloc + z
//  #case(5)
//   look_at camloc + -z
//  #end

#if(frame_number=0)
    look_at camloc + x
#end

#if(frame_number=1)
    look_at camloc + -x
#end

#if(frame_number=2)
    look_at camloc + y
#end

#if(frame_number=3)
    look_at camloc + -y
#end

#if(frame_number=4)
    look_at camloc + z
#end

#if(frame_number=5)
    look_at camloc + -z
#end

}

sky_sphere{
    pigment{ gradient y
            color_map{
            [0.0 color rgb<1,1,1> ]
                [0.3 color rgb<0.18,0.28,0.75>*0.8]
                [1.0 color rgb<0.15,0.28,0.75>*0.5]}
        scale 1.05
            translate<0,-0.05,0>
            } // end pigment
}


#declare R_planet = 6000000;
#declare R_sky    = R_planet + 2000;
sphere{ <0, -R_planet, 0>, R_sky hollow
        texture{
        pigment{ bozo turbulence 0.75
                octaves 6  omega 0.7
                lambda 2  phase 0.15
                color_map {
                [0.00 color rgb <1,1,1>*0.95]
                    [0.05 color rgb <1,1,1>*1.25]
                    [0.15 color rgb <1,1,1>*0.85]
                    [0.55 color rgbt<1,1,1,1>]
                    [1.00 color rgbt<1,1,1,1>]
                    } // end color_map
            translate< 3, 0,-1>
                scale<0.3, 0.4, 0.2>*3
                } // end pigment
#if(version = 3.7)
        finish{emission 1 diffuse 0}
#else finish{ ambient 1 diffuse 0}
#end
        scale 3000
            } // end texture
    // no_shadow // optional!!
}
