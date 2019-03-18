#version 3.7;

#declare camloc = <0,1,-3>;
camera {
 location camloc
 right x
 up y
 direction z

 angle 90

#if(frame_number=0)
    look_at camloc + y
#end

#if(frame_number=1)
    look_at camloc + -x
#end

#if(frame_number=2)
    look_at camloc + z
#end

#if(frame_number=3)
    look_at camloc + x
#end

#if(frame_number=4)
    look_at camloc + -z
#end

#if(frame_number=5)
    look_at camloc + -y
#end

}





// POV-Ray version 3.6/3.7 scenery file "p_sky05.pov"
// author: Friedrich A. Lohmueller, 2005, update Dec-2009 / Jan-2011 / Nov-2013
// homepage: http://www.f-lohmueller.de
//-----------------------------------------------------------------------------
#version 3.7; // 3.6;

global_settings{ assumed_gamma 1.0 }

#default{ finish{ ambient 0.1 diffuse 0.9 }}


//-----------------------------------------------------------------------------
global_settings { noise_generator 1 }


#include "colors.inc"
#include "textures.inc"

sky_sphere {
    pigment {
        crackle form <1,1,0>
        color_map {
            [.4 rgb 10]
            [.5 rgb 0]
        }
        scale .002
    }
}


// #include "stars.inc"
// sphere { <0,0,0>, 1
//     texture { Starfield6 //  1, 2, ... , 6
//     } // end of texture
//
//     scale 10000
// } //end of sphere



//---------------------------------------------------

// fog at the horizon
fog{fog_type   2
    distance   100
    color      rgb<1,1,1>*0.05
    fog_offset 0.1
    fog_alt    5
    turbulence 0.8}

//----------------------------------------------------


// ground
plane { <0,1,0>, 0
        texture{ pigment{color rgb<0.35,0.65,0.0>*0.7}
             normal {bumps 0.75 scale 0.015}
               } // end of texture
      } // end of plane
//----------------------------------------------------


