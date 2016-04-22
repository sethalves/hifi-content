#version 3.7;

//
// +kfi0 +kff5
//

global_settings {
  assumed_gamma 1.0
}


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



// PoVRay 3.6 / 3.7 Scene File "Clouds_by_media_01.pov"
// author: Friedrich A. Lohmueller, Dec-2009 / Jan-2011 / Nov-2013
// email: Friedrich.Lohmueller_at_t-online.de
// homepage: http://www.f-lohmueller.de
//--------------------------------------------------------------------------
#version 3.7; // 3.6;
global_settings{ assumed_gamma 1.0 }
#default{ finish{ ambient 0.1 diffuse 0.9 }}
//--------------------------------------------------------------------------
//--------------------------------------------------------------------------
#include "colors.inc"
#include "textures.inc"
#include "glass.inc"
#include "metals.inc"
#include "golds.inc"
#include "stones.inc"
#include "woods.inc"
#include "shapes.inc"
#include "shapes2.inc"
#include "functions.inc"
#include "math.inc"
#include "transforms.inc"
//-------------------------------------------------------------------------------------------------------<<<<
//------------------------------------------------------------- Camera_Position, Camera_look_at, Camera_Angle
#declare Camera_Position = < 0.00, 1.00,-3.00>;  // front view
#declare Camera_Look_At = < 0.00, 1.70, 0.00>;
#declare Camera_Angle = 65; // in degrees
//--------------------------------------------------------------------------------------------------------<<<<
// camera{ location  Camera_Position
//         right     x*image_width/image_height
//         angle     Camera_Angle
//         look_at   Camera_Look_At
//       }
//------------------------------------------------------------------------------------------------------<<<<<

// sun ---------------------------------------------------------------------
light_source{<-1500,2500,-2500> color White}
// sky ---------------------------------------------------------------------
sky_sphere { pigment { gradient <0,1,0>
                       color_map { [0.00 rgb <1.0,1.0,1.0>*0.8]
                                   [0.15 rgb <0.1,0.3,0.7>*0.6]
                                   [0.95 rgb <0.1,0.3,0.7>*0.6]
                                   [1.00 rgb <1.0,1.0,1.0>*0.8]
                                 }
                       scale 2
                     } // end of pigment
           } //end of skysphere
// ground ------------------------------------------------------------------
plane{ <0,1,0>, 0
       texture{ pigment{ color rgb <1.00,0.95,0.8>}
                normal { bumps 0.75 scale 0.025  }
                finish { phong 0.1 }
              } // end of texture
     } // end of plane
//--------------------------------------------------------------------------
//--------------------------------------------------------------------------
//---------------------------- objects in scene ----------------------------
//--------------------------------------------------------------------------
// the media clouds
box { <-1,-1,-1>,<1,1,1>
  texture {
    pigment {
     rgbf 1  // color Clear
    }
  }
  interior {
   media {
    method 3
    //intervals 2
    samples 10,10  // increese to 20,20
    absorption 1
    emission 0.5
    scattering { 0.8,<1,1,1>*0.5}

    density{ bozo //bumps
             color_map {
              [0.00 rgb 0]
              [0.50 rgb 0.01]
              [0.65 rgb 0.1]
              [0.75 rgb 0.5]
              [1.00 rgb 0.2]
             } // end color_map
             turbulence 0.85
             scale  0.75
             translate<1, 0.75,2>
           } // end density

    density{  boxed // or: spherical
              color_map {
               [0.0 rgb 0]    // border
               [0.1 rgb 0.05]
               [1.0 rgb 1]    // center
              } // end color_map
             scale <1,1,1>*1
           } // end density

   } // end media
  } // end interior
 hollow
 scale<5, 3.0, 5>
 translate<0,0.5,0>
 //----------------------------------------
scale 15
translate<0,02,30>
}//----------------------------------------
