#version 3.7;

global_settings {
  assumed_gamma 1.0
}

#if(frame_number>=0 & frame_number<6)
#declare camloc = <0,2,0>;
#declare baseframe = 0;
#end

#if(frame_number>=6 & frame_number<12)
#declare camloc = <0,12,0>;
#declare baseframe = 6;
#end

#if(frame_number>=12 & frame_number<18)
#declare camloc = <0,22,0>;
#declare baseframe = 12;
#end

#if(frame_number>=18 & frame_number<24)
#declare camloc = <0,32,0>;
#declare baseframe = 18;
#end

#if(frame_number>=24 & frame_number<30)
#declare camloc = <0,42,0>;
#declare baseframe = 24;
#end

#if(frame_number>=30 & frame_number<36)
#declare camloc = <0,52,0>;
#declare baseframe = 30;
#end

#if(frame_number>=36 & frame_number<42)
#declare camloc = <0,62,0>;
#declare baseframe = 36;
#end

#if(frame_number>=42 & frame_number<48)
#declare camloc = <0,72,0>;
#declare baseframe = 42;
#end

#if(frame_number>=48 & frame_number<54)
#declare camloc = <0,82,0>;
#declare baseframe = 48;
#end

#if(frame_number>=54 & frame_number<60)
#declare camloc = <0,92,0>;
#declare baseframe = 54;
#end


camera {
 location camloc
 right x
 up y
 direction z

 angle 90

#if(frame_number-baseframe=0)
    look_at camloc + y
#end

#if(frame_number-baseframe=1)
    look_at camloc + -x
#end

#if(frame_number-baseframe=2)
    look_at camloc + z
#end

#if(frame_number-baseframe=3)
    look_at camloc + x
#end

#if(frame_number-baseframe=4)
    look_at camloc + -z
#end

#if(frame_number-baseframe=5)
    look_at camloc + -y
#end
}



// POV-Ray version 3.6/3.7 scenery file "p_sky05.pov"
// author: Friedrich A. Lohmueller, 2005, update Dec-2009 / Jan-2011 / Nov-2013
// homepage: http://www.f-lohmueller.de
//-----------------------------------------------------------------------------
#version 3.7; // 3.6;
#default{ finish{ ambient 0.1 diffuse 0.9 }}
//-----------------------------------------------------------------------------
global_settings { noise_generator 1 }


#include "colors.inc"
#include "textures.inc"

light_source { <3000, 5000, -3500>  rgb<1,1,1> }
   // sun height over the clouds produces cloud shadows

// ***************************************************************
// a brighter version of Darin Dugger's T_Clouds from "skies.inc"
// modified by Friedrich A. Lohmueller for using with "fog":
// ***************************************************************
#declare T_Cloud2_Lo =
texture {
    pigment { bozo
        turbulence 1.5
        octaves 10
        omega 0.5
        lambda 2.5
        color_map { [0.0 color rgbf<0.85, 0.85, 0.85, 0.00>*1.0 ]
                    [0.5 color rgbf<0.95, 0.95, 0.95, 0.90>*1.12  ]
                    [0.7 color rgbf<1, 1, 1, 1> ]
                    [1.0 color rgbf<1, 1, 1, 1> ] }
    }
        #if (version = 3.7 )  finish {emission 0.95 diffuse 0}
        #else                 finish { ambient 0.95 diffuse 0}
        #end
}
//---------------------------
#declare T_Cloud3_Lo =
texture {
    pigment { bozo
        turbulence 0.8 //0.6
        octaves 10
        omega 0.5
        lambda 2.5
        color_map { [0.0 color rgbf<0.95, 0.95, 0.95, 0.00>*1.2]
                    [0.4 color rgbf<0.90, 0.90, 0.90, 0.90>*1]
                    [0.7 color rgbf<1, 1, 1, 1> ]
                    [1.0 color rgbf<1, 1, 1, 1> ] }
           }
        #if (version = 3.7 )  finish {emission 1 diffuse 0}
        #else                 finish { ambient 1 diffuse 0}
        #end
}
texture {
    pigment { bozo
        turbulence 0.8 //0.6
        octaves 10
        omega 0.5
        lambda 2.5
        color_map { [0.00 color rgbf<.85, .85, .85, 0.5>*1.5]
                    [0.35 color rgbf<.95, .95, .95, .95>*1.1]
                    [0.50 color rgbf<1, 1, 1, 1> ]
                    [1.00 color rgbf<1, 1, 1, 1> ] }
        }
        finish {emission 1 diffuse 0}
scale 0.9
translate y*-0.15
}


// Darin Dugger's DD_Cloud_Sky texture mapped onto a pair of planes
//  first cloud level  500
// second cloud level 3000

// "hollow" added by Friedrich A.Lohmueller,2000
// for using together with fog!


#declare O_Cloud2_Lo =
union {
 plane { <0,1,0>, 500 hollow //!!!!
        texture { T_Cloud3_Lo  scale 500}}

 plane { <0,1,0>, 3000 hollow  //!!!!
        texture {T_Cloud2_Lo scale <900,1,3000>
                 translate <3000,0,0> rotate <0,-30,0>}}

 plane { <0,1,0> , 10000  hollow
        texture{ pigment {color SkyBlue*0.20}
                 finish {ambient 1 diffuse 0}}}
scale<1.5,1,1.25>
}//--------------------------------------------------



object{O_Cloud2_Lo rotate<0,0,0> translate<0,0,0>}


//---------------------------------------------------

// fog at the horizon
fog{fog_type   2
    distance   100
    color      rgb<1,1,1>*0.75
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


