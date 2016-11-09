
// INSTRUCTIONS
//
//  - Place object on the location you want and set the rotation correct Y-Axis.
//	- Set the registration point x = 0.0 or 1.0 in most situations in my it where done on the Z-Axis
//	- Check the script and change if required the sound,direction etc. (line 31-33)
//	- Upload the script to ATP or other Webpage etc.
//	- copy and Paste the url inside the object.
//	- Make sure it's loading.
//
// Note: if the position or rotation or userDataOject is changed, you need to reload the script.
// 		 So the new position and rotation is read.
//
//		 Before making any changes on the doro, stop the script firts.
//		 Easy way todo that is by add // in front, and remove it when done.
//
//----------------------------
//
//  RiRa_Door_201605_rc1.js
//
//  Created by Richardus Raymaker on 15-05-2016
//	Thanks to Clement and others for the extra support and help.
//
//  Creative Common Attribution 4.0 International (CC BY 4.0)
//  http://creativecommons.org/licenses/by/4.0/
//
//----------------------------
// Sounds,
//
// https://www.freesound.org/people/rivernile7/sounds/249573/
// http://creativecommons.org/licenses/by/3.0/
//----------------------------

(function()
{
	var userDataOject = {"volume":0.5 , "reverse":true,"doorOpenTimeSec":15000,"doorIsOpen":false};
	var dooropenSound = SoundCache.getSound("http://market.simsquaremetaverse.nl/sounds/dooropen_CCBY30_249573.wav");
	var doorclosedSound = SoundCache.getSound("http://market.simsquaremetaverse.nl/sounds/doorclose_CCBY30_249573.wav");

//-----------------------------

	function rotToDeg(rotation)
	{
		var angle = Math.round((Quat.safeEulerAngles(rotation).y + 360.0) % 360.0);
		return angle;
	}

	function setDoorProperties(setrot, rotation, angvelocityY, angdamping, damping, friction)
	{
		if (setrot==true)
		{
			var newProperties =
			{
				rotation: rotation,
				angularVelocity: Quat.fromPitchYawRollDegrees(0,angvelocityY,0),
				angularDamping: angdamping,
				damping: damping,
				friction: friction,
			};
		}
		else
		{
			var newProperties =
			{
				angularVelocity: Quat.fromPitchYawRollDegrees(0,angvelocityY,0),
				angularDamping: angdamping,
				damping: damping,
				friction: friction,
			};
		}
		Entities.editEntity(Ent, newProperties);
	}

	function movedoor()
	{

		if (!doorMoves)
		{
			var angvelY;
			if (!userDataOject.doorIsOpen)
			{
				if (!userDataOject.reverse) { angvelY = vel; } else { angvelY = -vel; }
				Audio.playSound(dooropenSound, { loop: false, position: basepos ,volume: userDataOject.volume });
				timerAutoClose=Script.setInterval(autoDoorClose, userDataOject.doorOpenTimeSec);
			}
			else
			{
				if (!userDataOject.reverse) { angvelY = -vel; } else { angvelY = vel; }
			}

			setDoorProperties(false, baserot, angvelY,0.40,0.0,0.0);
			doorMoves = true;
			timerDoor=Script.setInterval(checkrotation,10);
		}
	}

	function autoDoorClose()
	{
		doorMoves =	false;
		userDataOject.doorIsOpen = true;
		Script.clearInterval(timerAutoClose);
		movedoor();
	}

	function checkrotation()
	{
		var target;
		var doorclosedangle;
        var data = Entities.getEntityProperties(Ent, ["rotation"]);
        var currentrot = rotToDeg(data.rotation);

		if (!userDataOject.reverse)
		{
			target = baserot + doorangle;
			doorclosedangle = doorangle-1;
		}
		else
		{
			target = baserot - doorangle;
			doorclosedangle = doorangle+1;
		}
		var signedDistance = (target - currentrot) % 360;

		if (Math.abs(signedDistance) < 5 && userDataOject.doorIsOpen==false)
		{
			userDataOject.doorIsOpen = true;
			setDoorProperties(false, baserot, 2,0.2,0.9,0.9);
			doorMoves =	false;
			Script.clearInterval(timerDoor);
		}
		else if (doorclosedangle < Math.abs(signedDistance) && userDataOject.doorIsOpen==true)
		{
			userDataOject.doorIsOpen = false;
			setDoorProperties(true, baserot, -2,0.2,0.9,0.9);
			doorMoves =	false;
			Audio.playSound(doorclosedSound, { loop: false, position: basepos ,volume: userDataOject.volume });
			Script.clearInterval(timerAutoClose);
			Script.clearInterval(timerDoor);
			var newProperties = {position: basepos};
			Entities.editEntity(Ent, newProperties);
		}
	}

    this.clickDownOnEntity = function(entityID, mouseEvent)
	{
		Ent = entityID;
		var copyEnt = Entities.getEntityProperties(Ent, ["rotation","position"]);
		var jsonEnt = JSON.stringify(copyEnt);
		var data = JSON.parse(jsonEnt);

		if (init)
		{
			baserot = rotToDeg(data.rotation);
			basepos = data.position;
			init=false;
		}
		movedoor();
    };

	var Ent;
	var timerDoor;
	var	timerAutoClose;
	var doorMoves = false;
	var init = true;
	var baserot;
	var basepos;
	var vel = 108;
	var doorangle = 88;
}
)

