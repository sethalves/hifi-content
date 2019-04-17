High Fidelity, Inc.
Avatar Exporter
Version 0.2

Note: It is recommended to use Unity versions between 2017.4.17f1 and 2018.2.12f1 for this Avatar Exporter.

To create a new avatar project:
1. Import your .fbx avatar model into your Unity project's Assets by either dragging and dropping the file into the Assets window or by using Assets menu > Import New Assets.
2. Select the .fbx avatar that you imported in step 1 in the Assets window, and in the Rig section of the Inspector window set the Animation Type to Humanoid and choose Apply. 
3. With the .fbx avatar still selected in the Assets window, choose High Fidelity menu > Export New Avatar.
4. Select a name for your avatar project (this will be used to create a directory with that name), as well as the target location for your project folder.
5. Once it is exported, your project directory will open in File Explorer.

To update an existing avatar project:
1. Select the existing .fbx avatar in the Assets window that you would like to re-export.
2. Choose High Fidelity menu > Update Existing Avatar and browse to the .fst file you would like to update.
3. If the .fbx file in your Unity Assets folder is newer than the existing .fbx file in your selected avatar project or vice-versa, you will be prompted if you wish to replace the older file with the newer file before performing the update.
4. Once it is updated, your project directory will open in File Explorer.

* WARNING *
If you are using any external textures as part of your .fbx model, be sure they are copied into the textures folder that is created in the project folder after exporting a new avatar.

For further details including troubleshooting tips, see the full documentation at https://docs.highfidelity.com/create-and-explore/avatars/create-avatars/unity-extension
