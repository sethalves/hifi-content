#
#
#

all:

upload: flatten.svg lock.svg smooth.svg sphere-add.svg sphere-delete.svg unlock.svg voxel-add.svg voxel-delete.svg voxel-terrain.svg
	s3cmd put $^ s3://hifi-content/seth/production/icons/
	s3cmd setacl s3://hifi-content/seth/production/icons/ --acl-public --recursive
