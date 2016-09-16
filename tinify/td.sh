#!/bin/bash

SAVEIFS=$IFS
IFS=$(echo -en "\n\b")
#for line in $(find /opt/mwd_admin/images/collection_items/  -iname '*.jpg'); do 
for line in $(ls /opt/mwd_admin/images/collection_logos/*.jpg); do 
     echo "$line"
     DIR=$(dirname  "${line}")
     FNM=$(basename "${line}")
     echo "${DIR}/p_i/${FNM}"
     if [ ! -f "${DIR}/p_i/${FNM}" ]; then
	echo "File not found!"
	/home/ubuntu/work/tinify/tf.py "$line"
     fi
     sleep 3
done
IFS=$SAVEIFS

