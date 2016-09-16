#!/bin/bash

for file in /opt/mybestday/images/u/posts/*
do
   echo "$file"
   /home/ubuntu/work/tinify/tf_mbd.py "$file" 2000 
done
