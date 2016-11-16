#!/bin/bash

for file in /opt/mybestday/images/u/slide/*
do
   echo "$file"
   /home/ubuntu/work/tinify/tf_mbd.py "$file" 780 
done

