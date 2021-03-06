#!/usr/bin/python3

import sys, os
from PIL import Image
#import tinify
#tinify.key = "bjRHvxqtkW0Lw3vIVMUc2-aM-kxMfYln"

origin_file = sys.argv[1]
dst_path    = str(os.path.dirname(origin_file)) + '/p_i/'
base_name   = str(os.path.basename(origin_file))
file_name, file_extension = os.path.splitext(base_name)


print( 'File:', str(origin_file))
print( 'Path:', dst_path)
print( 'Base:', base_name)
print( "FN: %s FE:%s " % (file_name, file_extension) )

opt_origin = dst_path + base_name
#tinify.from_file(origin_file).to_file(opt_origin)

#tinify.from_file(opt_origin).resize(method="scale",height=180).to_file(dst_path + file_name + "_180" + file_extension)

width=200
im = Image.open(origin_file)
xsize, ysize = im.size
if xsize < 200:
    ratio = 1
    width = xsize
else:
    ratio = xsize / width
opt_origin = dst_path + file_name + "_200" + file_extension


y = int(ysize / ratio)
if file_extension is None or file_extension == '':
    im.resize((width, y)).save(opt_origin, "JPEG")
else:
    im.resize((width, y)).save(opt_origin)


