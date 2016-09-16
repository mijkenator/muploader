#!/usr/bin/python3

from primarycolors import PrimaryColors

im = PrimaryColors(image='/opt/mybestday/images/u/01.jpg', max_colors=5)
for (c,p) in im.sorted_colors:
    print("%s - %s" % (c, p))
