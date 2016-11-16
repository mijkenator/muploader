#!/usr/bin/python3

import sys, pymysql.cursors, os, glob, re
from os import listdir
from os.path import isfile, join
from subprocess import call
import subprocess
from tendo import singleton

#
# /opt/mwd_admin/images/collection_item     items.back_image,front_images
# /opt/mwd_admin/images/collection_logos    collections.poster,logo,poster_logo,poster_season,poster_collection
# /opt/mwd_admin/images/bmcollection_items  bm_items.back_image,front_images
# /opt/mwd_admin/images/bmcollection_logo   bm_collections.poster,logo,poster_logo,poster_season,poster_collections
# /opt/mwd_admin/images/usercovers     user.coverimage
# 
# /opt/mwd_admin/images/blog_category   blog_category.image
# /opt/mwd_admin/images/blog_post       blog_post.image, logo
#
# /opt/mybestday/images/u/       mbd_uploads.img
# /opt/mybestday/images/u/posts  mbd_event_posts.img
# /opt/mybestday/images/u/slide  mbd_ms_uploads.img
#

def check_mbd_uploads(conn):
    print("Check (avatars) mbd_uploads!")
    dir1 = '/opt/mybestday/images/u/'
    dirs = [ dir1 ]
    onlyfiles = sum([[f for f in listdir(mypath) if isfile(join(mypath, f))] for mypath in dirs], [])
    for fname in onlyfiles:
        if os.path.splitext(fname)[1].lower() in ['svg','.svg']:
            continue
        if mbdu_count(connection, "/images/u/"+fname) == 0:
            print("%s is NOT present in DB! " % fname)
            os.remove(dir1 + fname)
            #remove mods
            for mf in glob.glob(dir1+'p_i/'+os.path.splitext(fname)[0]+"_*"):
                p = re.compile(".*/" + re.escape(os.path.splitext(fname)[0])+"_\d+"+os.path.splitext(fname)[1])
                if p.match(mf):
                    print("\tMods: %s" % mf)
                    os.remove(mf)
        else:
            print("%s is present in DB!" % fname)
            c = 0
            for mf in glob.glob(dir1+'p_i/'+os.path.splitext(fname)[0]+"_*"):
                p = re.compile(".*/" + re.escape(os.path.splitext(fname)[0])+"_\d+"+os.path.splitext(fname)[1])
                if p.match(mf):
                    print("\tMods: %s" % mf)
                    c += 1
            if c == 0:
                #no optimized images, produce it
                print("Making optimizing images")
                if test_pipe(fname):
                    call(["/home/ubuntu/work/tinify/tf_mbd180.py", dir1+fname])
    return True
    return True

def check_mbd_ms_uploads(conn):
    print("Check (slide) mbd_ms_uploads!")
    dir1 = '/opt/mybestday/images/u/slide/'
    dirs = [ dir1 ]
    onlyfiles = sum([[f for f in listdir(mypath) if isfile(join(mypath, f))] for mypath in dirs], [])
    for fname in onlyfiles:
        if os.path.splitext(fname)[1].lower() in ['svg','.svg']:
            continue
        if mbdms_count(connection, "/images/u/slide/"+fname) == 0:
            print("%s is NOT present in DB! " % fname)
            os.remove(dir1 + fname)
            #remove mods
            for mf in glob.glob(dir1+'p_i/'+os.path.splitext(fname)[0]+"_*"):
                p = re.compile(".*/" + re.escape(os.path.splitext(fname)[0])+"_\d+"+os.path.splitext(fname)[1])
                if p.match(mf):
                    print("\tMods: %s" % mf)
                    os.remove(mf)
        else:
            print("%s is present in DB!" % fname)
            c = 0
            for mf in glob.glob(dir1+'p_i/'+os.path.splitext(fname)[0]+"_*"):
                p = re.compile(".*/" + re.escape(os.path.splitext(fname)[0])+"_\d+"+os.path.splitext(fname)[1])
                if p.match(mf):
                    print("\tMods: %s" % mf)
                    c += 1
            if c == 0:
                #no optimized images, produce it
                print("Making optimizing images")
                if test_pipe(fname):
                    call(["/home/ubuntu/work/tinify/tf_mbd.py", dir1+fname, '2000'])
    return True

def check_mbd_event_posts(conn):
    print("Check (posts) mbd_event_posts!")
    dir1 = '/opt/mybestday/images/u/posts/'
    dirs = [ dir1 ]
    onlyfiles = sum([[f for f in listdir(mypath) if isfile(join(mypath, f))] for mypath in dirs], [])
    for fname in onlyfiles:
        if os.path.splitext(fname)[1].lower() in ['svg','.svg']:
            continue
        if mbdep_count(connection, "/images/u/posts/"+fname) == 0:
            print("%s is NOT present in DB! " % fname)
            os.remove(dir1 + fname)
            #remove mods
            for mf in glob.glob(dir1+'p_i/'+os.path.splitext(fname)[0]+"_*"):
                p = re.compile(".*/" + re.escape(os.path.splitext(fname)[0])+"_\d+"+os.path.splitext(fname)[1])
                if p.match(mf):
                    print("\tMods: %s" % mf)
                    os.remove(mf)
        else:
            print("%s is present in DB!" % fname)
            c = 0
            for mf in glob.glob(dir1+'p_i/'+os.path.splitext(fname)[0]+"_*"):
                p = re.compile(".*/" + re.escape(os.path.splitext(fname)[0])+"_\d+"+os.path.splitext(fname)[1])
                if p.match(mf):
                    print("\tMods: %s" % mf)
                    c += 1
            if c == 0:
                #no optimized images, produce it
                print("Making optimizing images")
                if test_pipe(fname):
                    call(["/home/ubuntu/work/tinify/tf_mbd.py", dir1+fname, '780'])
    return True

def check_blogpost(conn):
    print("Check blog post!")
    dir1 = '/opt/mwd_admin/images/blog_post/'
    dirs = [ dir1 ]
    onlyfiles = sum([[f for f in listdir(mypath) if isfile(join(mypath, f))] for mypath in dirs], [])
    #print(onlyfiles)
    for fname in onlyfiles:
        if os.path.splitext(fname)[1].lower() in ['svg','.svg']:
            continue
        if bp_count(connection, "/images/blog_post/"+fname) == 0:
            print("%s is NOT present in DB! " % fname)
            os.remove(dir1 + fname)
        else:
            print("%s is present in DB!" % fname)

def check_blogcategory(conn):
    print("Check blog category!")
    dir1 = '/opt/mwd_admin/images/blog_category/'
    dirs = [ dir1 ]
    onlyfiles = sum([[f for f in listdir(mypath) if isfile(join(mypath, f))] for mypath in dirs], [])
    #print(onlyfiles)
    for fname in onlyfiles:
        if os.path.splitext(fname)[1].lower() in ['svg','.svg']:
            continue
        if bc_count(connection, "/images/blog_category/"+fname) == 0:
            print("%s is NOT present in DB! " % fname)
            os.remove(dir1 + fname)
        else:
            print("%s is present in DB!" % fname)

def check_usercovers(conn):
    print("Check user covers!")
    dir1 = '/opt/mwd_admin/images/usercovers/'
    dirs = [ dir1 ]
    onlyfiles = sum([[f for f in listdir(mypath) if isfile(join(mypath, f))] for mypath in dirs], [])
    #print(onlyfiles)
    for fname in onlyfiles:
        if os.path.splitext(fname)[1].lower() in ['svg','.svg']:
            continue
        if uc_count(connection, "/images/usercovers/"+fname) == 0:
            print("%s is NOT present in DB! " % fname)
            os.remove(dir1 + fname)
        else:
            print("%s is present in DB!" % fname)

def check_collections(conn):
    print("Check collections!")
    dir1 = '/opt/mwd_admin/images/collection_logos/'
    dirs = [ dir1 ]
    onlyfiles = sum([[f for f in listdir(mypath) if isfile(join(mypath, f))] for mypath in dirs], [])
    #print(onlyfiles)
    for fname in onlyfiles:
        if os.path.splitext(fname)[1].lower() in ['svg','.svg']:
            continue
        if mdbcount1(connection, "/images/collection_logos/"+fname) == 0:
            print("%s is NOT present in DB! " % fname)
            os.remove(dir1 + fname)
            #remove mods
            for mf in glob.glob(dir1+'p_i/'+os.path.splitext(fname)[0]+"_*x*_*"):
                p = re.compile(".*/" + re.escape(os.path.splitext(fname)[0])+"_\d+x\d+_"+os.path.splitext(fname)[1])
                if p.match(mf):
                    print("\tMods: %s" % mf)
                    os.remove(mf)
        else:
            print("%s is present in DB!" % fname)
            c = 0
            for mf in glob.glob(dir1+'p_i/'+os.path.splitext(fname)[0]+"_*x*_*"):
                p = re.compile(".*/" + re.escape(os.path.splitext(fname)[0])+"_\d+x\d+_"+os.path.splitext(fname)[1])
                if p.match(mf):
                    print("\tMods: %s" % mf)
                    c += 1
            if c == 0:
                #no optimized images, produce it
                print("Making optimizing images")
                if test_pipe(fname):
                    call(["/home/ubuntu/work/tinify/tf.py", dir1+fname])


def check_collection_items(conn):
    print("Check collection items!")
    dir1 = '/opt/mwd_admin/images/collection_items/'
    dirs = [ dir1 ]
    onlyfiles = sum([[f for f in listdir(mypath) if isfile(join(mypath, f))] for mypath in dirs], [])
    #print(onlyfiles)
    for fname in onlyfiles:
        if os.path.splitext(fname)[1].lower() in ['svg','.svg']:
            continue
        if mdbcount(connection, "/images/collection_items/"+fname) == 0:
            print("%s is NOT present in DB! " % fname)
            os.remove(dir1 + fname)
            for mf in glob.glob(dir1+'p_i/'+os.path.splitext(fname)[0]+"_*x*_*"):
                p = re.compile(".*/" + re.escape(os.path.splitext(fname)[0])+"_\d+x\d+_"+os.path.splitext(fname)[1])
                if p.match(mf):
                    print("\tMods: %s" % mf)
                    os.remove(mf)
        else:
            print("%s is present in DB!" % fname)
            c = 0
            for mf in glob.glob(dir1+'p_i/'+os.path.splitext(fname)[0]+"_*x*_*"):
                p = re.compile(".*/" + re.escape(os.path.splitext(fname)[0])+"_\d+x\d+_"+os.path.splitext(fname)[1])
                if p.match(mf):
                    print("\tMods: %s" % mf)
                    c += 1
            if c == 0:
                #no optimized images, produce it
                print("Making optimizing images")
                if test_pipe(fname):
                    call(["/home/ubuntu/work/tinify/tf.py", dir1+fname])

def check_bmcollections(conn):
    print("Check VMcollections!")
    dir1 = '/opt/mwd_admin/images/bmcollection_logos/'
    dirs = [ dir1 ]
    onlyfiles = sum([[f for f in listdir(mypath) if isfile(join(mypath, f))] for mypath in dirs], [])
    #print(onlyfiles)
    for fname in onlyfiles:
        if os.path.splitext(fname)[1].lower() in ['svg','.svg']:
            continue
        if bm_mdbcount1(connection, "/images/bmcollection_logos/"+fname) == 0:
            print("%s is NOT present in DB! " % fname)
            os.remove(dir1 + fname)
            #remove mods
            for mf in glob.glob(dir1+'p_i/'+os.path.splitext(fname)[0]+"_*x*_*"):
                p = re.compile(".*/" + re.escape(os.path.splitext(fname)[0])+"_\d+x\d+_"+os.path.splitext(fname)[1])
                if p.match(mf):
                    print("\tMods: %s" % mf)
                    os.remove(mf)
        else:
            print("%s is present in DB!" % fname)
            c = 0
            for mf in glob.glob(dir1+'p_i/'+os.path.splitext(fname)[0]+"_*x*_*"):
                p = re.compile(".*/" + re.escape(os.path.splitext(fname)[0])+"_\d+x\d+_"+os.path.splitext(fname)[1])
                if p.match(mf):
                    print("\tMods: %s" % mf)
                    c += 1
            if c == 0:
                #no optimized images, produce it
                print("Making optimizing images")
                if test_pipe(fname):
                    call(["/home/ubuntu/work/tinify/tf.py", dir1+fname])

def check_bmcollection_items(conn):
    print("Check BMcollection items!")
    dir1 = '/opt/mwd_admin/images/bmcollection_items/'
    dirs = [ dir1 ]
    onlyfiles = sum([[f for f in listdir(mypath) if isfile(join(mypath, f))] for mypath in dirs], [])
    #print(onlyfiles)
    for fname in onlyfiles:
        if os.path.splitext(fname)[1].lower() in ['svg','.svg']:
            continue
        if bm_mdbcount(connection, "/images/bmcollection_items/"+fname) == 0:
            print("%s is NOT present in DB! " % fname)
            os.remove(dir1 + fname)
            for mf in glob.glob(dir1+'p_i/'+os.path.splitext(fname)[0]+"_*x*_*"):
                p = re.compile(".*/" + re.escape(os.path.splitext(fname)[0])+"_\d+x\d+_"+os.path.splitext(fname)[1])
                if p.match(mf):
                    print("\tMods: %s" % mf)
                    os.remove(mf)
        else:
            print("%s is present in DB!" % fname)
            c = 0
            for mf in glob.glob(dir1+'p_i/'+os.path.splitext(fname)[0]+"_*x*_*"):
                p = re.compile(".*/" + re.escape(os.path.splitext(fname)[0])+"_\d+x\d+_"+os.path.splitext(fname)[1])
                if p.match(mf):
                    print("\tMods: %s" % mf)
                    c += 1
            if c == 0:
                #no optimized images, produce it
                print("Making optimizing images")
                if test_pipe(fname):
                    call(["/home/ubuntu/work/tinify/tf.py", dir1+fname])

def mbdep_count(connection, name):
	try:
		with connection.cursor() as cursor:
		   sql = "SELECT count(*) as 'c' FROM `mbd_event_post` WHERE img like %s"
		   cursor.execute(sql, ('%'+name))
		   result = cursor.fetchone()
		   return result['c']
	except:
		return 0

def mbdu_count(connection, name):
	try:
		with connection.cursor() as cursor:
		   sql = "SELECT count(*) as 'c' FROM `mbd_uploads` WHERE img like %s"
		   cursor.execute(sql, ('%'+name))
		   result = cursor.fetchone()
		   return result['c']
	except:
		return 0

def mbdms_count(connection, name):
	try:
		with connection.cursor() as cursor:
		   sql = "SELECT count(*) as 'c' FROM `mbd_ms_uploads` WHERE img like %s"
		   cursor.execute(sql, ('%'+name))
		   result = cursor.fetchone()
		   return result['c']
	except:
		return 0

def bp_count(connection, name):
	try:
		with connection.cursor() as cursor:
		   sql = "SELECT count(*) as 'c' FROM `blog_post` WHERE image like %s or logo like %s"
		   cursor.execute(sql, ('%'+name,'%'+name))
		   result = cursor.fetchone()
		   return result['c']
	except:
		return 0

def bc_count(connection, name):
	try:
		with connection.cursor() as cursor:
		   sql = "SELECT count(*) as 'c' FROM `blog_category` WHERE image like %s"
		   cursor.execute(sql, ('%'+name))
		   result = cursor.fetchone()
		   return result['c']
	except:
		return 0

def uc_count(connection, name):
	try:
		with connection.cursor() as cursor:
		   sql = "SELECT count(*) as 'c' FROM `user` WHERE coverimage like %s"
		   cursor.execute(sql, ('%'+name))
		   result = cursor.fetchone()
		   return result['c']
	except:
		return 0

def mdbcount(connection, name):
	try:
		with connection.cursor() as cursor:
		   sql = "SELECT count(*) as 'c' FROM `items` WHERE `front_image` like %s or `back_image` like %s"
		   cursor.execute(sql, ('%'+name,'%'+name,))
		   result = cursor.fetchone()
		   return result['c']
	except:
		return 0

def bm_mdbcount(connection, name):
	try:
		with connection.cursor() as cursor:
		   sql = "SELECT count(*) as 'c' FROM `bm_items` WHERE `front_image` like %s or `back_image` like %s"
		   cursor.execute(sql, ('%'+name,'%'+name,))
		   result = cursor.fetchone()
		   return result['c']
	except:
		return 0

def mdbcount1(connection, name):
	try:
		with connection.cursor() as cursor:
		    sql = "SELECT count(*) as 'c' FROM `collections` WHERE `poster` like %s or `logo` like %s or `poster_logo` like %s or poster_season like %s or poster_collection like %s"
		    cursor.execute(sql, ('%'+name,'%'+name,'%'+name,'%'+name,'%'+name,))
		    result = cursor.fetchone()
		    return result['c']
	except:
		return 0

def bm_mdbcount1(connection, name):
	try:
		with connection.cursor() as cursor:
		    sql = "SELECT count(*) as 'c' FROM `bm_collections` WHERE `poster` like %s or `logo` like %s or `poster_logo` like %s or poster_season like %s or poster_collection like %s"
		    cursor.execute(sql, ('%'+name,'%'+name,'%'+name,'%'+name,'%'+name,))
		    result = cursor.fetchone()
		    return result['c']
	except:
		return 0

def test_pipe(name):
    mycmd=subprocess.getoutput('ps aux | grep \'%s\' | grep -v grep | wc -l' % name)
    if mycmd == "0":
        return True
    else:
        return False

me = singleton.SingleInstance()

connection = pymysql.connect(host='localhost',
                             user='mijkweb',
                             password='mijkweb',
                             db='edapi',
                             charset='utf8mb4',
                             cursorclass=pymysql.cursors.DictCursor)

check_collections(connection)
check_collection_items(connection)
check_bmcollections(connection)
check_bmcollection_items(connection)
check_usercovers(connection)
check_blogcategory(connection)
check_blogpost(connection)
check_mbd_ms_uploads(connection)
check_mbd_event_posts(connection)
check_mbd_uploads(connection)
	
connection.close()
