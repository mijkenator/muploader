#!/usr/bin/python3

import sys, pymysql.cursors, os, glob, re
from os import listdir
from os.path import isfile, join, isdir 
from subprocess import call
import subprocess
from tendo import singleton

#
# /opt/mybestday/images/u/posts  mbd_event_posts.img
# /opt/mybestday/images/u/slide  mbd_ms_uploads.img
#


def check_mbd_ms_uploads(conn):
    print("Check (slide) mbd_ms_uploads!")
    dir1 = '/opt/mybestday/images/u/slide/'
    dirs = sum([[join(mypath, d) for d in listdir(mypath) if isdir(join(mypath, d)) and d != 'p_i'] for mypath in [ dir1 ]], [])
    print("DIRS: %s" % dirs)
    onlyfiles = sum([[join(mypath,f) for f in listdir(mypath) if isfile(join(mypath, f))] for mypath in dirs], [])
    print("FILES: %s" % onlyfiles)
    for fname in onlyfiles:
        if os.path.splitext(fname)[1].lower() in ['svg','.svg']:
            continue
        if mbdms_count(connection, fname[14:]) == 0:
            serch_pat = os.path.split(fname)[0]+"/p_i/"+os.path.splitext(os.path.basename(fname))[0]+ "_*"
            print("%s is NOT present in DB! " % fname[14:])
            os.remove(fname)
            #remove mods
            m_reg = re.escape(os.path.split(fname)[0])+"/p_i/"+re.escape(os.path.splitext(os.path.basename(fname))[0])+"_\d+"
            p = re.compile(m_reg)
            print("search regex: %s" % m_reg)
            for mf in glob.glob(serch_pat):
                if p.match(mf):
                    print("\tMods: %s" % mf)
                    os.remove(mf)
        else:
            serch_pat = os.path.split(fname)[0]+"/p_i/"+os.path.splitext(os.path.basename(fname))[0]+ "_*"
            #print("%s is present in DB! (%s)" % (fname, serch_pat))
            print("%s is present in DB!" % fname)
            c = 0
            m_reg = re.escape(os.path.split(fname)[0])+"/p_i/"+re.escape(os.path.splitext(os.path.basename(fname))[0])+"_\d+"
            p = re.compile(m_reg)
            #print("search regex: %s" % m_reg)
            for mf in glob.glob(serch_pat):
                if p.match(mf):
                    #print("\tMods: %s" % mf)
                    c += 1
            if c == 0:
                #no optimized images, produce it
                print("Making optimizing images!!!!!")
                if test_pipe(fname):
                    # insert job into queue
                    made_img_conv_qr(fname[14:], '2000')
                    #call(["/home/ubuntu/work/tinify/tf_mbd.py", fname, '2000'])
    return True

def check_mbd_event_posts(conn):
    print("Check (posts) mbd_event_posts!")
    dir1 = '/opt/mybestday/images/u/posts/'
    dirs = sum([[join(mypath, d) for d in listdir(mypath) if isdir(join(mypath, d)) and d != 'p_i'] for mypath in [ dir1 ]], [])
    print("DIRS: %s" % dirs)
    onlyfiles = sum([[join(mypath,f) for f in listdir(mypath) if isfile(join(mypath, f))] for mypath in dirs], [])
    print("FILES: %s" % onlyfiles)
    for fname in onlyfiles:
        if os.path.splitext(fname)[1].lower() in ['svg','.svg']:
            continue
        if mbdep_count(connection, fname[14:]) == 0:
            print("%s is NOT present in DB! " % fname)
            os.remove(fname)
            #remove mods
            serch_pat = os.path.split(fname)[0]+"/p_i/"+os.path.splitext(os.path.basename(fname))[0]+ "_*"
            m_reg = re.escape(os.path.split(fname)[0])+"/p_i/"+re.escape(os.path.splitext(os.path.basename(fname))[0])+"_\d+"
            p = re.compile(m_reg)
            #for mf in glob.glob(os.path.split(fname)[0]+"/p_i/"+os.path.splitext(os.path.basename(fname))[0]+ "_*"):
            for mf in glob.glob(serch_pat):
                #p = re.compile(re.escape(os.path.splitext(fname)[0])+"_\d+"+os.path.splitext(fname)[1])
                if p.match(mf):
                    pass
                    #print("\tMods: %s" % mf)
                    os.remove(mf)
        else:
            print("%s is present in DB!" % fname)
            c = 0
            #for mf in glob.glob(dir1+'p_i/'+os.path.splitext(fname)[0]+"_*"):
            serch_pat = os.path.split(fname)[0]+"/p_i/"+os.path.splitext(os.path.basename(fname))[0]+ "_*"
            m_reg = re.escape(os.path.split(fname)[0])+"/p_i/"+re.escape(os.path.splitext(os.path.basename(fname))[0])+"_\d+"
            p = re.compile(m_reg)
            for mf in glob.glob(serch_pat):
                #p = re.compile(".*/" + re.escape(os.path.splitext(fname)[0])+"_\d+"+os.path.splitext(fname)[1])
                if p.match(mf):
                    #print("\tMods: %s" % mf)
                    c += 1
            if c == 0:
                #no optimized images, produce it
                print("Making optimizing images")
                if test_pipe(fname):
                    made_img_conv_qr(fname[14:], '780')
                #    call(["/home/ubuntu/work/tinify/tf_mbd.py", fname, '780'])
    return True


def mbdep_count(connection, name):
	try:
		with connection.cursor() as cursor:
		   sql = "SELECT count(*) as 'c' FROM `mbd_event_post` WHERE img like %s"
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


def test_pipe(name):
    mycmd=subprocess.getoutput('ps aux | grep \'%s\' | grep -v grep | wc -l' % name)
    if mycmd == "0":
        return True
    else:
        return False

def made_img_conv_qr(name, isize):
    try:
        with connection.cursor() as cursor:
           sql = "insert into img_converter_queue (img, status, isize) values (%s, 0, %s)"
           cursor.execute(sql, (name, isize))
           connection.commit()
    except:
        pass

me = singleton.SingleInstance()

connection = pymysql.connect(host='localhost',
                             user='mijkweb',
                             password='mijkweb',
                             db='edapi',
                             charset='utf8mb4',
                             cursorclass=pymysql.cursors.DictCursor)

#check_mbd_ms_uploads(connection)
check_mbd_event_posts(connection)
	
connection.close()

print("DONE!")
