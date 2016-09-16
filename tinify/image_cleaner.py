#!/usr/bin/python3

import sys, pymysql.cursors
from os import listdir
from os.path import isfile, join

def mdbcount(connection, name):
	try:
		with connection.cursor() as cursor:
		   sql = "SELECT count(*) as 'c' FROM `items` WHERE `front_image` like %s or `back_image` like %s"
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

dirs = ['/opt/mwd_admin/images/collection_items/', '/opt/mwd_admin/images/collection_items/']

onlyfiles = sum([[f for f in listdir(mypath) if isfile(join(mypath, f))] for mypath in dirs], [])
print(onlyfiles)


connection = pymysql.connect(host='localhost',
                             user='mijkweb',
                             password='mijkweb',
                             db='edapi',
                             charset='utf8mb4',
                             cursorclass=pymysql.cursors.DictCursor)

for fname in onlyfiles:
	if mdbcount(connection, fname) + mdbcount1(connection, fname) > 0:
		#print("%s is present in DB!" % fname)
		a = 1
	else:
		print("%s is NOT present in DB!" % fname)
	
connection.close()
