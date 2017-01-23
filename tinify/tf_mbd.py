#!/usr/bin/python3

from primarycolors import PrimaryColors
from PIL import Image
from PIL.ExifTags import TAGS, GPSTAGS
import sys, os, json, time
import sqlalchemy
from sqlalchemy import Table, Column, Integer, String, MetaData, ForeignKey, DateTime, text
from sqlalchemy.sql import select, and_
from subprocess import call
import subprocess

def _get_if_exist(data, key):
    if key in data:
        return data[key]
    return None

def _convert_to_degress(value):
    d0 = value[0][0]
    d1 = value[0][1]
    d = float(d0) / float(d1)

    m0 = value[1][0]
    m1 = value[1][1]
    m = float(m0) / float(m1)

    s0 = value[2][0]
    s1 = value[2][1]
    s = float(s0) / float(s1)

    return d + (m / 60.0) + (s / 3600.0)

def get_meta(im):
    exif_data = im._getexif()
    ctime = ''
    gpsi  = ''
    lat, lon = None, None
    for tag, value in exif_data.items():
        decoded = TAGS.get(tag, tag)
        print("%s = %s" % (decoded, value))
        if decoded   == 'DateTime': ctime = value
        if decoded   == 'DateTimeOriginal': ctime = value
        elif decoded == 'GPSInfo':
            gps_info = {}
            for t in value:
                sub_decoded = GPSTAGS.get(t, t)
                gps_info[sub_decoded] = value[t]
            
            gps_latitude = _get_if_exist(gps_info, "GPSLatitude")
            gps_latitude_ref = _get_if_exist(gps_info, 'GPSLatitudeRef')
            gps_longitude = _get_if_exist(gps_info, 'GPSLongitude')
            gps_longitude_ref = _get_if_exist(gps_info, 'GPSLongitudeRef')

            if gps_latitude and gps_latitude_ref and gps_longitude and gps_longitude_ref:
                lat = _convert_to_degress(gps_latitude)
                if gps_latitude_ref != "N":                     
                    lat = 0 - lat

                lon = _convert_to_degress(gps_longitude)
                if gps_longitude_ref != "E":
                    lon = 0 - lon
    if lat != None and lon != None:
        gpsi = "%s %s" % (lat, lon)
    return (ctime, gpsi)

def extra_resizes(origin_file):
    dst_path     = str(os.path.dirname(origin_file)) + '/p_i/'
    base_name    = str(os.path.basename(origin_file))
    file_name, file_extension = os.path.splitext(base_name)

    opt_origin    = dst_path + file_name + "_2000" + file_extension
    opt_origin_wp = dst_path + file_name + "_2000" + '.webp'
    subprocess.getoutput("cwebp -q 90 %s -o %s" % (opt_origin, opt_origin_wp))
    for width in [1500,1000,800,700,600,500,400,300,200]:
        opt_dst    = dst_path + file_name + "_" + str(width) + file_extension
        opt_dst_wp = dst_path + file_name + "_" + str(width) + '.webp'
        print("-> %s" % opt_dst)
        im = Image.open(opt_origin)
        xsize, ysize = im.size
        if xsize < width:
            ratio = 1
            width = xsize
        else:
            ratio = xsize / width
        y     = int(ysize / ratio)
        #im.resize((width, y)).save(opt_dst)
        if file_extension is None or file_extension == '':
            im.resize((width, y)).save(opt_dst, format='JPEG', quality=100, optimize=True)
        elif file_extension.lower() == '.jpg' or file_extension.lower() == '.jpeg':
            im.resize((width, y)).save(opt_dst, format='JPEG', quality=100, optimize=True)
        else:
            im.resize((width, y)).save(opt_dst)
        webpcmd = "cwebp -q 90 %s -o %s" % (opt_origin, opt_dst_wp)
        subprocess.getoutput(webpcmd)


def do_job(origin_file, width):
    dst_path     = str(os.path.dirname(origin_file)) + '/p_i/'
    base_name    = str(os.path.basename(origin_file))
    file_name, file_extension = os.path.splitext(base_name)
    #opt_origin = dst_path + base_name
    opt_origin = dst_path + file_name + "_" + str(width) + file_extension

    #print( 'File:', str(origin_file))
    #print( 'Path:', dst_path)
    #print( 'Base:', base_name)
    #print( 'Opto:', opt_origin)
    #print( 'Type:', convert_type)
    #print( "FN: %s FE:%s " % (file_name, file_extension) )

    im = Image.open(origin_file)
    xsize, ysize = im.size
    if xsize < width:
        ratio = 1
        width = xsize
    else:
        ratio = xsize / width
    print("x:%s , y:%s , w:%s , r:%s" % (xsize, ysize, width, ratio))
    y     = int(ysize / ratio)
    if file_extension is None or file_extension == '':
        im.resize((width, y)).save(opt_origin, format='JPEG', quality=80)
    elif file_extension == '.jpg' or file_extension == '.JPG':
        #im.resize((width, y)).save(opt_origin, format='JPEG', quality=80, optimize=True)
        im.resize((width, y)).save(opt_origin)
    else:
        im.resize((width, y)).save(opt_origin)

    im = PrimaryColors(image=opt_origin, max_colors=5)
    colors = []
    for (c,p) in im.sorted_colors:
        colors.append(({'color':c, 'probability':p}))

    try:
        (create_time, gps_info) = get_meta(im)
    except:
        (create_time, gps_info) = ("", "")

        
    ret = {'x': width, 'y':y, 'colors':colors, 'ctime':create_time, 'gps':gps_info}
    return ret

def get_db():
    try:
        engine = sqlalchemy.create_engine('mysql+pymysql://mijkweb:mijkweb@localhost/edapi')
        conn = engine.connect()
        metadata = MetaData(bind=engine)
    except:
        print('cannot connect to DB')
        return (None, None, None)
    return (engine, conn, metadata)

def save_picture_params(db, metadata, fname, jret, tcount, gpsi, ctimev):
    mep = Table('mbd_event_post', metadata, autoload=True)
    s   = select([mep.c.id, mep.c.eid, mep.c.img]).\
        where(
            and_(
                mep.c.type == 'image', 
                mep.c.img  != '',
                mep.c.img  != None,
                mep.c.img  == fname[14:])
            )
    rs  = db.execute(s).fetchall()
    rcount = 0
    for row in rs:
        rcount += 1
        print("%s %s %s" % (row[mep.c.id], row[mep.c.eid], row[mep.c.img]))
        us = mep.update().where(mep.c.id==row[mep.c.id]).values(extra=jret, ctime=ctimev, gps_info=gpsi)
        db.execute(us)
    if rcount == 0 and tcount < 4:
        time.sleep(2)
        save_picture_params(db, metadata, fname, jret, tcount+1, gpsi, ctimev)

def save_mbdms_upload(db, metadata, fname, jret, tcount, gpsi, ctimev):
    mmu = Table('mbd_ms_uploads', metadata, autoload=True)
    s   = select([mmu.c.id, mmu.c.img]).where( mmu.c.img  == fname[14:] )

    rs  = db.execute(s).fetchall()
    rcount = 0
    for row in rs:
        rcount += 1
        print("%s %s" % (row[mmu.c.id], row[mmu.c.img]))
        us = mmu.update().where(mmu.c.id==row[mmu.c.id]).values(extra=jret, ctime=ctimev, gps_info=gpsi)
        db.execute(us)

    if rcount == 0 and tcount < 4:
        time.sleep(2)
        save_mbdms_upload(db, metadata, fname, jret, tcount+1, gpsi, ctimev)
    

if __name__ == "__main__":
    ret = do_job(sys.argv[1], int(sys.argv[2]))
    
    gpsi  = ret.pop('gps', "")
    ctime = ret.pop('ctime', "")

    jret = json.dumps(ret)
    print(jret)
    (engine, db, metadata) = get_db()
    if sys.argv[2] == '780':
        tmpr = do_job(sys.argv[1], 2000)
        tmpr = do_job(sys.argv[1], 180)
        extra_resizes(sys.argv[1])
        save_picture_params(db, metadata,  sys.argv[1], jret, 0, gpsi, ctime)
    elif sys.argv[2] == '2000':
        tmpr = do_job(sys.argv[1], 780)
        tmpr = do_job(sys.argv[1], 180)
        extra_resizes(sys.argv[1])
        save_mbdms_upload(db, metadata, sys.argv[1], jret, 0, gpsi, ctime)
    else:
        print("unknown width, saving ignored")

