#!/usr/bin/python3

from primarycolors import PrimaryColors
from PIL import Image
import sys, os, json, time
import sqlalchemy
from sqlalchemy import Table, Column, Integer, String, MetaData, ForeignKey, DateTime, text
from sqlalchemy.sql import select, and_

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
        im.resize((width, y)).save(opt_origin, "JPEG")
    else:
        im.resize((width, y)).save(opt_origin)

    im = PrimaryColors(image=opt_origin, max_colors=5)
    colors = []
    for (c,p) in im.sorted_colors:
        colors.append(({'color':c, 'probability':p}))

    ret = {'x': width, 'y':y, 'colors':colors}
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

def save_picture_params(db, metadata, fname, jret, tcount):
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
        us = mep.update().where(mep.c.id==row[mep.c.id]).values(extra=jret)
        db.execute(us)
    if rcount == 0 and tcount < 4:
        time.sleep(2)
        save_picture_params(db, metadata, fname, jret, tcount+1)

def save_mbdms_upload(db, metadata, fname, jret, tcount):
    mmu = Table('mbd_ms_uploads', metadata, autoload=True)
    s   = select([mmu.c.id, mmu.c.img]).where( mmu.c.img  == fname[14:] )

    rs  = db.execute(s).fetchall()
    rcount = 0
    for row in rs:
        rcount += 1
        print("%s %s" % (row[mmu.c.id], row[mmu.c.img]))
        us = mmu.update().where(mmu.c.id==row[mmu.c.id]).values(extra=jret)
        db.execute(us)

    if rcount == 0 and tcount < 4:
        time.sleep(2)
        save_mbdms_upload(db, metadata, fname, jret, tcount+1)
    

if __name__ == "__main__":
    ret = do_job(sys.argv[1], int(sys.argv[2]))
    jret = json.dumps(ret)
    print(jret)
    (engine, db, metadata) = get_db()
    if sys.argv[2] == '600':
        tmpr = do_job(sys.argv[1], 2000)
        tmpr = do_job(sys.argv[1], 180)
        save_picture_params(db, metadata,  sys.argv[1], jret, 0)
    elif sys.argv[2] == '2000':
        tmpr = do_job(sys.argv[1], 600)
        tmpr = do_job(sys.argv[1], 180)
        save_mbdms_upload(db, metadata, sys.argv[1], jret, 0)
    else:
        print("unknown width, saving ignored")

