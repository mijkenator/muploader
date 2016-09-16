#!/usr/bin/python3

from primarycolors import PrimaryColors
from PIL import Image
import sys, os, json
import sqlalchemy
from sqlalchemy import Table, Column, Integer, String, MetaData, ForeignKey, DateTime, text
from sqlalchemy.sql import select, and_

def do_job(origin_file, width):
    dst_path     = str(os.path.dirname(origin_file)) + '/p_i/'
    base_name    = str(os.path.basename(origin_file))
    file_name, file_extension = os.path.splitext(base_name)
    opt_origin = dst_path + base_name

    #print( 'File:', str(origin_file))
    #print( 'Path:', dst_path)
    #print( 'Base:', base_name)
    #print( 'Opto:', opt_origin)
    #print( 'Type:', convert_type)
    #print( "FN: %s FE:%s " % (file_name, file_extension) )

    im = Image.open(origin_file)
    xsize, ysize = im.size
    ratio = xsize // width
    print("x:%s , y:%s , w:%s , r:%s" % (xsize, ysize, width, ratio))
    y     = ysize // ratio
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

def save_picture_params(db, metadata, fname, jret):
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
    if rcount == 0: os.remove(fname)

if __name__ == "__main__":
    ret = do_job(sys.argv[1], int(sys.argv[2]))
    jret = json.dumps(ret)
    print(jret)
    (engine, db, metadata) = get_db()
    save_picture_params(db, metadata,  sys.argv[1], jret)

