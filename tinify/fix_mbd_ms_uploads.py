#!/usr/bin/python3

from primarycolors import PrimaryColors
from PIL import Image
import sys, os, json
from shutil import copyfile
import sqlalchemy
from sqlalchemy import Table, Column, Integer, String, MetaData, ForeignKey, DateTime, text
from sqlalchemy.sql import select, and_, not_
import tf_mbd

pdir = "/opt/mybestday"

def get_slides(db, metadata):
    mmu = Table('mbd_ms_uploads', metadata, autoload=True)
    s   = select([mmu.c.id, mmu.c.img]).\
        where(
            and_(
                mmu.c.img  != '',
                mmu.c.img  != None,
                mmu.c.extra == None,
                not_(mmu.c.img.like("/images/u/slide/%"))
            ))
    rs  = db.execute(s).fetchall()
    for row in rs:
        print("%s %s" % (row[mmu.c.id], row[mmu.c.img]))
        newname = "/images/u/slide/" + row[mmu.c.img][10:]
        print("newname: %s" % newname)
        copyfile(pdir + row[mmu.c.img], pdir + newname)
        try:
            ret = tf_mbd.do_job(pdir + newname, 2000)
            jret = json.dumps(ret)
            print(jret)
            us = mmu.update().where(mmu.c.id==row[mmu.c.id]).values(extra=jret, img=newname)
            db.execute(us)
            print("UPDATE OK")
            os.remove(pdir + row[mmu.c.img])
        except:
            print("ERROR", sys.exc_info()[0])
    

if __name__ == "__main__":
    (engine, db, metadata) = tf_mbd.get_db()
    print("HOLA!")
    get_slides(db, metadata)
