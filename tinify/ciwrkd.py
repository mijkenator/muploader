#!/usr/bin/python3

import os, sys, getopt, re
import grp
import signal,os
#import daemon, daemon.pidfile
import daemon
import lockfile, pprint
import time, datetime
import logging
import logging.handlers
import sqlalchemy,  random
from os.path import join, dirname
from sqlalchemy import Table, Column, Integer, String, MetaData, ForeignKey, DateTime, text
from sqlalchemy.sql import select, and_
from tendo import singleton
import threading, uuid, subprocess

grace_stop = 0



class MkhLog:
    def __init__(self):
        self.mkhlogger = logging.getLogger('BrtdLogger')
        self.mkhlogger.setLevel(logging.DEBUG)
        self.handler = logging.handlers.RotatingFileHandler('/var/log/ciwrkd.log', maxBytes=20000000, backupCount=50)
        self.mkhlogger.addHandler(self.handler)

    def logfmt(self, level, msg):
        dt = datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")
        return dt + " - " + level + "\t - " + msg

    def info(self, str):
        self.mkhlogger.info(self.logfmt("INFO", str))

    def error(self, str):
        self.mkhlogger.info(self.logfmt("ERROR", str))

    def exception(self, str):
        self.mkhlogger.exception(self.logfmt("ERROR", str))


def program_cleanup(signum, frame):
    my_logger.info('program_cleanup signal ... %s - %s' % (signum, frame))

def reload_program_config(signum, frame):
    my_logger.info('reload_program_config signal ... %s - %s' % (signum, frame))

def program_terminate(signum, frame):
    global grace_stop
    my_logger.info('graceful stop inited')
    #os.remove('/var/run/ciwrkd.pid')
    grace_stop = 1

def get_db():
    try:
        engine = sqlalchemy.create_engine('mysql+pymysql://mijkweb:mijkweb@34.198.91.198/edapi')
        conn = engine.connect()
    except Exception as e:
        my_logger.error('cannot connect to DB %s' % e)
        return (None, None)
    return (engine, conn)

def get_active_jobs():
    cmdret = subprocess.getoutput("ps aux | grep tf_mbd_auto.py | grep -v grep | wc -l")
    my_logger.info("Active jobs: '%s'" % cmdret)
    return int(cmdret)

def do_job(db, img, job_id, size):
    cmd = "/home/ap/work/tf_mbd_auto.py %s %s %s " % (img, size, job_id)
    my_logger.info("do_job: %s" % cmd)
    os.system(cmd + " &")
    #cmdret = subprocess.getoutput(cmd)
    #my_logger.info("do_job ret:  %s" % cmdret)
    pass
def update_job(db, job_id, job_state):
    db.engine.execute("update img_converter_queue set status=%s where id=%s" % (job_state, job_id))

def get_jobs(db, maxr=1):
    my_logger.info("get %s new jobs" % maxr)
    resp = db.engine.execute("select id, img, isize from img_converter_queue where status=0 order by id asc limit %s" % maxr)
    for row in resp:
        my_logger.info("processing img %s" % row[1])
        if row[2] == '2000' or row[2] == '780' or row[2] == 'mbdbg':
            img = '/opt/mybestday' + row[1]
        else: img = row[1]
        update_job(db, row[0], 1)
        do_job(db, img, row[0], row[2])
        #copy_resuls(row[1])
        #update_job(db, row[0], 2)

def iteratej(db):
    active_jobs = get_active_jobs()
    if max_jobs > active_jobs:
        get_jobs(db, max_jobs - active_jobs)

def main():
    global my_logger
    global max_jobs
    global threadLock
    max_jobs = 50

    context = daemon.DaemonContext(
        working_directory='/tmp',
        umask=0o002,
        #pidfile=daemon.pidfile.PIDLockFile('/var/run/ciwrkd.pid'),
        )

    context.signal_map = {
        signal.SIGTERM:  program_terminate,
        signal.SIGHUP:   program_terminate,
        signal.SIGUSR1:  reload_program_config,
        }

    with context:
        my_logger = MkhLog()
        my_logger.info('ciwrkd started')
        my_logger.info('ciwrkd configured')

        (engine, db) = get_db()
        my_logger.info('db connected')

        while True:
            try:
                iteratej(db)

                my_logger.info('working ... %s ' % grace_stop)
                time.sleep(2)
                if grace_stop == 1:
                    my_logger.info("Program stopped by evil signal")
                    sys.exit(0)        
            except Exception as e:
                my_logger.info("Program stopped by exception: %s" % e)
                #sys.exit(0)
                

me = singleton.SingleInstance()

main()

