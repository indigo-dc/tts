#!/usr/bin/python
# -*- coding: utf-8 -*-

import sqlite3
import json
import sys
import os
from pwd import getpwnam
import traceback

#prefix of the username, followed by a number
USER_PREFIX="ttsuser_"
# the minimal uid to reserve for TTS users 
MIN_UID=2000
# the maximal uid to reserve for TTS users, set to 0 for unlimited 
MAX_UID=8999
# should local posix accounts be created
CREATE_LOCAL_ACCOUNTS = False
# the create_user python script to use if local accounts will be created  
CREATE_USER="sudo /usr/share/tts/idh/create_user.py"
# the location of the sqlite database
SQLITE_DB="/var/lib/tts/idh/idh.db"

con = None

def reset_database():
    global con
    print "resetting database %s"%SQLITE_DB
    cur = con.cursor()
    cur.execute("DROP TABLE IF EXISTS oidc")
    cur.execute("DROP TABLE IF EXISTS posix")
    create_database()

def create_database():
    global con
    cur = con.cursor()
    cur.execute("CREATE TABLE oidc (Id INTEGER PRIMARY KEY, Issuer TEXT, Subject TEXT)")
    cur.execute("CREATE TABLE posix (Id INTEGER PRIMARY KEY, OidcID INTEGER, Uid TEXT, UidNumber INTEGER, GidNumber INTEGER, HomeDir TEXT)")

def oidc_lookup(Issuer,Subject,Create):
    global con
    cur = con.cursor()
    Query = "SELECT Id FROM oidc WHERE (Issuer = ? AND Subject =  ?)"
    cur.execute(Query ,(Issuer, Subject))
    Result = cur.fetchall()
    OidcId = None
    if len(Result) == 0 and Create:
        OidcId = add_oidc(Issuer, Subject,False)
    elif len(Result) == 1:
        OidcId = Result[0][0]

    if OidcId == None:
        con.rollback()
        return json.dumps({'error': 'oidc not found'}) 
    
    Query = "SELECT Uid, UidNumber, GidNumber, HomeDir FROM posix WHERE (OidcID = ?)"
    cur.execute(Query ,(OidcId,))
    Rows = cur.fetchall()
    UserSet = None
    if len(Result) == 0 and Create:
        if CREATE_LOCAL_ACCOUNTS:
            UserSet = create_posix_account(OidcId,False)
        else:
            UserSet = create_posix(OidcId,False)
    elif len(Result) == 1:
        UserSet = Rows[0]

    if UserSet == None:
        con.rollback()
        return json.dumps({'error': 'posix not found'}) 

    UserName = UserSet[0]
    Uid = UserSet[1]
    Gid = UserSet[2]
    HomeDir = UserSet[3]
    con.commit()
    return json.dumps({"uid":UserName, "uidNumber":Uid, "gidNumber":Gid,
        "homeDirectory":HomeDir, "userIds":[[Issuer, Subject]]}) 

        
def create_posix(OidcId, Commit=True):
    # just virtual in the database
    # in contrast to create_account
    cur = con.cursor()
    Query = "SELECT MAX(UidNumber) FROM posix "
    cur.execute(Query)
    Result = cur.fetchall()
    MaxUid = Result[0][0]
    NextXid = None
    if MaxUid == None:
        NextXid = MIN_UID 
    else:
        NextXid = MaxUid +1
 
    if NextXid == None:
        return None

    if NextXid > MAX_UID and MAX_UID > 0:
        return None

    NextUserName = "%d%d"%(USER_PREFIX, UserIndex)
    HomeDir = "/home/%s"%NextUserName
    Result =  add_posix(NextUserName,NextXid,NextXid,HomeDir,OidcId,Commit)
    return Result

def create_posix_account(OidcId, Commit=True):
    UserIndex = OidcId + MIN_UID 
    UserName = "%d%d"%(USER_PREFIX, UserIndex)
    User = create_account(UserName) 
    Result =  add_posix(User[0],User[1],User[2],User[3],OidcId,Commit)
    return Result


def create_account(UserName):
    Cmd = "%s %s"%(CREATE_USER, UserName)
    Result = os.system(Cmd)
    if Result == 0:
        # user has been created
        UidNumber = getpwnam(UserName).pw_uid 
        GidNumber = getpwnam(UserName).pw_gid 
        HomeDir = getpwnam(UserName).pw_dir 
        return (UserName, UidNumber, GidNumber, HomeDir) 
    else:
        return None


    
def add_oidc(Issuer, Subject,Commit=True):
    global con
    cur = con.cursor()
    Query = "INSERT INTO oidc (Issuer, Subject) VALUES (?, ?)"
    cur.execute(Query ,(Issuer, Subject))
    if Commit:
        con.commit()
    return cur.lastrowid

def add_posix(UserName, Uid, Gid, HomeDir, OidcId, Commit=True):
    global con
    cur = con.cursor()
    Query = "INSERT INTO posix (OidcId, Uid, UidNumber, GidNumber, HomeDir) VALUES (?, ?, ?, ?, ?)"
    cur.execute(Query ,(OidcId, UserName, Uid, Gid, HomeDir))
    if Commit:
        con.commit()
    return (UserName, Uid, Gid, HomeDir)

def connect():
    global con
    create = False
    if not os.path.isfile(SQLITE_DB):
        create = True
    if not con:
        con = sqlite3.connect(SQLITE_DB)
        if create:
            create_database()

def disconnect():
    global conn
    con.close()


def main():
    connect()
    try:
        Cmd = None
        if len(sys.argv) > 1:
            Cmd = sys.argv[1]

        if Cmd == "OpenIdConnect":
            Issuer = sys.argv[2]
            Subject = sys.argv[3]
            print oidc_lookup(Issuer, Subject, True)
        elif Cmd == "add_posix":
            UserName = sys.argv[2]
            Uid = sys.argv[3]
            Gid = sys.argv[4]
            HomeDir = sys.argv[5]
            OidcId = sys.argv[6]
            print add_posix(UserName, Uid, Gid, HomeDir, OidcId) 
        elif Cmd == "create":
            UserName = sys.argv[2]
            print create_account(UserName)
        elif Cmd == "reset":
            reset_database()
        else:
            print json.dumps({"error":"no_parameter"})
    except Exception, E:
        TraceBack = traceback.format_exc(),
        print json.dumps({"error":"exception", "details": str(E), "trace":TraceBack})
        con.rollback()
        pass
    disconnect()


if __name__ == "__main__":
    main()
