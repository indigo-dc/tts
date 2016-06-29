#!/usr/bin/python
# -*- coding: utf-8 -*-

import sqlite3
import json
import sys
import os
from pwd import getpwnam
import traceback


CREATE_LOCAL_ACCOUNTS = False


SQLITE_DB=os.path.join(os.path.dirname(os.path.realpath(__file__)),"idh.db")
# the create_user python script needs to be executable using sudo  
CREATE_USER_PYTHON="sudo %s"%os.path.join(os.path.dirname(os.path.realpath(__file__)),"create_user.py")
# the create_user binary needs to be owned by root and the setuid flag set
CREATE_USER_BIN=os.path.join(os.path.dirname(os.path.realpath(__file__)),"create_user")
CREATE_USER=CREATE_USER_PYTHON
con = None

def reset_database():
    global con
    print "resetting database %s"%SQLITE_DB
    cur = con.cursor()
    cur.execute("DROP TABLE IF EXISTS oidc")
    cur.execute("DROP TABLE IF EXISTS posix")
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
        NextXid = 2000
    else:
        NextXid = MaxUid +1
  
    if NextXid == None or NextXid > 9999:
        return None

    NextUserName = "ttsuser_%d"%NextXid 
    HomeDir = "/home/%s"%NextUserName
    Result =  add_posix(NextUserName,NextXid,NextXid,HomeDir,OidcId,Commit)
    return Result

def create_posix_account(OidcId, Commit=True):
    UserIndex = OidcId + 2000
    UserName = "ttsuser_%d"%UserIndex
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
    if not con:
        con = sqlite3.connect(SQLITE_DB)

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
