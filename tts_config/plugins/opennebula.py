#!/usr/bin/env python2
# -*- coding: utf-8 -*-

import urlparse
import json
import base64
import random
import string
import sys
import traceback
import xmlrpclib
from xml.dom.minidom import parseString

import sqlite3

# URL of the XML-RPC API of the OpeNebula server
ONE_API_ENDPOINT = "http://localhost:2633/RPC2"
# User to access the API
SESSIONID = "oneadmin:somepass"
# Group ID to add the users created
USERS_GROUP = 105
# DB to store the data about the users
# IMPORTANT!!! Must be only accesible by the user which executes this script!!!
# ALSO IMPORTANT!!! Must be only stored in persistent storage (be careful in containers)
DB_USERS_FILENAME = "/tmp/users.db"
# Prefix to add to all user names created
USER_PREFIX = "tts"


def create_one_user(username, group, oidc, password):
    """
    Create the specified user in the OpenNebula site
    """
    server = xmlrpclib.ServerProxy(ONE_API_ENDPOINT, allow_none=True)

    template = ('ISS="%s"\nSUB="%s"\nName="%s"' % (oidc['iss'],
                                                   oidc['sub'],
                                                   oidc['name']))

    success, userid, _ = server.one.user.allocate(SESSIONID, username, password, "core")
    if not success:
        return False, userid

    success, msg, _ = server.one.user.update(SESSIONID, userid, template, 0)
    if not success:
        delete_one_user(userid)
        return False, msg

    success, msg, _ = server.one.user.chgrp(SESSIONID, userid, group)
    if not success:
        delete_one_user(userid)
        return False, msg

    return True, userid


def delete_one_user(userid):
    """
    Delete the specified user from the OpenNebula site
    """
    server = xmlrpclib.ServerProxy(ONE_API_ENDPOINT, allow_none=True)

    success, userid, _ = server.one.user.delete(SESSIONID, userid)
    if not success:
        return False, userid
    return True, ""


def id_generator(size=16, chars=string.ascii_uppercase + string.digits + string.ascii_lowercase):
    """
    Genererate a random password
    """
    return ''.join(random.choice(chars) for _ in range(size))


def user_exist(username):
    """
    Check if a username exists in the OpenNebula site, and return the ID
    """
    server = xmlrpclib.ServerProxy(ONE_API_ENDPOINT, allow_none=True)

    success, userpool, _ = server.one.userpool.info(SESSIONID)
    if not success:
        return False, userpool

    userpool_info = parseString(userpool)
    for user in userpool_info.getElementsByTagName("USER"):
        user_name = user.getElementsByTagName('NAME')[0].firstChild.nodeValue.strip()
        user_id = int(user.getElementsByTagName('ID')[0].firstChild.nodeValue.strip())
        if username == user_name:
            return True, user_id

    return False, ""


def init_db():
    """
    Initialize the users DB
    """
    connection = sqlite3.connect(DB_USERS_FILENAME)
    cursor = connection.cursor()
    sql = 'select name from sqlite_master where type="table" and name="one_users"'
    cursor.execute(sql)
    res = cursor.fetchall()
    if (len(res) == 0):
        cursor.execute("CREATE TABLE one_users(username VARCHAR(255) PRIMARY KEY, password VARCHAR(255))")
        connection.commit()
        connection.close()
        return True

    connection.close()
    return False


def save_user_data(username, password):
    """
    Save the user data into the DB
    """
    init_db()

    connection = sqlite3.connect(DB_USERS_FILENAME)
    cursor = connection.cursor()
    sql = 'insert into one_users values ("%s", "%s")' % (username, password)
    cursor.execute(sql)
    connection.commit()
    connection.close()

    return True


def delete_user_data(username):
    """
    Delete the user data from the DB
    """
    created = init_db()
    if created:
        return False
    else:
        connection = sqlite3.connect(DB_USERS_FILENAME)
        cursor = connection.cursor()
        sql = 'delete from one_users where username = "%s"' % username
        cursor.execute(sql)
        connection.commit()
        connection.close()
        return True


def get_user_password(username):
    """
    Get the user password stored in the DB
    """
    created = init_db()
    if created:
        return None
    else:
        connection = sqlite3.connect(DB_USERS_FILENAME)
        cursor = connection.cursor()
        sql = 'select password from one_users where username = "%s"' % username
        cursor.execute(sql)
        res = cursor.fetchall()
        connection.close()
        if len(res) > 0:
            return res[0][0]
        else:
            return None


def create_user(username, group, oidc):
    """
    Create a new user and return the appropriate credentials
    """
    exists, user_id = user_exist(username)
    if not exists:
        password = id_generator()
        success, userid = create_one_user(username, group, oidc, password)
        if not success:
            return json.dumps({'error': 'Error creating user: %s' % userid})

        save_user_data(username, password)
    else:
        password = get_user_password(username)
        if not password:
            # This case only happens in case of previous error
            # The user has been created without storing the password in the DB
            # First delete the incorrect user data from DB
            delete_user_data(username)
            # Generate new password
            password = id_generator()
            # Update in ONE
            server = xmlrpclib.ServerProxy(ONE_API_ENDPOINT, allow_none=True)
            success, user, _ = server.one.user.passwd(SESSIONID, user_id, password)
            if not success:
                return json.dumps({'error': 'Error setting user password: %s' % user})
            else:
                # And save in the DB
                save_user_data(username, password)

    credential = [{'name': 'Username', 'type': 'text', 'value': username},
                  {'name': 'Password', 'type': 'text', 'value': password}]
    return json.dumps({'credential': credential, 'state': username})


def revoke_user(username):
    """
    Revoke user credentials
    """
    exists, userid = user_exist(username)
    if exists:
        success, msg = delete_one_user(userid)
        if success:
            delete_user_data(username)
            return json.dumps({'result': 'ok'})
        else:
            return json.dumps({'error': msg})
    else:
        if userid == "":
            return json.dumps({'result': 'ok'})
        else:
            return json.dumps({'error': userid})


def process_request(request):
    """
    Process the TTS request from the json data provided
    """
    json_data = str(request) + '=' * (4 - len(request) % 4)
    jobject = json.loads(str(base64.urlsafe_b64decode(json_data)))

    action = jobject['action']
    user_info = jobject['user_info']
    oidc = user_info['oidc']
    iss = urlparse.urlparse(oidc['iss'])
    iss_host = iss[1]
    username = "%s_%s_%s" % (USER_PREFIX, iss_host, oidc['sub'])

    if action == "request":
        return create_user(username, USERS_GROUP, oidc)
    elif action == "revoke":
        state = jobject['cred_state']
        if state != username:
            return json.dumps({"error": "incorrect_state"})
        else:
            return revoke_user(state)
    else:
        return json.dumps({"error": "unknown_action", "details": action})


def main():
    try:
        if len(sys.argv) == 2:
            print process_request(sys.argv[1])
        else:
            print json.dumps({"error": "no_parameter"})
    except Exception, E:
        TraceBack = traceback.format_exc(),
        print json.dumps({"error": "exception", "details": str(E), "trace": TraceBack})

if __name__ == "__main__":
    main()
