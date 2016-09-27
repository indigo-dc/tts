#!/usr/bin/env python2
# -*- coding: utf-8 -*-

import urlparse
import json
import base64
import random
import string
import sys
import traceback

def list_params():
    RequestParams = []
    ConfParams = []
    return json.dumps({'conf_params': ConfParams, 'request_params': RequestParams})

def request_info(UserInfo):
    Credential = info_to_credential(UserInfo)
    return json.dumps({'credential': Credential, 'state': 'user_info'})

def info_to_credential(UserInfo):
    Site = UserInfo['site']
    Oidc = UserInfo['oidc']
    SiteInfo = site_to_credential(Site)
    OidcInfo = oidc_to_credential(Oidc)
    return SiteInfo + OidcInfo

def site_to_credential(Site):
    UserName = Site['uid']
    Uid = Site['uidNumber']
    Gid = Site['gidNumber']
    HomeDir = Site['homeDirectory']

    NameObj = {'name':'Site-Username', 'type':'text', 'value':UserName}
    UidObj = {'name':'Site-Uid', 'type':'text', 'value':Uid}
    GidObj = {'name':'Site-Gid', 'type':'text', 'value':Gid}
    DirObj = {'name':'Site-HomeDirectory', 'type':'text', 'value':HomeDir}
    SiteCredential = [NameObj, UidObj, GidObj, DirObj]
    return SiteCredential

def oidc_to_credential(Oidc):
    OidcCredential = []
    for Key in Oidc:
        KeyName = oidc_key_to_name(Key)
        Type = oidc_key_to_type(Key)
        Name = "Oidc-%s"%KeyName
        Value = Oidc[Key]
        NewObj = [{'name':Name, 'type':Type, 'value':Value  }]
        OidcCredential = OidcCredential + NewObj
    return OidcCredential

def oidc_key_to_name(Key):
    if Key == "iss":
        return "Issuer"
    if Key == "sub":
        return "Subject"
    if Key == "name":
        return "Name"
    if Key == "groups":
        return "Groups"
    if Key == "email":
        return "E-Mail"
    return Key


def oidc_key_to_type(Key):
    if Key == "groups":
        return "textarea"
    return "text"

def revoke_info():
    return json.dumps({'result': 'ok'})


def main():
    try:
        if len(sys.argv) == 2:
            json_data = str(sys.argv[1]) + '=' * (4 - len(sys.argv[1]) % 4)
            jobject = json.loads(str(base64.urlsafe_b64decode(json_data)))
            action = jobject['action']

            if Action == "get_params":
                print list_params()

            else:
                user_info = jobject['user_info']
                if action == "request":
                    print request_info(user_info)
                elif action == "revoke":
                    print revoke_info()
                else:
                    print json.dumps({"error": "unknown_action", "details": action})
        else:
            print json.dumps({"error": "no_parameter"})
    except Exception, E:
        TraceBack = traceback.format_exc(),
        print json.dumps({"error": "exception", "details": str(E), "trace": TraceBack})

if __name__ == "__main__":
    main()
