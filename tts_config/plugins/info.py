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
    OidcCredential = []
    for Key in UserInfo:
        KeyName = oidc_key_to_name(Key)
        Type = oidc_key_to_type(Key)
        Name = "%s"%KeyName
        Value = UserInfo[Key]
        NewObj = [{'name':Name, 'type':Type, 'value':Value  }]
        OidcCredential = OidcCredential + NewObj
    return OidcCredential

def oidc_key_to_name(Key):
    if Key == "iss":
        return "Issuer"
    if Key == "userid":
        return "UserId (encoded iss/sub)"
    if Key == "sub":
        return "Subject"
    if Key == "name":
        return "Name"
    if Key == "groups":
        return "Groups"
    if Key == "email":
        return "E-Mail"
    if Key == "gender":
        return "Gender"
    return Key


def oidc_key_to_type(Key):
    if Key == "groups":
        return "textarea"
    if Key == "userid":
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

            if action == "get_params":
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
