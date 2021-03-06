#!/usr/bin/env python2
# -*- coding: utf-8 -*-

import urlparse
import json
import base64
import random
import string
import sys
import traceback
import time
import os

VERSION="0.0.1"

def list_params():
    RequestParams = [
                     [{"key":"missing_result", "name":"missing result", "type":"textarea", "description":"send this and it will cause an error", "mandatory":True}],
                     [{"key":"bad_json", "name":"create bad json", "type":"textarea", "description":"send this and it will cause an error", "mandatory":True}],
                     [{"key":"no_json_map", "name":"create json, but not an object", "type":"textarea", "description":"send this and it will cause an error", "mandatory":True}],
                     [{"key":"bad_result", "name":"create bad result", "type":"textarea", "description":"send this and it will cause an error", "mandatory":True}],
                     [{"key":"bad_error", "name":"create bad error", "type":"textarea", "description":"send this and it will cause an error", "mandatory":True}],
                     []]
    ConfParams = []
    Email = "Bas.Wegh@kit.edu"
    Config = {'result':'ok', 'conf_params': ConfParams, 'request_params': RequestParams, 'version':VERSION, 'developer_email':Email}
    return json.dumps(Config)


def request(JObject):
    Params = JObject['params']
    if Params.has_key('missing_result') :
        return json.dumps({'credential': [], 'state': 'test'})
    elif Params.has_key('bad_json') :
        return "no json"
    elif Params.has_key('no_json_map') :
        return "[3,4,1,\"no json map\"]"
    elif Params.has_key('bad_result') :
        return json.dumps({'result':'ok', 'credential': []})
    elif Params.has_key('bad_error') :
        return json.dumps({'result':'error', 'log_msg': "this is missing the user message"})

    return json.dumps({'result':'ok', 'credential': [], 'state': 'test'})


def revoke(JObject):
    return json.dumps({'result': 'ok'})


def get_jobject():
        Data = ""
        if 'WATTS_PARAMETER' in os.environ:
            Data = os.environ['WATTS_PARAMETER']
        elif len(sys.argv) == 2:
            Data = sys.argv[1]
        else:
            return None
        Json = str(Data)+ '=' * (4 - len(Data) % 4)
        JObject = json.loads(str(base64.urlsafe_b64decode(Json)))
        return JObject



def main():
    try:
        UserMsg = "Internal error, please contact the administrator"

        JObject = get_jobject()
        if JObject != None:
            Action = JObject['action']
            if Action == "parameter":
                print list_params()
            elif Action == "request":
                print request(JObject)
            elif Action == "revoke":
                print revoke(JObject)
            else:
                print json.dumps({"result":"error", "user_msg":"unknown_action"})
        else:
            print json.dumps({"result":"error", "user_msg":"no_parameter"})
    except Exception, E:
        TraceBack = traceback.format_exc(),
        LogMsg = "the plugin failed with %s - %s"%(str(E), TraceBack)
        print json.dumps({'result':'error', 'user_msg':UserMsg, 'log_msg':LogMsg})

if __name__ == "__main__":
    main()
