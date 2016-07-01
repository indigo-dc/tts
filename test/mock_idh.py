#!/usr/bin/env python2
# -*- coding: utf-8 -*-
#
# Dummy idenitity harmonization that always returns the current user.
# For debugging and testing purposes only.
#

import pwd, json, sys, os, traceback


def oidc_lookup(Issuer, Subject, Create):
    return json.dumps({
        "uid": "you",
        "uidNumber": 1000,
        "gidNumber": 1000,
        "homeDirectory": "/home/you",
        "userIds": [[Issuer, Subject]]
    })


def main():
    try:
        Cmd = None
        if len(sys.argv) > 1:
            Cmd = sys.argv[1]

        if Cmd == "OpenIdConnect":
            Issuer = sys.argv[2]
            Subject = sys.argv[3]
            print oidc_lookup(Issuer, Subject, True)
        else:
            # Other commands should not be executed, since the
            # former always succeedes
            print json.dumps({"error": "unknown command"})
    except Exception, E:
        TraceBack = traceback.format_exc(),
        print json.dumps({"error": "exception", "details": str(E), "trace": TraceBack})


if __name__ == "__main__":
    main()
