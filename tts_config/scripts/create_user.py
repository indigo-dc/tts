#!/usr/bin/python
# -*- coding: utf-8 -*-

import os
import sys 
from grp import getgrnam

def create_account(UserName):
    # create a user with:
    # main group indigo_users
    # expiredate 2016/04/06
    # with their own home directory 
    # with no dedicated group 
    # the shell /bin/sh 
    # and uid of 2000+ 
    GroupExists = ensure_group_exists("tts_user")
    if GroupExists == False:
        sys.exit(2)

    Cmd = '/usr/sbin/useradd -c "INDIGO USER created by IDH"'
    Cmd = Cmd + ' -g tts_user --create-home --no-user-group'
    Cmd = Cmd + ' --shell /bin/sh --key UID_MIN=2000 %s > /dev/null'%UserName
    Result = os.system(Cmd)
    if Result == 0:
        # user has been created
        sys.exit(0)
    else:
        sys.exit(1)

def ensure_group_exists(GroupName):
    try:
        getgrnam(GroupName)
        return True
    except Exception:
        Cmd = '/usr/sbin/groupadd --key GID_MIN=2000 %s > /dev/null'%GroupName
        Result = os.system(Cmd)
        if Result == 0:
            return True
        pass
    return False

def main():
    try:
        Cmd = None
        if len(sys.argv) == 2:
            UserName = sys.argv[1]
            create_account(UserName)
        else:
            sys.exit(254)
    except Exception, E:
        sys.exit(255)


if __name__ == "__main__":
    main()
