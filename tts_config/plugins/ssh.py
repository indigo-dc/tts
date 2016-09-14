#!/usr/bin/env python2
# -*- coding: utf-8 -*-

import json
import base64
import sys
import os
import traceback
import string
import random
from pwd import getpwnam

STATE_PREFIX="TTS_"

# change this to False to enable real user login
OVERRIDE_USER_NAME={{plugin_ssh_override_user}}
# change this to True, to enable SSH
FULL_ACCESS=False

def create_ssh(UserName, Uid, Gid, HomeDir):
    UserExists = does_user_exist(UserName,Uid,Gid,HomeDir)
    if UserExists:
        return create_ssh_for(UserName,HomeDir)
    else:
        # dear admin, this is not the problem of tts
        return json.dumps({'error':'no_user'})


def revoke_ssh(UserName, Uid, Gid, HomeDir, State):
    UserExists = does_user_exist(UserName,Uid,Gid,HomeDir)
    if UserExists:
        return delete_ssh_for(UserName,HomeDir,State)
    else:
        # dear admin, this is not the problem of tts
        return json.dumps({'error':'no_user'})


def create_ssh_for(UserName,HomeDir):
    SshDir = create_ssh_dir(UserName,HomeDir)
    if SshDir == None:
        return json.dumps({'error':'ssh_dir_missing'})
    Password = id_generator()
    State = "%s%s"%(STATE_PREFIX,id_generator(32))
    # maybe change this to random/temp file
    OutputFile = os.path.join(SshDir,"tts_ssh_key")
    AuthorizedFile = os.path.join(SshDir,"authorized_keys")
    DelKey = "rm -f %s %s.pub > /dev/null"%(OutputFile,OutputFile)
    # DelKey = "srm -f %s %s.pub > /dev/null"%(OutputFile,OutputFile)
    # DelKey = "shred -f %s %s.pub > /dev/null"%(OutputFile,OutputFile)
    os.system(DelKey)
    Cmd = "ssh-keygen -N %s -C %s -f %s > /dev/null"%(Password,State,OutputFile)
    if os.system(Cmd) != 0:
        return json.dumps({'error':'keygen_failed'})

    if FULL_ACCESS:
        Prepend='no-port-forwarding,no-X11-forwarding,no-agent-forwarding '
    else:
        #make this a really locked down ssh access
        Prepend='command="cat {{platform_etc_dir}}/services/ssh.msg",no-port-forwarding,no-X11-forwarding,no-agent-forwarding,no-pty '
    Cmd = "echo -n '%s' |  cat - %s.pub >> %s"%(Prepend,OutputFile,AuthorizedFile)
    os.system(Cmd)
    PrivKey = get_file_content(OutputFile),
    os.system(DelKey)
    UserNameObj = {'name':'Username', 'type':'text', 'value':UserName}
    PrivKeyObj = {'name':'Private Key', 'type':'textfile', 'value':PrivKey, 'rows':'30', 'cols':'64'}
    PasswdObj = {'name':'Passphrase (for Private Key)', 'type':'text', 'value':Password}
    Credential = [UserNameObj, PrivKeyObj, PasswdObj]
    return json.dumps({'credential':Credential, 'state':State})


def delete_ssh_for(UserName, HomeDir, State):
    SshDir = create_ssh_dir(UserName,HomeDir)
    if SshDir == None:
        return json.dumps({'error':'ssh_dir_missing'})
    AuthorizedFile = os.path.join(SshDir,"authorized_keys")
    BackupFile = "%s%s"%(AuthorizedFile,".backup")
    TempFile = "%s%s"%(AuthorizedFile,".tts")
    Copy = "cp %s %s"%(AuthorizedFile,BackupFile)
    Remove = "grep -v %s %s > %s"%(State,BackupFile,AuthorizedFile)
    Delete = "rm -f %s"%BackupFile
    Res1 = os.system(Copy)
    Res2 = os.system(Remove)
    Res3 = os.system(Delete)
    if ( (Res1 != 0) or (Res2 != 0 and Res2 != 256) or (Res3 != 0) ):
        return json.dumps({'error':'delete_failed', "details":{"copy":Res1, "remove":Res2, "delete":Res3 }})

    return json.dumps({'result':'ok'})





def get_file_content(File):
    fo = open(File)
    Content = fo.read()
    fo.close()
    return Content



def create_ssh_dir(UserName,HomeDir):
    SshDir=os.path.join(HomeDir,".ssh/")
    AuthorizedFile=os.path.join(SshDir,"authorized_keys")
    if not os.path.exists(SshDir):
        CreateSshDir = "mkdir -p %s"%SshDir
        if os.system(CreateSshDir) != 0:
            return None
    if not os.path.exists(AuthorizedFile):
        CreateFile = "touch %s"%AuthorizedFile
        if os.system(CreateFile) != 0:
            return None

    # always enfore mod and ownership
    ChOwnSshDir = "chown %s %s"%(UserName,SshDir)
    ChModSshDir = "chmod 700 %s"%SshDir
    ChOwnAuthFile = "chown %s %s"%(UserName,AuthorizedFile)
    ChModAuthFile = "chmod 600 %s"%AuthorizedFile
    if os.system(ChOwnSshDir) != 0:
        return None
    if os.system(ChModSshDir) != 0:
        return None
    if os.system(ChModAuthFile) != 0:
        return None
    if os.system(ChOwnAuthFile) != 0:
        return None

    return SshDir



def does_user_exist(UserName, Uid, Gid, HomeDir):
    # user has been created
    try:
        SysUid = getpwnam(UserName).pw_uid
        SysGid = getpwnam(UserName).pw_gid
        SysHomeDir = getpwnam(UserName).pw_dir
        UidOk =  (SysUid == Uid)
        GidOk = (SysGid == Gid)
        DirOk = (SysHomeDir == HomeDir)
        return UidOk and GidOk and DirOk
    except Exception:
        return False


def id_generator(size=16, chars=string.ascii_uppercase + string.digits+string.ascii_lowercase):
    return ''.join(random.choice(chars) for _ in range(size))


def main():
    try:
        Cmd = None
        if len(sys.argv) == 2:
            Json = str(sys.argv[1])+ '=' * (4 - len(sys.argv[1]) % 4)
            JObject = json.loads(str(base64.urlsafe_b64decode(Json)))

            #general information
            Action = JObject['action']
            State = JObject['cred_state']
            Params = JObject['params']
            UserInfo = JObject['user_info']
            Site = UserInfo['site']
            #Oidc = UserInfo['oidc']

            # information coming from the site
            # uid - the username
            # uidNumber - the uid of the user
            # gidNumber - the gid of the primary group of the user
            # homeDirectory - the home directory of the user
            UserName = Site['uid']
            Uid = Site['uidNumber']
            Gid = Site['gidNumber']
            HomeDir = Site['homeDirectory']

            # information coming from the openid provider
            # which information are available depends on the
            # OpenId Connect provider
            #
            # iss - the issuer
            # sub - the subject
            # name - the full name of the user
            # email - the email of the user
            #
            # IAM also provides
            # groups - a list of groups each consisting of
            #    id - uuid of the group
            #    name - readable name of the group
            # organisation_name - name of the organisation, indigo_dc
            # preferred_username - if possible create accounts with this name
            # Name = Oidc['name']
            # OidcUserName = Oidc['preferred_username']

            if OVERRIDE_USER_NAME:
                # override username, uid, gid and homedir in DEMO mode
                UserName = "{{runner_user}}"
                Uid = getpwnam(UserName).pw_uid
                Gid = getpwnam(UserName).pw_gid
                HomeDir = getpwnam(UserName).pw_dir

            if Action == "request":
                print create_ssh(UserName, Uid, Gid, HomeDir)
            elif Action == "revoke":
                print revoke_ssh(UserName, Uid, Gid, HomeDir, State)
            else:
                print json.dumps({"error":"unknown_action", "details":Action})
        else:
            print json.dumps({"error":"no_parameter"})
    except Exception, E:
        TraceBack = traceback.format_exc(),
        print json.dumps({"error":"exception", "details": str(E), "trace":TraceBack})
        pass

if __name__ == "__main__":
    main()
