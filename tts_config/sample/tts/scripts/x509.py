#!/usr/bin/env python2
# -*- coding: utf-8 -*-

import json
import base64
import sys
import os
import traceback
import string
import random

CA_BASE="~/.config/tts_ca"
CA_SUBJECT="/C=DE/O=KIT/OU=INDIGO/CN=TTS-CA"
CERT_SUBJECT="/C=DE/O=KIT/OU=INDIGO/CN=%s@%s_%s"
OPENSSL_CONF = """
[ ca ]
default_ca = CA_default 

[ CA_default ]
dir               = %s
new_certs_dir     = \$dir/certs
database          = \$dir/index.txt
certificate       = \$dir/certs/cacert.pem
serial            = \$dir/serial
private_key       = \$dir/private/cakey.pem
default_days      = 365
default_crl_days  = 30
default_md        = sha1
preserve          = no

[ policy_anything ]
countryName             = optional
organizationName        = optional
organizationalUnitName  = optional
commonName              = optional

[ usr_cert ]
subjectKeyIdentifier    =hash
authorityKeyIdentifier  =keyid, issuer
basicConstraints        =CA:FALSE
keyUsage                =digitalSignature, keyEncipherment
"""
CA_ABS_BASE=os.path.abspath(os.path.expanduser(CA_BASE))

def create_cert(Subject, Issuer):
    init_ca_if_needed()
    Serial = read_serial()
    return issue_certificate(Subject, Issuer, Serial)

def issue_certificate(Subject, Issuer, Serial):
    AbsBase = CA_ABS_BASE
    IssuerDomain = string.rsplit(Issuer, "/", 1)[-1]
    Subject = CERT_SUBJECT%(Subject, IssuerDomain, Serial)
    Password = id_generator(32)
    CAPassFile = "%s/private/pass"%(AbsBase)
    CertFile = "%s/certs/usercert_%s.pem"%(AbsBase, Serial)
    CsrFile = "%s/users/csr/userreq_%s.pem"%(AbsBase, Serial)
    KeyFile = "%s/users/private/userkey_%s.pem"%(AbsBase, Serial)
    PassFile = "%s/users/private/userpass_%s"%(AbsBase, Serial)
    ConfFile = "%s/openssl.conf"%(AbsBase)
    LogFile = "%s/users/private/userlog_%s"%(AbsBase, Serial)
    Cmd = "echo -n \"%s\" > %s"%(Password, PassFile)
    if os.system(Cmd) != 0:
        return json.dumps({'error':'userpass_failed'})
    Cmd = "openssl req -newkey rsa:1024 -keyout %s -sha256 -out %s -subj \"%s\" -passout file:%s >> %s 2>&1"%(KeyFile, CsrFile, Subject, PassFile, LogFile) 
    Log = "echo %s > %s"%(Cmd, LogFile) 
    os.system(Log)
    if os.system(Cmd) != 0:
        return json.dumps({'error':'csr_failed'})
    Cmd = "openssl ca -batch -config %s -policy policy_anything -extensions usr_cert -out %s -passin file:%s -infiles %s >> %s 2>&1"%(ConfFile, CertFile, CAPassFile, CsrFile, LogFile)
    Log = "echo %s >> %s"%(Cmd, LogFile) 
    os.system(Log)
    if os.system(Cmd) != 0:
        return json.dumps({'error':'sign_failed'})
    Cert = get_file_content(CertFile)
    PrivKey = get_file_content(KeyFile)
    Cmd = "rm %s %s"%(PassFile, KeyFile)
    if os.system(Cmd) != 0:
        return json.dumps({'error':'purge_files'})
    CertObj = {'name':'Certificate', 'type':'textfile', 'value':Cert, 'rows':'30', 'cols':'64'}
    PrivKeyObj = {'name':'Private Key', 'type':'textfile', 'value':PrivKey, 'rows':'30', 'cols':'64'}
    PasswdObj = {'name':'Passphrase (for Private Key)', 'type':'text', 'value':Password}
    Credential = [CertObj, PrivKeyObj, PasswdObj]
    return json.dumps({'credential':Credential, 'state':Serial})


def revoke_cert(SerialNumber):
    init_ca_if_needed()
    #just return okay for now, will add revocation later
    return json.dumps({'result':'ok'})

def init_ca_if_needed():
    if not os.path.isdir(CA_ABS_BASE):
        init_ca()
        
def read_serial():
    SerialFile = "%s/serial"%(CA_ABS_BASE)
    Serial = get_file_content(SerialFile).rstrip('\n')
    return Serial
    

def init_ca():
    AbsBase = CA_ABS_BASE
    os.mkdir("%s"%(AbsBase))
    os.mkdir("%s/certs"%(AbsBase))
    os.mkdir("%s/private"%(AbsBase))
    os.mkdir("%s/proxies"%(AbsBase))
    os.mkdir("%s/users"%(AbsBase))
    os.mkdir("%s/users/csr"%(AbsBase))
    os.mkdir("%s/users/private"%(AbsBase))

    LogFile = "%s/private/ca.log"%(AbsBase)
    Cmd = "touch %s/index.txt > /dev/null"%(AbsBase)
    if os.system(Cmd) != 0:
        return json.dumps({'error':'touch_failed'})
    Cmd = "echo \"01\" > %s/serial"%(AbsBase)
    if os.system(Cmd) != 0:
        return json.dumps({'error':'serial_failed'})
    Config = OPENSSL_CONF%(AbsBase)
    Cmd = "echo \"%s\" > %s/openssl.conf"%(Config, AbsBase)
    if os.system(Cmd) != 0:
        return json.dumps({'error':'config_failed'})
    Password = id_generator(32)
    Cmd = "echo -n \"%s\" > %s/private/pass"%(Password, AbsBase)
    if os.system(Cmd) != 0:
        return json.dumps({'error':'pass_failed'})
    Cmd = "openssl req -x509 -newkey rsa:2048 -keyout %s/private/cakey.pem -sha256 -days 3650 -out %s/certs/cacert.pem -subj '%s' -passout file:%s/private/pass -set_serial 0 > %s 2>&1"%(AbsBase, AbsBase, CA_SUBJECT, AbsBase, LogFile)
    if os.system(Cmd) != 0:
        return json.dumps({'error':'openssl_failed'})

def get_file_content(File):
    fo = open(File)
    Content = fo.read()
    fo.close()
    return Content
    
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
            # Site = UserInfo['site']
            Oidc = UserInfo['oidc']

            # information coming from the site
            # uid - the username
            # uidNumber - the uid of the user 
            # gidNumber - the gid of the primary group of the user 
            # homeDirectory - the home directory of the user 
            # UserName = Site['uid']
            # Uid = Site['uidNumber']
            # Gid = Site['gidNumber']
            # HomeDir = Site['homeDirectory']

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
            Issuer = Oidc['iss']
            Subject = Oidc['sub']
            # OidcUserName = Oidc['preferred_username']

            if Action == "request":
                print create_cert(Subject, Issuer) 
            elif Action == "revoke":
                print revoke_cert(State) 
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
