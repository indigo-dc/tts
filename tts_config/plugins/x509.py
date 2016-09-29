#!/usr/bin/env python2
# -*- coding: utf-8 -*-

import json
import base64
import sys
import os
import traceback
import string
import random

CA_BASE="{{platform_data_dir}}/tts_ca"
CA_SUBJECT="/C=EU/O=INDIGO/OU=TTS/CN=TTS-CA"
CERT_SUBJECT="/C=EU/O=INDIGO/OU=TTS/CN=%s@%s"
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
crldir            = \$dir/crl
crl               = \$crldir/crl.pem
crlnumber         = \$crldir/crlnumber
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

ISSUER_MAPPING=""" { "https://iam-test.indigo-datacloud.eu/":"indigo-iam-test",
                     "https://accounts.google.com":"google"
                   } """

def create_cert(Subject, Issuer, NumDaysValid):
    init_ca_if_needed()
    Serial = read_serial()
    return issue_certificate(Subject, Issuer, NumDaysValid, Serial)

def revoke_cert(Serial):
    init_ca_if_needed()
    return revoke_certificate(Serial)

def issue_certificate(Subject, Issuer, NumDaysValid, Serial):
    ShortIss = shorten_issuer(Issuer)
    if ShortIss == None:
        return json.dumps({'error':'unknown issuer'})

    AbsBase = CA_ABS_BASE
    Issuer = string.rstrip(Issuer, "/")
    Password = id_generator(32)
    CertSubject = CERT_SUBJECT%(Subject, ShortIss)
    AltSub = "subjectAltName = URI:%s/%s"%(Issuer, Subject)
    CAPassFile = "%s/private/pass"%(AbsBase)
    CACertFile = "%s/certs/cacert.pem"%(AbsBase)
    CertFile = "%s/certs/usercert_%s.pem"%(AbsBase, Serial)
    CsrFile = "%s/users/csr/userreq_%s.pem"%(AbsBase, Serial)
    KeyFile = "%s/users/private/userkey_%s.pem"%(AbsBase, Serial)
    PassFile = "%s/users/private/userpass_%s"%(AbsBase, Serial)
    TmpConfFile = "%s/users/private/userconf_%s"%(AbsBase, Serial)
    ConfFile = "%s/openssl.conf"%(AbsBase)
    LogFile = "%s/users/private/userlog_%s"%(AbsBase, Serial)
    Cmd = "echo -n \"%s\" > %s"%(Password, PassFile)

    if os.system(Cmd) != 0:
        return json.dumps({'error':'userpass_failed'})

    Cmd = "openssl req -newkey rsa:1024 -keyout %s -sha256 -out %s -subj \"%s\" -passout file:%s >> %s 2>&1"%(KeyFile, CsrFile, CertSubject, PassFile, LogFile)
    Log = "echo %s > %s"%(Cmd, LogFile)
    os.system(Log)
    if os.system(Cmd) != 0:
        return json.dumps({'error':'csr_failed'})

    Cmd = "cp %s %s"%(ConfFile, TmpConfFile)
    if os.system(Cmd) != 0:
        return json.dumps({'error':'conf_failed'})

    Cmd = "echo \"%s\" >> %s"%(AltSub, TmpConfFile)
    if os.system(Cmd) != 0:
        return json.dumps({'error':'conf_update_failed'})

    Cmd = "openssl ca -batch -config %s -days %s -policy policy_anything -extensions usr_cert -out %s -passin file:%s -infiles %s >> %s 2>&1"%(TmpConfFile, NumDaysValid, CertFile, CAPassFile, CsrFile, LogFile)
    Log = "echo %s >> %s"%(Cmd, LogFile)
    os.system(Log)
    if os.system(Cmd) != 0:
        return json.dumps({'error':'sign_failed'})
    Cert = get_file_content(CertFile)
    CACert = get_file_content(CACertFile)
    PrivKey = get_file_content(KeyFile)
    # Cmd = "shred --remove=wipe %s %s"%(PassFile, KeyFile)
    Cmd = "rm %s %s"%(PassFile, KeyFile)
    if os.system(Cmd) != 0:
        return json.dumps({'error':'purge_files'})
    CertObj = {'name':'Certificate', 'type':'textfile', 'value':Cert, 'rows':'30', 'cols':'64'}
    PrivKeyObj = {'name':'Private Key', 'type':'textfile', 'value':PrivKey, 'rows':'21', 'cols':'64'}
    PasswdObj = {'name':'Passphrase (for Private Key)', 'type':'text', 'value':Password}
    CACertObj = {'name':'CA Certificate', 'type':'textfile', 'value':CACert, 'rows':'21', 'cols':'64'}
    Credential = [CertObj, PrivKeyObj, PasswdObj, CACertObj]
    return json.dumps({'credential':Credential, 'state':Serial})


def shorten_issuer(Issuer):
    IssuerDict = json.loads(ISSUER_MAPPING.replace("\n",""))
    if Issuer in IssuerDict:
        return IssuerDict[Issuer]
    return None

def revoke_certificate(Serial):
    AbsBase = CA_ABS_BASE
    LogFile = "%s/users/private/userlog_%s"%(AbsBase, Serial)
    CertFile = "%s/certs/usercert_%s.pem"%(AbsBase, Serial)
    ConfFile = "%s/openssl.conf"%(AbsBase)
    CAPassFile = "%s/private/pass"%(AbsBase)
    CrlFile = "%s/crl/crl.pem"%(AbsBase)
    ConfigPass = "-config %s -passin file:%s"%(ConfFile, CAPassFile)
    Cmd = "openssl ca %s -revoke %s >> %s 2>&1"%(ConfigPass, CertFile, LogFile)
    if os.system(Cmd) != 0:
        return json.dumps({'error':'revoke'})
    Cmd = "openssl ca -gencrl %s -out %s >> %s 2>&1"%(ConfigPass, CrlFile, LogFile)
    if os.system(Cmd) != 0:
        return json.dumps({'error':'crl'})
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
    os.mkdir("%s/crl"%(AbsBase))

    LogFile = "%s/private/ca.log"%(AbsBase)
    Cmd = "touch %s/index.txt > /dev/null"%(AbsBase)
    if os.system(Cmd) != 0:
        return json.dumps({'error':'touch_failed'})
    Cmd = "echo \"01\" > %s/serial"%(AbsBase)
    if os.system(Cmd) != 0:
        return json.dumps({'error':'serial_failed'})
    Cmd = "echo \"01\" > %s/crl/crlnumber"%(AbsBase)
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

            NumDaysValid = "11"

            if Action == "request":
                print create_cert(Subject, Issuer, NumDaysValid)
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
