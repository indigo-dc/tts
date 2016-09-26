import sys
import os
import unittest
import json
import base64
from mock import patch, MagicMock

dir_path = os.path.dirname(os.path.realpath(__file__))
print dir_path
sys.path.append(os.path.join(dir_path, "../tts_config/plugins"))

from opennebula import process_request, DB_USERS_FILENAME


class TestONEPlugin(unittest.TestCase):

    @patch('xmlrpclib.ServerProxy')
    def test_01_create_cred(self, server_proxy):
        if os.path.exists(DB_USERS_FILENAME):
            os.remove(DB_USERS_FILENAME)
        req = """{
                "action": "request",
                "cred_state": "undefined",
                "gid": 0,
                "home_dir": "/root",
                "params": [
                ],
                "uid": 0,
                "user": "root",
                "user_info": {
                    "oidc": {
                        "email": "micafer1@upv.es",
                        "iss": "https://iam-test.indigo-datacloud.eu/",
                        "name": "Miguel Caballer",
                        "sub": "dc5d5ab7-6db9-4079-985c-80ac05017066",
                        "email_verified": true,
                        "family_name": "Caballer",
                        "given_name": "Miguel",
                        "groups": [
                            {
                                "id": "54e3843d-2b9d-45df-a76d-03bdf2fe46a2",
                                "name": "Users"
                            },
                            {
                                "id": "19a8dd29-2b8d-4efd-85cf-f8091037d51f",
                                "name": "Developers"
                            }
                        ],
                        "organisation_name": "indigo-dc",
                        "preferred_username": "micafer",
                        "updated_at": "Fri Jun 17 13:52:32 UTC 2016"
                    },
                    "site": {
                        "gidNumber": 0,
                        "homeDirectory": "/root",
                        "uid": "root",
                        "uidNumber": 0,
                        "userIds": {
                            "https://iam-test.indigo-datacloud.eu/": "dc5d5ab7-6db9-4079-985c-80ac05017066"
                        }
                    },
                    "uid": "root"
                }
            }"""
        req = base64.b64encode(req)

        one_server = MagicMock()
        one_server.one.user.allocate.return_value = (True, 1, 0)
        one_server.one.user.update.return_value = (True, 1, 0)
        one_server.one.user.chgrp.return_value = (True, 1, 0)
        user_xml = """
            <USER>
                <NAME>username</NAME>
                <ID>0</ID>
            </USER>
        """
        one_server.one.userpool.info.return_value = (True, user_xml, 0)
        server_proxy.return_value = one_server

        res = json.loads(process_request(req))
        self.assertEqual(res["state"], "tts_iam-test.indigo-datacloud.eu_dc5d5ab7-6db9-4079-985c-80ac05017066")
        pass_ret = False
        username_ret = False
        for elem in res["credential"]:
            if elem["name"] == "Username":
                self.assertEqual(elem["value"], "tts_iam-test.indigo-datacloud.eu_dc5d5ab7-6db9-4079-985c-80ac05017066")
                username_ret = True
            elif elem["name"] == "Password":
                self.assertTrue(len(elem["value"]) > 0)
                pass_ret = True
            else:
                self.fail("Credential name %s not expected." % elem["name"])
        self.assertTrue(username_ret, "Username credential not returned.")
        self.assertTrue(pass_ret, "Password credential not returned.")

        # Create it again, but now the user exists
        user_xml = """
            <USER>
                <NAME>tts_iam-test.indigo-datacloud.eu_dc5d5ab7-6db9-4079-985c-80ac05017066</NAME>
                <ID>1</ID>
            </USER>
        """
        one_server.one.userpool.info.return_value = (True, user_xml, 0)

        res = json.loads(process_request(req))
        self.assertEqual(res["state"], "tts_iam-test.indigo-datacloud.eu_dc5d5ab7-6db9-4079-985c-80ac05017066")
        pass_ret = False
        username_ret = False
        for elem in res["credential"]:
            if elem["name"] == "Username":
                self.assertEqual(elem["value"], "tts_iam-test.indigo-datacloud.eu_dc5d5ab7-6db9-4079-985c-80ac05017066")
                username_ret = True
            elif elem["name"] == "Password":
                self.assertTrue(len(elem["value"]) > 0)
                pass_ret = True
            else:
                self.fail("Credential name %s not expected." % elem["name"])
        self.assertTrue(username_ret, "Username credential not returned.")
        self.assertTrue(pass_ret, "Password credential not returned.")
        
        self.assertEqual(one_server.one.user.update.call_args_list[0][0][2],
                         ('ISS="https://iam-test.indigo-datacloud.eu/"\n'
                          'SUB="dc5d5ab7-6db9-4079-985c-80ac05017066"\n'
                          'Name="Miguel Caballer"'))

    @patch('xmlrpclib.ServerProxy')
    def test_10_revoke_cred(self, server_proxy):
        req = """{
            "action": "revoke",
            "cred_state": "tts_iam-test.indigo-datacloud.eu_dc5d5ab7-6db9-4079-985c-80ac05017066",
            "gid": 2000,
            "home_dir": "/home/ttsuser_2000",
            "params": "undefined",
            "uid": 2000,
            "user": "ttsuser_2000",
            "user_info": {
                "oidc": {
                    "iss": "https://iam-test.indigo-datacloud.eu/",
                    "name": "Miguel Caballer",
                    "sub": "dc5d5ab7-6db9-4079-985c-80ac05017066",
                    "external_authn": {
                        "iss": "https://accounts.google.com",
                        "sub": "111377986371988458806",
                        "type": "oidc"
                    },
                    "family_name": "Caballer",
                    "gender": "M",
                    "given_name": "Miguel",
                    "groups": [
                        "Users",
                        "Developers"
                    ],
                    "organisation_name": "indigo-dc",
                    "preferred_username": "micafer",
                    "updated_at": "Fri Jun 17 13:52:32 UTC 2016"
                },
                "site": {
                    "gidNumber": 2000,
                    "homeDirectory": "/home/ttsuser_2000",
                    "uid": "ttsuser_2000",
                    "uidNumber": 2000,
                    "userIds": {
                        "https://iam-test.indigo-datacloud.eu/": "dc5d5ab7-6db9-4079-985c-80ac05017066"
                    }
                },
                "uid": "ttsuser_2000"
            }
        }"""
        req = base64.b64encode(req)

        one_server = MagicMock()
        one_server.one.user.delete.return_value = (True, 1, 0)
        user_xml = """
            <USER>
                <NAME>tts_iam-test.indigo-datacloud.eu_dc5d5ab7-6db9-4079-985c-80ac05017066</NAME>
                <ID>1</ID>
            </USER>
        """
        one_server.one.userpool.info.return_value = (True, user_xml, 0)
        server_proxy.return_value = one_server

        res = json.loads(process_request(req))
        self.assertEqual(res["result"], "ok")

        # Revoke it again, but now the user does not exists
        user_xml = """
            <USER>
                <NAME>username</NAME>
                <ID>0</ID>
            </USER>
        """
        one_server.one.userpool.info.return_value = (True, user_xml, 0)

        res = json.loads(process_request(req))
        self.assertEqual(res["result"], "ok")

if __name__ == '__main__':
    unittest.main()
