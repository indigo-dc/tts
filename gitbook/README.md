# Token Translation Service 
## Purpose

Token Translation Service (TTS) returns credentials after successful
authentication.  It offers an easy way to self service credentials by the
users. TTS was developed for cases in which users can be (only)
authenticated via OpenId-Connect but require different credentials for
accessing services.

## Services
Arbitrary services can be supported via a plugin interface. Currently
supported services include

- ssh public/private keypair creation
- S3 authentication tokens
- X.509 certificates
- OpenNebula credentials

## Interfaces
TTS provides a web and a REST interface for users to create/retrieve and
remove credentials.

