# TTS - Service Reference Card

* Daemons running
  * tts - Token Translation service
    * run_erl
    * beam.smp (can be multiple, usually #cores + 1)
  * epmd - erlang distribution daemon
* Init scripts and options
  * tts start - starts the TTS
  * tts stop - stops the TTS
  * tts restart - restarts the TTS
  * tts ping - check if TTS is up and running, prints "pong" if up
  * tts getpid - prints the pid of TTS
  * tts version - prints out the verison of the TTS
  * tts console - starts the TTS in forground mode, e.g. for debugging
  * tts attach - connects to the shell of a running TTS, use with caution
* Configuration files location
  * /etc/tts/main.conf - the main configuration file
  * /etc/tts/oidc/ - directory of OpenID Connect configurations
  * /etc/tts/service/ - directory of service configurations
* Logfile locations (and management) and other useful audit information
  * /var/log/tts/* - all differnet logs
* Open ports
  * 8080 - in default config
  * 4369 - epmd, can be blocked by firewall
* Possible unit test of the service
  * configure at least one OpenID Connect Provider
  * configure the info service
  * login to TTS and request the Info credentials
  * a list of informations should be shown
  * revoke the credential
  * the screen should look like in the beginning
* Where is service state held (and can it be rebuilt)
  * /var/lib/tts - this directory contains all data/state
  * /etc/tts - this directory contains all settings
* Cron jobs
  * none
* Security information
  * Access control Mechanism description (authentication & authorization)
    * done via OpenID Connect library, oidcc
  * How to block/ban a user
    * done in the plugins
  * Network Usage
    * http connections for the REST/Web interface
    * outgoing connections depend upon services/plugins in use
  * Firewall configuration
    * only open port configured in main.conf, in default config port 8080
  * Security recommendations
    * run as a dedicated, non root, user
    * set up SSL before running in production
    * do not use self-signed certificates in production
