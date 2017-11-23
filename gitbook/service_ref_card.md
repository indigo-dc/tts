# WaTTS - Service Reference Card

* Daemons running:
  * watts - The INDIGO Token Translation service
    * run_erl
    * beam.smp (can be multiple, usually #cores + 1)
  * epmd - erlang distribution daemon
* Init scripts and options:
  * watts start - starts WaTTS
  * watts stop - stops WaTTS
  * watts restart - restarts WaTTS
  * watts ping - check if WaTTS is up and running, prints "pong" if up
  * watts getpid - prints the pid of WaTTS
  * watts version - prints out the verison of WaTTS
  * watts console - starts WaTTS in forground mode, e.g. for debugging
  * watts attach - connects to the shell of a running WaTTS, use with caution
* Configuration files location:
  * /etc/watts/watts.conf - the main configuration file
* Logfile locations (and management) and other useful audit information:
  * /var/log/watts/* - all differnet logs
* Open ports:
  * 8080 - in default config
  * 4369 - epmd, can be blocked by firewall
  * In a production setup this usually changes to
    * 8080 - http redirection to the SSL secured connection
    * 8443 - https web interface and api
    * the above ports get redirected by iptables to the specified ports (see Server Settings)
* Possible unit test of the service:
  * configure at least one OpenID Connect Provider
  * configure the info service
  * login to WaTTS and request the Info credentials
  * a list of informations should be shown
  * revoke the credential
  * the screen should look like in the beginning
* Where is service state held (and can it be rebuilt):
  * /var/lib/watts - this directory contains all data/state
  * /etc/watts - this directory contains all settings
* Cron jobs:
  * none
* Security information:
  * Access control Mechanism description (authentication & authorization):
    * done via OpenID Connect library, oidcc
  * How to block/ban a user:
    * either configuer the service authz (see configuration documentation)
    * or done in the plugins (see developer documentation)
  * Network Usage:
    * http connections for the REST/Web interface
    * outgoing https connections to OpenId Connect Provider
    * other outgoing connections depend upon services/plugins in use
  * Firewall configuration:
    * only open listen_port and redirect_port configured in watts.conf, in default config port 8080
  * Security recommendations:
    * run as a dedicated, non-root user
    * set up SSL before running in production
    * do not use self-signed certificates in production
