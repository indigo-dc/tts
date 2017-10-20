# Configuring SSH for WaTTS
WaTTS does not yet support hashed hosts in the `known_hosts` file. As the
connection to the remote host is done without user interaction, the host MUST be
listed in the `known_hosts` file.

To add a host to the list of known hosts in a way readable for WaTTS, the
`ssh_config` (usually located in `/etc/ssh/ssh_config`) must have the setting
`HashKnownHosts no`. After checking and eventually updating the configuration,
one can login as a WaTTS user.

As a WaTTS user, one connects to remote hosts using the credential specified in the
service configuration, with potentially using the verbose flag (-v). During the
connection there are two possibilities:

1. The client asks whether the host should be added. In the case it is a host that
   should be accessible by WaTTS, the user should answer with yes, and then
   can go on with the next host.
2. The client silently connects without asking. If this is the case, the host is
   already in the `~/.ssh/known_hosts` file. In the verbose connection, the
   output will tell which line in the file belongs to the remote host. In this
   case you can edit the `known_hosts` file and delete the line at the number
   printed before. Save the file and start the connection again, and the
   situation described above will happen.

After adding all the hosts, the `ssh_config` should be modified. Open the
`ssh_config` and change the hash hosts setting to `HashKnownHosts yes`.
