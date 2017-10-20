# Configuration Guide
The configuration of WaTTS consists of one file. The file is located at `/etc/watts/watts.conf`.
One other location is supported for development purposes; in this case please place your
configuration file as `~/.config/watts/watts.conf`.


## Configuration (watts.conf)
The WaTTS is deployed with sane defaults, you only need to touch user-specific
changes.

Each setting consists of one simple line of the format, comments are starting with '#'.

```
# this is a comment and ignored
key = value
```


### Testing your configuration
WaTTS provides a simple command to check the configuration file
```
watts chkconfig
```
run it after changing the configuration to ensure your configuration file is correct.
If everything is fine it will print out a line telling you so:
```
config is OK
```

### Datatypes
There are different datatypes used in the configuration, a detailed description can be
seen in the following table.

| Datatype | Description |
| :---: | --- |
| 'word'| the word itself is the value, without the ' |
| host | a valid fully qualified hostname |
| port | an integer within the valid range for TCP ports |
| boolean | either 'true' or 'false' |
| file | an absolute path to a file |
| dir | an absolute path to a directory |
| duration | a timespan given by an integer and a unit, the unit can be ms, s, m, h |
| string | just the string value |
| integer | an integer value, i.e. a number |
| url | a valid url, use https as much as possible |
| comma separated list | values separated by comma |
| any | depends on the usage and can't be specified |
