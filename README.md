# foldrerl
Erlang application to replicate a folder hierarchy between two nodes

## Quick start

```
[root@dev1 foldrerl]# rebar get-deps clean compile shell
Erlang/OTP 17 [erts-6.4] [source-2e19e2f] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V6.4  (abort with ^G)
1> application:set_env(foldrerl,address,{"127.0.0.1",12345}).
2> application:set_env(foldrerl,cert,"/tmp/cert.pem").
3> application:set_env(foldrerl,key,"/tmp/key.pem").
4> application:set_env(foldrerl,ca,"/tmp/cacert.pem").
5> application:ensure_all_started(foldrerl).
6> foldrerl:retrieve_folder(node(),"/tmp/foo","/tmp/bar").
ok
```

### Introduction

This is a quick library to retrieve a folder from a remote erlang node which has this library installed.

The library assumes you've got an environment variable which is set to the listening socket IP/port pair under the key foldrerl->address (as in the example above).

This is to allow for erlang distribution being on a different interface than the file transfers. The address is sent to the peer (which then connects back over TCP). Therefore, using loopback interfaces won't work (duh), nor will the 0.0.0.0 address (the listening socket will work, but the client won't know how to call back).


### API

`folderl:retrieve_folder(RemoteNode,RemotePath,LocalPath)`:
* RemoteNode is the target for which you want to get the files from
* RemotePath is the path you want to retrieve
* LocalPath is the local path to store the files in.

Notes:
* Files are checksummed to ensure transfers succeeded.
* It will retry up to 10 times
* This library performs some RPC calls, so nodes must be properly clustered.
* The root of the remote path will be stripped in the local path. For example, if your remote path is "/foo", local path "/baz" and you have a remote file "/foo/bar/buz" then the file will be dropped in "/baz/bar/buz".
