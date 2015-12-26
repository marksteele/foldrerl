# foldrerl
Erlang application to replicate a folder hierarchy between two nodes

## Quick start

```
[root@dev1 foldrerl]# rebar compile; rebar shell
==> foldrerl (compile)
==> foldrerl (shell)
Erlang/OTP 17 [erts-6.4] [source-2e19e2f] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V6.4  (abort with ^G)
1> foldrerl:retrieve_folder(node(),"/tmp/foo","/tmp/bar").
ok
```

### Introduction

This is a quick library to retrieve a folder from a remote erlang node which has this library installed.

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