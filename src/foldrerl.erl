%%%-------------------------------------------------------------------
%%% @author Mark Steele <mark@control-alt-del.org>
%%% @copyright (C) 2015, Mark Steele
%%% @doc
%%% Quick library for synching a remote folder tree
%%% @end
%%% Created : 21 Dec 2015 by Mark Steele <mark@control-alt-del.org>
%%%-------------------------------------------------------------------
-module(foldrerl).

-define(TIMEOUT,10000).

%% API
-export([
         retrieve_folder/3,
         folder_manifest/1,
         send_file/3
        ]).

%%%===================================================================
%%% API
%%%===================================================================

retrieve_folder(Node, PeerPath, LocalPath) ->
  %% Spawn local gen_fsm to handle this transfer
  {ok, Manifest} = rpc:call(Node,foldrerl,folder_manifest,[PeerPath]),
  retrieve_files(Manifest,Node,PeerPath,LocalPath).

folder_manifest(Path) ->
  {ok, filelib:fold_files(
    Path,
    ".*",
    true,
    fun(X,Acc) ->
        {ok,MD5} = lib_md5:file(X),
        Acc ++ [{X,MD5}]
    end,
    [])}.

send_file(Path, Host, Port) ->
  spawn(fun() ->
            {ok, Sock} = gen_tcp:connect(Host, Port, [binary, {packet,0}]),
            {ok, _Bytes} = file:sendfile(Path,Sock)
        end),
  ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================

retrieve_files(Manifest,Node,PeerPath,LocalPath) ->
  {ok, LSock} = gen_tcp:listen(0,
                               [
                                binary,
                                {packet, 0},
                                {active, false},
                                {reuseaddr, true}
                               ]
                              ),
  retrieve_files(Manifest,Node,PeerPath,LocalPath,LSock).

retrieve_files([],_,_,_,_) ->
  ok;
retrieve_files([{Path,MD5}|Manifest],Node,PeerPath,LocalPath,LSock) ->
  %% Remove leading slash
  Stripped0 = string:strip(Path,left,$/),
  %% Strip out source path root
  Stripped1 = string:substr(Stripped0,string:span(PeerPath,Stripped0)),
  %% Remove any remaining leading slashes
  Stripped2 = string:strip(Stripped1,left,$/),
  %% Join to new path
  NewPath = filename:join(LocalPath,Stripped2),

  ok = filelib:ensure_dir(NewPath),
  {ok,File} = file:open(NewPath,[write]),
  {ok, {Address,Port}} = inet:sockname(LSock),
  ok = rpc:call(Node,foldrerl,send_file,[Path,Address,Port]),
  {ok, Sock} = gen_tcp:accept(LSock),
  ok = receive_file(Sock,File),
  ok = file:close(File),
  gen_tcp:close(Sock),
  {ok,Digest} = lib_md5:file(NewPath),
  case Digest of
    MD5 ->
      retrieve_files(Manifest,Node,PeerPath,LocalPath,LSock);
    _ ->
      retrieve_files([{Path,MD5}|Manifest],Node,PeerPath,LocalPath,LSock)
  end.

receive_file(Sock,File) ->
  case gen_tcp:recv(Sock, 0, ?TIMEOUT) of
    {ok, B} ->
      case file:write(File,B) of
        ok ->
          receive_file(Sock, File);
        _ ->
          error
      end;
    {error, closed} ->
      ok
  end.
