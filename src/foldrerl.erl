%%%-------------------------------------------------------------------
%%% @author Mark Steele <mark@control-alt-del.org>
%%% @copyright (C) 2016, Mark Steele
%%% @doc
%%% Foldrerl server
%%% @end
%%% Created :  5 Jan 2016 by Mark Steele <mark@control-alt-del.org>
%%%-------------------------------------------------------------------
-module(foldrerl).

-behaviour(gen_server).

%% API
-export([
         start_link/0,
         retrieve_folder/3
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(MAX_RETRIES,10).
-define(DEFAULT_TIMEOUT,100000).

-include_lib("public_key/include/public_key.hrl").
-define(PEM_ENCODED_LINE_LENGTH, 64).

-record(state, {cert,key,ca,ca_bin}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

retrieve_folder(Node, PeerPath, LocalPath) ->
  CleanPeerPath = remove_trailing_slash(PeerPath),
  CleanLocalPath = remove_trailing_slash(LocalPath),
  {ok, Manifest} = gen_server:call({?SERVER,Node},{manifest,CleanPeerPath}),
  retrieve_files(Manifest,Node,CleanPeerPath,CleanLocalPath).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
  {ok, Cert} = application:get_env(foldrerl,cert),
  {ok, Key} = application:get_env(foldrerl,key),
  {ok, CA} = application:get_env(foldrerl,ca),
  {ok, CACertBin} = file:read_file(CA),
  {ok, #state{cert=Cert,key=Key,ca=CA,ca_bin=CACertBin}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_call({manifest,Path},_From,S) ->
  {reply,
   {ok,
    filelib:fold_files(
      Path,
      ".*",
      true,
      fun(X,Acc) ->
          {ok,MD5} = lib_md5:file(X),
          Acc ++ [{X,MD5}]
      end,
      [])},
   S};


handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_cast({send_file,Path, Host, Port},
            State=#state{cert=Cert,key=Key,ca=CA}) ->
  spawn(
    fun() ->
        {ok, Sock} = ssl:connect(
                       Host,
                       Port,
                       [
                        binary,
                        {packet,0},
                        {certfile,Cert},
                        {keyfile,Key},
                        {cacertfile,CA},
                        {versions,['tlsv1.2']},
                        {ciphers,
                         [
                          "ECDHE-ECDSA-AES256-SHA384",
                          "ECDHE-RSA-AES256-SHA384",
                          "ECDH-ECDSA-AES256-SHA384",
                          "ECDH-RSA-AES256-SHA384",
                          "DHE-RSA-AES256-SHA256",
                          "DHE-DSS-AES256-SHA256",
                          "ECDHE-ECDSA-AES128-SHA256",
                          "ECDHE-RSA-AES128-SHA256",
                          "ECDH-ECDSA-AES128-SHA256",
                          "ECDH-RSA-AES128-SHA256",
                          "DHE-RSA-AES128-SHA256",
                          "DHE-DSS-AES128-SHA256"
                         ]}
                       ]),
        ok = send_file(Sock,Path)
    end),
  {noreply, State};

handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

remove_trailing_slash(Path) ->
  string:strip(Path,right,$/).

retrieve_files(Manifest,Node,PeerPath,LocalPath) ->
  {ok, {IP,Port}} = application:get_env(foldrerl,address),
  {ok, Cert} = application:get_env(foldrerl,cert),
  {ok, Key} = application:get_env(foldrerl,key),
  {ok, CA} = application:get_env(foldrerl,ca),
  {ok, ParsedIP} = inet:parse_address(IP),
  {ok, LSock} = ssl:listen(Port,
                               [
                                {certfile,Cert},
                                {keyfile,Key},
                                {cacertfile,CA},
                                {depth,0},
                                {verify,verify_peer},
                                {fail_if_no_peer_cert,true},
                                {honor_cipher_order, true},
                                {versions,['tlsv1.2']},
                                {ciphers,
                                 [
                                  "ECDHE-ECDSA-AES256-SHA384",
                                  "ECDHE-RSA-AES256-SHA384",
                                  "ECDH-ECDSA-AES256-SHA384",
                                  "ECDH-RSA-AES256-SHA384",
                                  "DHE-RSA-AES256-SHA256",
                                  "DHE-DSS-AES256-SHA256",
                                  "ECDHE-ECDSA-AES128-SHA256",
                                  "ECDHE-RSA-AES128-SHA256",
                                  "ECDH-ECDSA-AES128-SHA256",
                                  "ECDH-RSA-AES128-SHA256",
                                  "DHE-RSA-AES128-SHA256",
                                  "DHE-DSS-AES128-SHA256"
                                 ]},
                                binary,
                                {packet, 0},
                                {active, false},
                                {reuseaddr, true},
                                {ip,ParsedIP}
                               ]
                              ),

  retrieve_files(Manifest,Node,PeerPath,LocalPath,LSock,0,ParsedIP,Port).

retrieve_files([],_,_,_,LSock,_,_,_) ->
  ssl:close(LSock),
  ok;
retrieve_files(_,_,_,_,_,Errors,_,_) when Errors =:= ?MAX_RETRIES ->
  error;
retrieve_files([{Path,MD5}|Manifest],Node,PeerPath,LocalPath,LSock,Errors,IP,Port) ->
  try
    NewPath = re:replace(Path,"^" ++ PeerPath,LocalPath,[{return,list}]),
    ok = filelib:ensure_dir(NewPath),
    {ok,File} = file:open(NewPath,[write]),
    gen_server:cast({?SERVER,Node},{send_file,Path,IP,Port}),
    {ok, Sock} = ssl:transport_accept(LSock),
    ok = ssl:ssl_accept(Sock),
    ok = receive_file(Sock,File),
    ok = file:close(File),
    ssl:close(Sock),
    {ok,Digest} = lib_md5:file(NewPath),
    MD5 = Digest,
    retrieve_files(Manifest,Node,PeerPath,LocalPath,LSock,Errors,IP,Port)
  catch
    _:_ ->
      retrieve_files([{Path,MD5}|Manifest],Node,PeerPath,LocalPath,LSock,Errors,IP,Port)
  end.

receive_file(Sock,File) ->
  case ssl:recv(Sock, 0, ?DEFAULT_TIMEOUT) of
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

send_file(Sock,Filename) ->
  case file:open(Filename,[read,raw,binary]) of
    {ok, RawFile} ->
      send_file(Sock,RawFile,0);
    _ ->
      error
  end.

send_file(Sock,File,Offset) ->
  {ok,Offset} = file:position(File,{bof,Offset}),
  case file:read(File,16#1FFF) of
    {ok, IoData} ->
      case ssl:send(Sock,IoData) of
        ok ->
          send_file(Sock,File,Offset+byte_size(IoData));
        _ ->
          error
      end;
    eof ->
      file:close(File),
      ok;
    _ ->
      error
  end.
