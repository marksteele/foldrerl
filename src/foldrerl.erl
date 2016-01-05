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

-record(state, {}).

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
  lager:debug("Initiating file transfer, grabbing remote folder: ~p on node ~p and copying to ~p",[PeerPath,Node,LocalPath]),
  CleanPeerPath = remove_trailing_slash(PeerPath),
  CleanLocalPath = remove_trailing_slash(LocalPath),
  {ok, Manifest} = gen_server:call({?SERVER,Node},{manifest,CleanPeerPath}),
  lager:debug("Received folder manifest, starting transfer"),
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
  {ok, #state{}}.

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
  lager:debug("Generating folder manifest for path ~p",[Path]),
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

handle_cast({send_file,Path, Host, Port},State) ->
  lager:debug("Sending file ~p to ~p:~p",[Path,Host,Port]),
  spawn(
    fun() ->
        {ok, Sock} = gen_tcp:connect(Host, Port, [binary, {packet,0}]),
        {ok, _Bytes} = file:sendfile(Path,Sock),
        lager:debug("File ~p sent to ~p:~p successfully",[Path,Host,Port])
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
    {ok, LSock} = gen_tcp:listen(0,
                               [
                                binary,
                                {packet, 0},
                                {active, false},
                                {reuseaddr, true}
                               ]
                              ),
  retrieve_files(Manifest,Node,PeerPath,LocalPath,LSock,0).

retrieve_files([],_,_,_,LSock,_) ->
  gen_tcp:close(LSock),
  ok;
retrieve_files(_,_,_,_,_,Errors) when Errors =:= ?MAX_RETRIES ->
  error;
retrieve_files([{Path,MD5}|Manifest],Node,PeerPath,LocalPath,LSock,Errors) ->
  try
    NewPath = re:replace(Path,"^" ++ PeerPath,LocalPath,[{return,list}]),
    ok = filelib:ensure_dir(NewPath),
    {ok,File} = file:open(NewPath,[write]),
    {ok, {Address,Port}} = inet:sockname(LSock),
    gen_server:cast({?SERVER,Node},{send_file,Path,Address,Port}),
    {ok, Sock} = gen_tcp:accept(LSock),
    ok = receive_file(Sock,File),
    ok = file:close(File),
    gen_tcp:close(Sock),
    {ok,Digest} = lib_md5:file(NewPath),
    MD5 = Digest,
    retrieve_files(Manifest,Node,PeerPath,LocalPath,LSock,Errors)
  catch
    _:_ ->
      retrieve_files([{Path,MD5}|Manifest],Node,PeerPath,LocalPath,LSock,Errors)
  end.

receive_file(Sock,File) ->
  case gen_tcp:recv(Sock, 0, ?DEFAULT_TIMEOUT) of
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
