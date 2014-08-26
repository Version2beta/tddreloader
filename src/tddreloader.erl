%% @copyright 2014 OC Tanner Company
%% @author M Robert Martin <rob@version2beta.com>

-module(tddreloader).
-author("M Robert Martin <rob@version2beta.com>").

-include_lib("kernel/include/file.hrl").
-include_lib("eunit/include/eunit.hrl").

-behaviour(gen_server).
-export([start/0, start_link/0]).
-export([stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([all_changed/0]).
-export([is_changed/1]).
-export([reload_modules/0, reload_modules/1]).
-export([tests/0]).
-record(state, {last, tref}).

%% External API

start() ->
  gen_server:start({local, ?MODULE}, ?MODULE, [], []).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:call(?MODULE, stop).

%% gen_server callbacks

init([]) ->
  {ok, TRef} = timer:send_interval(timer:seconds(1), doit),
  {ok, #state{last = stamp(), tref = TRef}}.

handle_call(stop, _From, State) ->
  {stop, shutdown, stopped, State};
handle_call(_Req, _From, State) ->
  {reply, {error, badrequest}, State}.

handle_cast(_Req, State) ->
  {noreply, State}.

handle_info(doit, State) ->
  Now = stamp(),
  _ = doit(State#state.last, Now),
  {noreply, State#state{last = Now}};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, State) ->
  {ok, cancel} = timer:cancel(State#state.tref),
  ok.

code_change(_Vsn, State, _Extra) ->
  {ok, State}.

all_changed() ->
  [M || {M, Fn} <- code:all_loaded(), is_list(Fn), is_changed(M)].

is_changed(M) ->
  try
    module_vsn(M:module_info()) =/= module_vsn(code:get_object_code(M))
    catch _:_ ->
      false
  end.

reload_modules() ->
  reload_modules(all_changed()).
reload_modules(Modules) ->
  [begin code:purge(M), code:load_file(M) end || M <- Modules].

tests() ->
  eunit:test({dir, 'src'}).

%% Internal API

module_vsn({M, Beam, _Fn}) ->
  {ok, {M, Vsn}} = beam_lib:version(Beam),
  Vsn;
module_vsn(L) when is_list(L) ->
  {_, Attrs} = lists:keyfind(attributes, 1, L),
  {_, Vsn} = lists:keyfind(vsn, 1, Attrs),
  Vsn;
module_vsn(M) when is_atom(M) ->
  module_vsn(M:module_info()).

sources() ->
  sources("src/**/*.?rl").
sources(Path) ->
  filelib:wildcard(Path).

doit(From, To) ->
  [case file:read_file_info(Filename) of
      {ok, #file_info{mtime = Mtime}} when Mtime >= From, Mtime < To ->
        compile(Filename);
      {ok, _} ->
        unmodified;
      {error, Reason} ->
        io:format("Error reading ~s's file info: ~p~n",
          [Filename, Reason]),
        error
    end || Filename <- sources()].

compile(Filename) ->
  io:format("~nCompiling ~p ... ", [Filename]),
  case compile:file(Filename, {outdir, "ebin"}) of
    {ok, Module} ->
      io:format("succeeded (~p)~n", [Module]),
      reload(Module);
    _ ->
      io:format("failed~n")
  end.

reload(Module) ->
  io:format("Reloading ~p ... ", [Module]),
  code:purge(Module),
  case code:load_file(Module) of
    {module, Module} ->
      io:format("version ~w ok.~n", module_vsn(Module)),
      eunit:test(Module, [verbose]);
    {error, Reason} ->
      io:format(" fail: ~p.~n", [Reason]),
      error
  end.


stamp() ->
  erlang:localtime().

%%
%% Tests
%%

dummy_test() ->
  2 = 1 + 1.
dummy2_test() ->
  2 = 1 + 1.
dummy3_test() ->
  2 = 1 + 1.
dummy4_test() ->
  2 = 1 + 1.
