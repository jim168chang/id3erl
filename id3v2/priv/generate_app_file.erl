%% Copyright
-module(generate_app_file).
-author("Nikolay Mavrenkov (koluch@koluch.ru)").

%% API
-export([generate/1]).

generate(Settings) ->
    Id = get_value(id,Settings),
    SourceFileName = "../" ++ atom_to_list(Id) ++ ".app.src",
    {ok, [FileContent]} = file:consult(SourceFileName),
    {application, Id, Values} = FileContent,
    App = {application, Id, create_full_settings(Values,Settings)},
    io:format("App: ~p~n", [App]),
    AppFileName = "../" ++ atom_to_list(Id) ++ ".app",
    unconsult(AppFileName, [App]).


create_full_settings([],Settings) -> generate_new(Settings);
create_full_settings([Value|Rest],Settings) -> [process_value(Value)|create_full_settings(Rest,Settings)].

process_value(Other) -> Other.

generate_new(Settings) ->
    {ok, ModuleFiles} = file:list_dir("../src"),
    ModuleNames = lists:map(fun(Name) ->
        list_to_atom(string:left(Name, string:len(Name) - 4))
    end, ModuleFiles),
    [{modules,ModuleNames},{vsn, get_value(vsn,Settings)}].


unconsult(File, L) ->
    {ok, S} = file:open(File, write),
    lists:foreach(fun(X) -> io:format(S, "~p.~n" ,[X]) end, L),
    file:close(S).

get_value(_, []) -> undef;
get_value(Key, [{Key,Value}|_]) -> Value;
get_value(Key, [_|T]) -> get_value(Key,T).