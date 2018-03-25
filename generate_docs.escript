#!/usr/bin/env escript
%% -*- erlang -*-
%%! +A0

main([]) ->
    {ok, Forms} = epp:parse_file("corpus.erl", []),
    {ok, F} = file:open("README.md", [write]),
    try
        io:format(F, "~n", []),
        lists:foldl(fun (Form, Comment) ->
                        doc_form(F, Form, Comment)
                    end,
                    none,            %% Comment on previous line
                    Forms)
    after
        file:close(F)
    end.

doc_form(F, Form, Comment) ->
    {ok, TypeRE} = re:compile(<<"::\\s*(.*)\\.\\n*">>),
    case Form of
        {attribute, _, file, _M} ->
            ok;
        {eof, _} ->
            ok;
        {attribute, _, p, S} when is_list(S) ->
            io:format(F, "~s~n~n", [S]);
        {attribute, _, h1, S} when is_list(S) ->
            io:format(F, "~s~n~s~n~n", [S, lists:duplicate(length(S), $=)]);
        {attribute, _, h2, S} when is_list(S) ->
            ok = io:format(F, "~s~n~s~n~n",
                           [S, lists:duplicate(length(S), $-)]);
        {attribute, _, defs, {X,Y,Z}} ->
            io:format(F, "<table><tr><th>~s</th><th>~s</th><th>~s</th></tr>~n",
                      [X, Y, Z]);
        {attribute, _, enddefs, []} ->
            io:format(F, "</table>\n");
        {attribute, _, comment, S} when is_list(S) ->
            %% Comments about the following example. Pass on to the next form.
            S;
        {attribute, _, record, _} ->
            %% Uptyped record declaration. When types are present, this is
            %% followed by a record type declaration form.
            print(F, erl_pp:form(Form), Form);
        {attribute, _, type, {{record, RName}, Fields, []}} ->
            %% Record with typed fields
            FieldSrcs = lists:map(fun pp_record_field/1, Fields),
            Indent = lists:duplicate(length(atom_to_list(RName)) + 10, $\s),
            FieldsSrc = string:join(FieldSrcs, ",\n" ++ Indent),
            Src = io_lib:format("-record(~p,{~s}).", [RName, FieldsSrc]),
            print(F, Src, Form);
        {attribute, _, type, {_Name, T, []}} ->
            TypeDef  = erl_pp:form(Form),
            {match, [Src]} = re:run(TypeDef, TypeRE,
                                    [{capture, all_but_first, binary}]),
            if is_list(Comment) ->
                   print(F, Src, T, Comment);
               true ->
                   print(F, Src, T)
            end;
        _ ->
            %% Any form
            Src = erl_pp:form(Form),
            io:format("Unexpected: ~s~n~p~n", [Src, Form])
            %print(F, Src, Form)
    end.

pp_type(T) ->
    pp_type_from_attribute({attribute, 0, type, {t, T, []}}).

pp_type_from_attribute(AttributeForm) ->
    TypeDef = list_to_binary(erl_pp:form(AttributeForm)),
    [_, Src0] = binary:split(TypeDef, <<" :: ">>),
    SrcSize = byte_size(Src0) - 2,
    <<Src:SrcSize/binary, ".\n">> = Src0,
    Src.

pp_record_field({typed_record_field, RecordField, Type}) ->
    WithoutType = pp_record_field(RecordField),
    io_lib:format("~s :: ~s", [WithoutType, pp_type(Type)]);
pp_record_field({record_field, _, {atom, _, Name}, Def}) ->
    io_lib:format("~p = ~s", [Name, erl_pp:expr(Def)]);
pp_record_field({record_field, _, {atom, _, Name}}) ->
    io_lib:format("~p", [Name]).

print(F, Src, Abs) ->
    ok = io:format(F, "## `~s`~n~n```Erlang~n~p~n```~n~n", [Src, Abs]).
print(F, Src, Abs, Comment) ->
    ok = io:format(F, "## `~s`~n~n```Erlang~n~p %% ~s~n```~n~n",
                   [Src, Abs, Comment]).
