#!/usr/bin/env escript
%% -*- erlang -*-
%%! +A0

main([]) ->
    {ok, [{attribute,_,file,_} | Forms]} = epp:parse_file("corpus.erl", []),
    {ok, F} = file:open("README.md", [write]),
    try doc_forms(F, Forms)
    after file:close(F)
    end.

doc_forms(F, [{attribute, _, comment, Comment}, Form | Forms]) ->
    %% A comment followed by another form.
    doc_form(F, Form, Comment),
    doc_forms(F, Forms);
doc_forms(F, [Form | Forms]) ->
    case Form of
        {attribute, _, doc, Doc} ->
            io:format(F, "~s~n~n", [Doc]);
        {attribute, _, file, M} when is_atom(M) ->
            ok;
        {error, E} ->
            io:format("Error: ~p~n", [E]);
        {eof, _} ->
            ok;
        _ ->
            doc_form(F, Form, "")
    end,
    doc_forms(F, Forms);
doc_forms(_F, []) ->
    ok.

doc_form(F, Form, Comment) ->
    {ok, TypeRE} = re:compile(<<"::\\s*(.*)\\.\\n*">>),
    {PP, Abs} =
        case Form of
            {attribute, _, record, _} ->
                %% Uptyped record declaration. When types are present, this is
                %% followed by a record type declaration form.
                {erl_pp:form(Form), Form};
            {attribute, _, type, {{record, RName}, Fields, []}} ->
                %% Record with typed fields. Doesn't seem to be implemented in
                %% erl_pp so we do our own pretty-printing.
                FieldSrcs = lists:map(fun pp_record_field/1, Fields),
                Indent = lists:duplicate(length(atom_to_list(RName)) + 11, $\s),
                FieldsSrc = string:join(FieldSrcs, ",\n" ++ Indent),
                Src = io_lib:format("-record(~p, {~s}).", [RName, FieldsSrc]),
                {Src, Form};
            {attribute, _, type, {t, T, []}} ->
                %% Type, wrapped in a type definition for type 't'.
                TypeDef  = erl_pp:form(Form),
                {match, [Src]} = re:run(TypeDef, TypeRE,
                                        [{capture, all_but_first, binary}]),
                {Src, T};
            {function, _, f, 0, [{clause, _, [], [], [Expr]}]} ->
                %% Expression, wrapped in f() -> Expr.
                {erl_pp:expr(Expr), Expr};
            _ ->
                %% Any form
                Src = erl_pp:form(Form),
                {string:strip(lists:flatten(Src), right, $\n), Form}
        end,
    Comment1 = case Comment of "" -> ""; _ -> [" (", Comment, ")"] end,
    ok = io:format(F, "**`~s`**~s~n~n"
                      "```Erlang~n~p~n```~n~n",
                   [PP, Comment1, Abs]).

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
