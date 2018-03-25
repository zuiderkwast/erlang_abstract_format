#!/usr/bin/env escript
%% -*- erlang -*-
%%! +A0

main([]) ->
    {ok, Forms} = epp:parse_file("corpus.erl", []),
    {ok, F} = file:open("README.md", [write]),
    try doc_forms(F, Forms, false)
    after file:close(F)
    end.

doc_forms(F, [{attribute, _, comment, Comment}, Form | Forms], InTable) ->
    %% A comment followed by another form.
    InTable orelse io:format(F, "<table>~n", []),
    doc_form(F, Form, Comment),
    doc_forms(F, Forms, true);
doc_forms(F, [Form | Forms], InTable) ->
    InTableAfter =
        case Form of
            {attribute, _, doc, Doc} ->
                InTable andalso io:format(F, "</table>~n~n", []),
                io:format(F, "~s~n~n", [Doc]),
                false;
            {attribute, _, file, _M} ->
                InTable;
            {eof, _} ->
                InTable;
            _ ->
                InTable orelse io:format(F, "<table>~n", []),
                doc_form(F, Form, ""),
                true
        end,
    doc_forms(F, Forms, InTableAfter);
doc_forms(F, [], true) ->
    io:format(F, "</table>~n", []);
doc_forms(_F, [], false) ->
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
                Indent = lists:duplicate(length(atom_to_list(RName)) + 10, $\s),
                FieldsSrc = string:join(FieldSrcs, ",\n" ++ Indent),
                Src = io_lib:format("-record(~p,{~s}).", [RName, FieldsSrc]),
                {Src, Form};
            {attribute, _, type, {_Name, T, []}} ->
                TypeDef  = erl_pp:form(Form),
                {match, [Src]} = re:run(TypeDef, TypeRE,
                                        [{capture, all_but_first, binary}]),
                {Src, T};
            _ ->
                %% Any form
                Src = erl_pp:form(Form),
                io:format("Unexpected form: ~s~p~n~n", [Src,Form]),
                {Src, Form}
        end,
    ok = io:format(F, "<tr><td>~n```Erlang~n~s~n```~n</td>"
                      "<td>~n```Erlang~n~p~n```~n</td>"
                      "<td>~s</td></tr>~n",
                   [PP, Abs, Comment]).

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
