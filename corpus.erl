%% encoding: utf-8
%% This file is used for generating the documentation. It contains
%% erlang code with corresponding descriptions.

-doc
"Erlang Abstract Format\n"
"======================\n"
"\n"
"The Erlang Abstract Format is used in parse trees in various tools, such\n"
"as the compiler and preprocessor modules shipped with Erlang/OTP. A brief\n"
"documentation is available at http://erlang.org/doc/apps/erts/absform.html.\n"
"However, the complete syntax of abstract format is not documented.\n"
"\n"
"This is an attempt to document every langugage construct by examples.\n"
"The examples are generated using Erlang source code,\n"
"which is parsed to generate the abstract form, and pretty-printed again to\n"
"generate the Erlang code examples.\n"
"\n"
"Every form is a tuple. The second element is the line number in the source\n"
"file.\n"
"\n"
"Preprocess directives such as -define, -include and -ifdef are currently\n"
"not included because `epp:parse_file/2` is used for parsing the file, which\n"
"consumes the preprocessor directives.".

-doc
"Top-level forms\n"
"---------------\n".

-file("corpus.erl", 30).
-module(corpus).
-compile(export_all).
-vsn("1.2.3").
-on_load(my_function/0).
-behaviour(gen_server).
-export([my_function/0]).
-export_type([my_type/0]).
-import(lists, [foldl/3]).
-error(my_error).
-warning(my_warning).
-include("include_not_found.hrl").
-include_lib("include_lib/not_found.hrl").
-type my_type() :: a | b.
-opaque my_opaque() :: a | b.
-type maybe(A) :: {just, A} | nothing.
-callback my_function() -> ok.
-spec my_function() -> ok.
-spec my_function(A :: atom()) -> B when B :: boolean().
my_function() -> ok.
my_function(X) when X > 2 -> X + 1.





-doc
"Record declarations: "
"A record is represented as a form without types. If any fields are typed,\n"
"the form is followed by a 'record type' form. Thus, for records with typed\n"
"fields, there are two consecutive forms.".
-record(myrec, {field1 = foo :: atom(),
                field2       :: undefined,
                field3 = foo,
                field4}).

-doc
"Types\n"
"-----\n"
"\n"
"Types, mostly in the order as listed under \"Types and their Syntax\"\n"
"on http://erlang.org/doc/reference_manual/typespec.html.\n"
"\n"
"Abstract form syntax for types: `{type, Line, TypeName, TypeParams}`.".

-type t() :: any().
-type t() :: none().
-type t() :: pid().
-type t() :: port().
-type t() :: reference().
-type t() :: [].
-type t() :: atom().

%% Binary
-type t() :: <<>>.
-comment "M is a positive integer".
-type t() :: <<_:M>>.
-comment "N is a positive integer".
-type t() :: <<_:_*N>>.
-type t() :: <<_:M, _:_*N>>.

-type t() :: float().

%% Function
-comment "any function".
-type t() :: fun().
-comment "any arity, returning Type".
-type t() :: fun((...) -> integer()).
-type t() :: fun(() -> integer()).
-type t() :: fun((atom(), atom()) -> integer()).

%% Integer
-type t() :: integer().
-comment "..., -1, 0, 1, ... 42 ...".
-type t() :: 42.
-comment "specifies an integer range".
-type t() :: 1..10.

%% List
-comment "Proper list ([]-terminated)".
-type t() :: list(integer()).
-comment "Type1=contents, Type2=termination".
-type t() :: maybe_improper_list(integer(), atom()).
-comment "Type1 and Type2 as above".
-type t() :: nonempty_improper_list(integer(), atom()).
-comment "Proper non-empty list".
-type t() :: nonempty_list(integer()).

%% Map
-comment "denotes a map of any size".
-type t() :: map().
-comment "denotes the empty map".
-type t() :: #{}.
-type t() :: #{size      := integer(),      %% denotes a mandatory association
               integer() => any()}.         %% denotes an optional association

%% Tuple
-type t() :: tuple().                       %% denotes a tuple of any size
-type t() :: {}.
-type t() :: {atom()}.
-type t() :: {atom(), integer()}.

%% Record
-type t() :: #my_record{}.

%% Union
-type t() :: atom() | integer().

%% User-def and remote
-type t() :: my_type().
-type t() :: maybe(integer()).
-type t() :: module:type().

%% ----------------

-doc
"For convenience, the following types are also built-in. They can be\n"
"thought as predefined aliases for the type unions shown in the code\n"
"comments.".

%% The following lines are generated by piping the table "Built in, Defined
%% as" on the page http://erlang.org/doc/reference_manual/typespec.html
%% copy-pasted using Firefox to the following perl one-liner:
%%
%% perl -pe 's/^(\S+) *\t([^\n\t]+)$/-comment("$1 :: $2").\n-type t() :: $1./'
-comment "term() :: any()".
-type t() :: term().
-comment "binary() :: <<_:_*8>>".
-type t() :: binary().
-comment "bitstring() :: <<_:_*1>>".
-type t() :: bitstring().
-comment "boolean() :: 'false' | 'true'".
-type t() :: boolean().
-comment "byte() :: 0..255".
-type t() :: byte().
-comment "char() :: 0..16#10ffff".
-type t() :: char().
-comment "nil() :: []".
-type t() :: nil().
-comment "number() :: integer() | float()".
-type t() :: number().
-comment "list() :: [any()]".
-type t() :: list().
-comment "maybe_improper_list() :: maybe_improper_list(any(), any())".
-type t() :: maybe_improper_list().
-comment "nonempty_list() :: nonempty_list(any())".
-type t() :: nonempty_list().
-comment "string() :: [char()]".
-type t() :: string().
-comment "nonempty_string() :: [char(),...]".
-type t() :: nonempty_string().
-comment "iodata() :: iolist() | binary()".
-type t() :: iodata().
-comment "iolist() :: maybe_improper_list(byte() | binary() | iolist(), binary() | [])".
-type t() :: iolist().
-comment "function() :: fun()".
-type t() :: function().
-comment "module() :: atom()".
-type t() :: module().
-comment "mfa() :: {module(),atom(),arity()}".
-type t() :: mfa().
-comment "arity() :: 0..255".
-type t() :: arity().
-comment "identifier() :: pid() | port() | reference()".
-type t() :: identifier().
-comment "node() :: atom()".
-type t() :: node().
-comment "timeout() :: 'infinity' | non_neg_integer()".
-type t() :: timeout().
-comment "no_return() :: none()".
-type t() :: no_return().
%%---

-doc "Built-in types; can be thought defined by the syntax...".
%% perl -pe 's/^(\S+) *\t([^\n\t]+)$/-comment("$1 :: $2").\n-type t() :: $1./'
-comment "non_neg_integer() :: 0..".
-type t() :: non_neg_integer().
-comment "pos_integer() :: 1..".
-type t() :: pos_integer().
-comment "neg_integer() :: ..-1".
-type t() :: neg_integer().

-doc
"Exressions\n"
"----------\n".

%% Terms
f() -> 42.
f() -> 3.141592653589.
f() -> ok.
f() -> [].
f() -> [x,y].
f() -> [x | XS].
f() -> "".
f() -> "abc".
f() -> <<"abc">>.
f() -> <<A:8/integer, B:32/float-little, C/binary>>.
f() -> <<"abc"/utf8, XYZ/utf16>>.
f() -> #{old_key := updated_value, new_key => 42}.
f() -> {x, y}.
f() -> #my_record{foo = X}.
f() -> Rec#my_record.foo.
f() -> fun f/1.
f() -> fun m:f/1.

%% Variables
f() -> X.
f() -> _.

%% Operators
f() -> not true.
f() -> 1 + 1.
f() -> 1 == 1.
f() -> X and Y.
f() -> X andalso Y.

%% Function call and pattern matching
f() -> f(42).
f() -> m:f(42).
f() -> X = 42.
f() -> fun (42) -> true; (_) -> false end.
f() -> fun (X) when is_atom(X) -> X end.
f() -> if P -> hello; true -> ok end.
f() -> case foo of bar -> baz; _ -> ok end.
f() -> begin ok, ok end.
f() -> Pid ! message.
f() -> receive X -> ok end.
f() -> receive X -> ok after 1000 -> timeout end.
f() -> try f() catch error:E -> fail end.
f() -> try f(),g() of X when is_integer(X) -> X catch C:E when is_tuple(E) -> fail after afterwards() end.
f() -> catch X.

%% List comprehensions
f() -> [2 || is_integer(2)].
f() -> [X || X <- XS, is_atom(X)].
f() -> << <<X:4>> || <<X:4>> <= Bin, X /= 0 >>.

