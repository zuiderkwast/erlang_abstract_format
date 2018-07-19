Erlang Abstract Format
======================

The Erlang Abstract Format is used in parse trees in various tools, such
as the compiler and preprocessor modules shipped with Erlang/OTP. A brief
documentation is available at http://erlang.org/doc/apps/erts/absform.html.
However, the complete syntax of abstract format is not documented.

This is an attempt to document every langugage construct by examples.
The examples are generated using Erlang source code,
which is parsed to generate the abstract form, and pretty-printed again to
generate the Erlang code examples.

Every form is a tuple. The second element is the line number in the source
file.

Preprocess directives such as -define, -include and -ifdef are currently
not included because `epp:parse_file/2` is used for parsing the file, which
consumes the preprocessor directives.

Table of Contents
-----------------

* [Top-level forms](#top-level-forms)
* [Types](#types)
* [Expressions](#expressions)


Top-level forms
---------------


**`-file("corpus.erl", 37).`**

```Erlang
{attribute,[{generated,true},{location,37}],file,{"corpus.erl",37}}
```

**`-module(corpus).`**

```Erlang
{attribute,38,module,corpus}
```

**`-compile(export_all).`**

```Erlang
{attribute,39,compile,export_all}
```

**`-vsn("1.2.3").`**

```Erlang
{attribute,40,vsn,"1.2.3"}
```

**`-on_load({my_function,0}).`**

```Erlang
{attribute,41,on_load,{my_function,0}}
```

**`-behaviour(gen_server).`**

```Erlang
{attribute,42,behaviour,gen_server}
```

**`-export([my_function/0]).`**

```Erlang
{attribute,43,export,[{my_function,0}]}
```

**`-export_type([my_type/0]).`**

```Erlang
{attribute,44,export_type,[{my_type,0}]}
```

**`-import(lists, [foldl/3]).`**

```Erlang
{attribute,45,import,{lists,[{foldl,3}]}}
```

**`-error(my_error).`** (`{attribute,46,error,my_error}` before OTP 19)

```Erlang
{error,{46,epp,{error,my_error}}}
```

**`-warning(my_warning).`** (`{attribute,47,warning,my_warning}` before OTP 19)

```Erlang
{warning,{47,epp,{warning,my_warning}}}
```

**`-include("include_not_found.hrl").`**

```Erlang
{error,{48,epp,{include,file,"include_not_found.hrl"}}}
```

**`-include_lib("include_lib/not_found.hrl").`**

```Erlang
{error,{49,epp,{include,lib,"include_lib/not_found.hrl"}}}
```

**`-type my_type() :: a | b.`**

```Erlang
{attribute,50,type,{my_type,{type,50,union,[{atom,50,a},{atom,50,b}]},[]}}
```

**`-opaque my_opaque() :: a | b.`**

```Erlang
{attribute,51,opaque,{my_opaque,{type,51,union,[{atom,51,a},{atom,51,b}]},[]}}
```

**`-type maybe(A) :: {just, A} | nothing.`**

```Erlang
{attribute,52,type,
           {maybe,{type,52,union,
                        [{type,52,tuple,[{atom,52,just},{var,52,'A'}]},
                         {atom,52,nothing}]},
                  [{var,52,'A'}]}}
```

**`-callback my_function() -> ok.`**

```Erlang
{attribute,53,callback,
           {{my_function,0},
            [{type,53,'fun',[{type,53,product,[]},{atom,53,ok}]}]}}
```

**`-spec my_function() -> ok.`**

```Erlang
{attribute,54,spec,
           {{my_function,0},
            [{type,54,'fun',[{type,54,product,[]},{atom,54,ok}]}]}}
```

**`-spec my_function(A :: atom()) -> B when B :: boolean().`**

```Erlang
{attribute,55,spec,
    {{my_function,1},
     [{type,55,bounded_fun,
          [{type,55,'fun',
               [{type,55,product,
                    [{ann_type,55,[{var,55,'A'},{type,55,atom,[]}]}]},
                {var,55,'B'}]},
           [{type,55,constraint,
                [{atom,55,is_subtype},
                 [{var,55,'B'},{type,55,boolean,[]}]]}]]}]}}
```

**`my_function() -> ok.`**

```Erlang
{function,56,my_function,0,[{clause,56,[],[],[{atom,56,ok}]}]}
```

**`my_function(X) when X > 2 -> X + 1.`**

```Erlang
{function,57,my_function,1,
          [{clause,57,
                   [{var,57,'X'}],
                   [[{op,57,'>',{var,57,'X'},{integer,57,2}}]],
                   [{op,57,'+',{var,57,'X'},{integer,57,1}}]}]}
```

Record declarations: Before OTP 19, a record is represented as a form without types. If any fields are typed,
the form is followed by a 'record type' form. Thus, for records with typed
fields, there are two consecutive forms. In OTP 19 and later, there is only
one form.

**`-record(untyped_rec,{field1 = foo,field2}). `**

```Erlang
{attribute,65,record,
           {untyped_rec,[{record_field,65,{atom,65,field1},{atom,65,foo}},
                         {record_field,66,{atom,66,field2}}]}}
```

**`-record(typed_rec,{field1 = foo :: atom(), field2 :: undefined, field3 = foo, field4}). `**

```Erlang
{attribute,68,record,
    {typed_rec,
        [{typed_record_field,
             {record_field,68,{atom,68,field1},{atom,68,foo}},
             {type,68,atom,[]}},
         {typed_record_field,
             {record_field,69,{atom,69,field2}},
             {atom,69,undefined}},
         {record_field,70,{atom,70,field3},{atom,70,foo}},
         {record_field,71,{atom,71,field4}}]}}
```

Types
-----

Types, mostly in the order as listed under "Types and their Syntax"
on http://erlang.org/doc/reference_manual/typespec.html.

Abstract form syntax for types: `{type, Line, TypeName, TypeParams}`.

**`any()`**

```Erlang
{type,82,any,[]}
```

**`none()`**

```Erlang
{type,83,none,[]}
```

**`pid()`**

```Erlang
{type,84,pid,[]}
```

**`port()`**

```Erlang
{type,85,port,[]}
```

**`reference()`**

```Erlang
{type,86,reference,[]}
```

**`[]`**

```Erlang
{type,87,nil,[]}
```

**`atom()`**

```Erlang
{type,88,atom,[]}
```

**`<<>>`** (the empty binary)

```Erlang
{type,92,binary,[{integer,92,0},{integer,92,0}]}
```

**`<<_:M>>`** (M is a positive integer)

```Erlang
{type,94,binary,[{var,94,'M'},{integer,94,0}]}
```

**`<<_:_*N>>`** (N is a positive integer)

```Erlang
{type,96,binary,[{integer,96,0},{var,96,'N'}]}
```

**`<<_:M, _:_*N>>`**

```Erlang
{type,97,binary,[{var,97,'M'},{var,97,'N'}]}
```

**`float()`**

```Erlang
{type,99,float,[]}
```

**`fun()`** (any function)

```Erlang
{type,103,'fun',[]}
```

**`fun((...) -> integer())`** (any arity, returning Type)

```Erlang
{type,105,'fun',[{type,105,any},{type,105,integer,[]}]}
```

**`fun(() -> integer())`**

```Erlang
{type,106,'fun',[{type,106,product,[]},{type,106,integer,[]}]}
```

**`fun((atom(), atom()) -> integer())`**

```Erlang
{type,107,'fun',
      [{type,107,product,[{type,107,atom,[]},{type,107,atom,[]}]},
       {type,107,integer,[]}]}
```

**`integer()`**

```Erlang
{type,110,integer,[]}
```

**`42`** (..., -1, 0, 1, ... 42 ...)

```Erlang
{integer,112,42}
```

**`1..10`** (specifies an integer range)

```Erlang
{type,114,range,[{integer,114,1},{integer,114,10}]}
```

**`[integer()]`** (Proper list ([]-terminated))

```Erlang
{type,118,list,[{type,118,integer,[]}]}
```

**`maybe_improper_list(integer(), atom())`** (Type1=contents, Type2=termination)

```Erlang
{type,120,maybe_improper_list,[{type,120,integer,[]},{type,120,atom,[]}]}
```

**`nonempty_improper_list(integer(), atom())`** (Type1 and Type2 as above)

```Erlang
{type,122,nonempty_improper_list,[{type,122,integer,[]},{type,122,atom,[]}]}
```

**`[integer(), ...]`** (Proper non-empty list)

```Erlang
{type,124,nonempty_list,[{type,124,integer,[]}]}
```

**`map()`** (denotes a map of any size)

```Erlang
{type,128,map,any}
```

**`#{}`** (denotes the empty map)

```Erlang
{type,130,map,[]}
```

**`#{size := integer(), integer() => any()}`**

```Erlang
{type,131,map,
      [{type,131,map_field_exact,[{atom,131,size},{type,131,integer,[]}]},
       {type,132,map_field_assoc,[{type,132,integer,[]},{type,132,any,[]}]}]}
```

**`tuple()`**

```Erlang
{type,135,tuple,any}
```

**`{}`**

```Erlang
{type,136,tuple,[]}
```

**`{atom()}`**

```Erlang
{type,137,tuple,[{type,137,atom,[]}]}
```

**`{atom(), integer()}`**

```Erlang
{type,138,tuple,[{type,138,atom,[]},{type,138,integer,[]}]}
```

**`#my_record{}`**

```Erlang
{type,141,record,[{atom,141,my_record}]}
```

**`atom() | integer()`**

```Erlang
{type,144,union,[{type,144,atom,[]},{type,144,integer,[]}]}
```

**`my_type()`**

```Erlang
{user_type,147,my_type,[]}
```

**`maybe(integer())`**

```Erlang
{user_type,148,maybe,[{type,148,integer,[]}]}
```

**`module:type()`**

```Erlang
{remote_type,149,[{atom,149,module},{atom,149,type},[]]}
```

For convenience, the following types are also built-in. They can be
thought as predefined aliases for the types shown in the comments.


**`term()`** (term() :: any())

```Erlang
{type,163,term,[]}
```

**`binary()`** (binary() :: <<\_:\_*8>>)

```Erlang
{type,165,binary,[]}
```

**`bitstring()`** (bitstring() :: <<\_:\_*1>>)

```Erlang
{type,167,bitstring,[]}
```

**`boolean()`** (boolean() :: 'false' | 'true')

```Erlang
{type,169,boolean,[]}
```

**`byte()`** (byte() :: 0..255)

```Erlang
{type,171,byte,[]}
```

**`char()`** (char() :: 0..16#10ffff)

```Erlang
{type,173,char,[]}
```

**`[]`** (nil() :: [])

```Erlang
{type,175,nil,[]}
```

**`number()`** (number() :: integer() | float())

```Erlang
{type,177,number,[]}
```

**`list()`** (list() :: [any()])

```Erlang
{type,179,list,[]}
```

**`maybe_improper_list()`** (maybe_improper_list() :: maybe_improper_list(any(), any()))

```Erlang
{type,181,maybe_improper_list,[]}
```

**`nonempty_list()`** (nonempty_list() :: nonempty_list(any()))

```Erlang
{type,183,nonempty_list,[]}
```

**`string()`** (string() :: [char()])

```Erlang
{type,185,string,[]}
```

**`nonempty_string()`** (nonempty_string() :: [char(),...])

```Erlang
{type,187,nonempty_string,[]}
```

**`iodata()`** (iodata() :: iolist() | binary())

```Erlang
{type,189,iodata,[]}
```

**`iolist()`** (iolist() :: maybe_improper_list(byte() | binary() | iolist(), binary() | []))

```Erlang
{type,191,iolist,[]}
```

**`function()`** (function() :: fun())

```Erlang
{type,193,function,[]}
```

**`module()`** (module() :: atom())

```Erlang
{type,195,module,[]}
```

**`mfa()`** (mfa() :: {module(),atom(),arity()})

```Erlang
{type,197,mfa,[]}
```

**`arity()`** (arity() :: 0..255)

```Erlang
{type,199,arity,[]}
```

**`identifier()`** (identifier() :: pid() | port() | reference())

```Erlang
{type,201,identifier,[]}
```

**`node()`** (node() :: atom())

```Erlang
{type,203,node,[]}
```

**`timeout()`** (timeout() :: 'infinity' | non_neg_integer())

```Erlang
{type,205,timeout,[]}
```

**`no_return()`** (no_return() :: none())

```Erlang
{type,207,no_return,[]}
```

Built-in types; can be thought defined by the syntax...

**`non_neg_integer()`** (non_neg_integer() :: 0..)

```Erlang
{type,213,non_neg_integer,[]}
```

**`pos_integer()`** (pos_integer() :: 1..)

```Erlang
{type,215,pos_integer,[]}
```

**`neg_integer()`** (neg_integer() :: ..-1)

```Erlang
{type,217,neg_integer,[]}
```

Exressions
----------


**`42`**

```Erlang
{integer,224,42}
```

**`3.141592653589`**

```Erlang
{float,225,3.141592653589}
```

**`ok`**

```Erlang
{atom,226,ok}
```

**`[]`**

```Erlang
{nil,227}
```

**`[x,y]`**

```Erlang
{cons,228,{atom,228,x},{cons,228,{atom,228,y},{nil,228}}}
```

**`[x|XS]`**

```Erlang
{cons,229,{atom,229,x},{var,229,'XS'}}
```

**`""`**

```Erlang
{string,230,[]}
```

**`"abc"`**

```Erlang
{string,231,"abc"}
```

**`<<"abc">>`**

```Erlang
{bin,232,[{bin_element,232,{string,232,"abc"},default,default}]}
```

**`<<A:8/integer,B:32/float-little,C/binary>>`**

```Erlang
{bin,233,
     [{bin_element,233,{var,233,'A'},{integer,233,8},[integer]},
      {bin_element,233,{var,233,'B'},{integer,233,32},[float,little]},
      {bin_element,233,{var,233,'C'},default,[binary]}]}
```

**`<<"abc"/utf8,XYZ/utf16>>`**

```Erlang
{bin,234,
     [{bin_element,234,{string,234,"abc"},default,[utf8]},
      {bin_element,234,{var,234,'XYZ'},default,[utf16]}]}
```

**`#{old_key := updated_value,new_key => 42}`**

```Erlang
{map,235,
     [{map_field_exact,235,{atom,235,old_key},{atom,235,updated_value}},
      {map_field_assoc,235,{atom,235,new_key},{integer,235,42}}]}
```

**`{x,y}`**

```Erlang
{tuple,236,[{atom,236,x},{atom,236,y}]}
```

**`#my_record{foo = X}`**

```Erlang
{record,237,my_record,[{record_field,237,{atom,237,foo},{var,237,'X'}}]}
```

**`Rec#my_record.foo`**

```Erlang
{record_field,238,{var,238,'Rec'},my_record,{atom,238,foo}}
```

**`fun f/1`**

```Erlang
{'fun',239,{function,f,1}}
```

**`fun m:f/1`**

```Erlang
{'fun',240,{function,{atom,240,m},{atom,240,f},{integer,240,1}}}
```

**`X`**

```Erlang
{var,243,'X'}
```

**`_`**

```Erlang
{var,244,'_'}
```

**`not true`**

```Erlang
{op,247,'not',{atom,247,true}}
```

**`1 + 1`**

```Erlang
{op,248,'+',{integer,248,1},{integer,248,1}}
```

**`1 == 1`**

```Erlang
{op,249,'==',{integer,249,1},{integer,249,1}}
```

**`X and Y`**

```Erlang
{op,250,'and',{var,250,'X'},{var,250,'Y'}}
```

**`X andalso Y`**

```Erlang
{op,251,'andalso',{var,251,'X'},{var,251,'Y'}}
```

**`f(42)`**

```Erlang
{call,254,{atom,254,f},[{integer,254,42}]}
```

**`m:f(42)`**

```Erlang
{call,255,{remote,255,{atom,255,m},{atom,255,f}},[{integer,255,42}]}
```

**`X = 42`**

```Erlang
{match,256,{var,256,'X'},{integer,256,42}}
```

**`fun(42) -> true; (_) -> false end`**

```Erlang
{'fun',257,
       {clauses,[{clause,257,[{integer,257,42}],[],[{atom,257,true}]},
                 {clause,257,[{var,257,'_'}],[],[{atom,257,false}]}]}}
```

**`fun(X) when is_atom(X) -> X end`**

```Erlang
{'fun',258,
       {clauses,[{clause,258,
                         [{var,258,'X'}],
                         [[{call,258,{atom,258,is_atom},[{var,258,'X'}]}]],
                         [{var,258,'X'}]}]}}
```

**`if P -> hello; true -> ok end`**

```Erlang
{'if',259,
      [{clause,259,[],[[{var,259,'P'}]],[{atom,259,hello}]},
       {clause,259,[],[[{atom,259,true}]],[{atom,259,ok}]}]}
```

**`case foo of bar -> baz; _ -> ok end`**

```Erlang
{'case',260,
        {atom,260,foo},
        [{clause,260,[{atom,260,bar}],[],[{atom,260,baz}]},
         {clause,260,[{var,260,'_'}],[],[{atom,260,ok}]}]}
```

**`begin ok, ok end`**

```Erlang
{block,261,[{atom,261,ok},{atom,261,ok}]}
```

**`Pid ! message`**

```Erlang
{op,262,'!',{var,262,'Pid'},{atom,262,message}}
```

**`receive X -> ok end`**

```Erlang
{'receive',263,[{clause,263,[{var,263,'X'}],[],[{atom,263,ok}]}]}
```

**`receive X -> ok after 1000 -> timeout end`**

```Erlang
{'receive',264,
           [{clause,264,[{var,264,'X'}],[],[{atom,264,ok}]}],
           {integer,264,1000},
           [{atom,264,timeout}]}
```

**`try f() catch error:E -> fail end`**

```Erlang
{'try',265,
       [{call,265,{atom,265,f},[]}],
       [],
       [{clause,265,
                [{tuple,265,[{atom,265,error},{var,265,'E'},{var,265,'_'}]}],
                [],
                [{atom,265,fail}]}],
       []}
```

**`try f(), g() of X when is_integer(X) -> X catch C:E when is_tuple(E) -> fail after afterwards() end`**

```Erlang
{'try',266,
       [{call,266,{atom,266,f},[]},{call,266,{atom,266,g},[]}],
       [{clause,266,
                [{var,266,'X'}],
                [[{call,266,{atom,266,is_integer},[{var,266,'X'}]}]],
                [{var,266,'X'}]}],
       [{clause,266,
                [{tuple,266,[{var,266,'C'},{var,266,'E'},{var,266,'_'}]}],
                [[{call,266,{atom,266,is_tuple},[{var,266,'E'}]}]],
                [{atom,266,fail}]}],
       [{call,266,{atom,266,afterwards},[]}]}
```

**`catch X`**

```Erlang
{'catch',267,{var,267,'X'}}
```

**`[ 2 || is_integer(2) ]`**

```Erlang
{lc,270,{integer,270,2},[{call,270,{atom,270,is_integer},[{integer,270,2}]}]}
```

**`[ X || X <- XS, is_atom(X) ]`**

```Erlang
{lc,271,
    {var,271,'X'},
    [{generate,271,{var,271,'X'},{var,271,'XS'}},
     {call,271,{atom,271,is_atom},[{var,271,'X'}]}]}
```

**`<< <<X:4>> || <<X:4>> <= Bin, X /= 0 >>`**

```Erlang
{bc,272,
    {bin,272,[{bin_element,272,{var,272,'X'},{integer,272,4},default}]},
    [{b_generate,272,
                 {bin,272,
                      [{bin_element,272,
                                    {var,272,'X'},
                                    {integer,272,4},
                                    default}]},
                 {var,272,'Bin'}},
     {op,272,'/=',{var,272,'X'},{integer,272,0}}]}
```

