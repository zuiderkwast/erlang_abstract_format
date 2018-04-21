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

Top-level forms
---------------


**`-file("corpus.erl", 30).`**

```Erlang
{attribute,[{generated,true},{location,30}],file,{"corpus.erl",30}}
```

**`-module(corpus).`**

```Erlang
{attribute,31,module,corpus}
```

**`-compile(export_all).`**

```Erlang
{attribute,32,compile,export_all}
```

**`-vsn("1.2.3").`**

```Erlang
{attribute,33,vsn,"1.2.3"}
```

**`-on_load({my_function,0}).`**

```Erlang
{attribute,34,on_load,{my_function,0}}
```

**`-behaviour(gen_server).`**

```Erlang
{attribute,35,behaviour,gen_server}
```

**`-export([my_function/0]).`**

```Erlang
{attribute,36,export,[{my_function,0}]}
```

**`-export_type([my_type/0]).`**

```Erlang
{attribute,37,export_type,[{my_type,0}]}
```

**`-import(lists, [foldl/3]).`**

```Erlang
{attribute,38,import,{lists,[{foldl,3}]}}
```

**`-error(my_error).`** (`{attribute,39,error,my_error}` before OTP 19)

```Erlang
{error,{39,epp,{error,my_error}}}
```

**`-warning(my_warning).`** (`{attribute,40,warning,my_warning}` before OTP 19)

```Erlang
{warning,{40,epp,{warning,my_warning}}}
```

**`-include("include_not_found.hrl").`**

```Erlang
{error,{41,epp,{include,file,"include_not_found.hrl"}}}
```

**`-include_lib("include_lib/not_found.hrl").`**

```Erlang
{error,{42,epp,{include,lib,"include_lib/not_found.hrl"}}}
```

**`-type my_type() :: a | b.`**

```Erlang
{attribute,43,type,{my_type,{type,43,union,[{atom,43,a},{atom,43,b}]},[]}}
```

**`-opaque my_opaque() :: a | b.`**

```Erlang
{attribute,44,opaque,{my_opaque,{type,44,union,[{atom,44,a},{atom,44,b}]},[]}}
```

**`-type maybe(A) :: {just, A} | nothing.`**

```Erlang
{attribute,45,type,
           {maybe,{type,45,union,
                        [{type,45,tuple,[{atom,45,just},{var,45,'A'}]},
                         {atom,45,nothing}]},
                  [{var,45,'A'}]}}
```

**`-callback my_function() -> ok.`**

```Erlang
{attribute,46,callback,
           {{my_function,0},
            [{type,46,'fun',[{type,46,product,[]},{atom,46,ok}]}]}}
```

**`-spec my_function() -> ok.`**

```Erlang
{attribute,47,spec,
           {{my_function,0},
            [{type,47,'fun',[{type,47,product,[]},{atom,47,ok}]}]}}
```

**`my_function() -> ok.`**

```Erlang
{function,48,my_function,0,[{clause,48,[],[],[{atom,48,ok}]}]}
```

**`my_function(X) when X > 2 -> X + 1.`**

```Erlang
{function,49,my_function,1,
          [{clause,49,
                   [{var,49,'X'}],
                   [[{op,49,'>',{var,49,'X'},{integer,49,2}}]],
                   [{op,49,'+',{var,49,'X'},{integer,49,1}}]}]}
```

Record declarations: A record is represented as a form without types. If any fields are typed,
the form is followed by a 'record type' form. Thus, for records with typed
fields, there are two consecutive forms.

**`-record(myrec,{field1 = foo :: atom(), field2 :: undefined, field3 = foo, field4}). `**

```Erlang
{attribute,56,record,
    {myrec,
        [{typed_record_field,
             {record_field,56,{atom,56,field1},{atom,56,foo}},
             {type,56,atom,[]}},
         {typed_record_field,
             {record_field,57,{atom,57,field2}},
             {atom,57,undefined}},
         {record_field,58,{atom,58,field3},{atom,58,foo}},
         {record_field,59,{atom,59,field4}}]}}
```

Types
-----

Types, mostly in the order as listed under "Types and their Syntax"
on http://erlang.org/doc/reference_manual/typespec.html.

Abstract form syntax for types: `{type, Line, TypeName, TypeParams}`.

**`any()`**

```Erlang
{type,70,any,[]}
```

**`none()`**

```Erlang
{type,71,none,[]}
```

**`pid()`**

```Erlang
{type,72,pid,[]}
```

**`port()`**

```Erlang
{type,73,port,[]}
```

**`reference()`**

```Erlang
{type,74,reference,[]}
```

**`[]`**

```Erlang
{type,75,nil,[]}
```

**`atom()`**

```Erlang
{type,76,atom,[]}
```

**`<<>>`**

```Erlang
{type,79,binary,[{integer,79,0},{integer,79,0}]}
```

**`<<_:M>>`** (M is a positive integer)

```Erlang
{type,81,binary,[{var,81,'M'},{integer,81,0}]}
```

**`<<_:_*N>>`** (N is a positive integer)

```Erlang
{type,83,binary,[{integer,83,0},{var,83,'N'}]}
```

**`<<_:M, _:_*N>>`**

```Erlang
{type,84,binary,[{var,84,'M'},{var,84,'N'}]}
```

**`float()`**

```Erlang
{type,86,float,[]}
```

**`fun()`** (any function)

```Erlang
{type,90,'fun',[]}
```

**`fun((...) -> integer())`** (any arity, returning Type)

```Erlang
{type,92,'fun',[{type,92,any},{type,92,integer,[]}]}
```

**`fun(() -> integer())`**

```Erlang
{type,93,'fun',[{type,93,product,[]},{type,93,integer,[]}]}
```

**`fun((atom(), atom()) -> integer())`**

```Erlang
{type,94,'fun',
      [{type,94,product,[{type,94,atom,[]},{type,94,atom,[]}]},
       {type,94,integer,[]}]}
```

**`integer()`**

```Erlang
{type,97,integer,[]}
```

**`42`** (..., -1, 0, 1, ... 42 ...)

```Erlang
{integer,99,42}
```

**`1..10`** (specifies an integer range)

```Erlang
{type,101,range,[{integer,101,1},{integer,101,10}]}
```

**`[integer()]`** (Proper list ([]-terminated))

```Erlang
{type,105,list,[{type,105,integer,[]}]}
```

**`maybe_improper_list(integer(), atom())`** (Type1=contents, Type2=termination)

```Erlang
{type,107,maybe_improper_list,[{type,107,integer,[]},{type,107,atom,[]}]}
```

**`nonempty_improper_list(integer(), atom())`** (Type1 and Type2 as above)

```Erlang
{type,109,nonempty_improper_list,[{type,109,integer,[]},{type,109,atom,[]}]}
```

**`[integer(), ...]`** (Proper non-empty list)

```Erlang
{type,111,nonempty_list,[{type,111,integer,[]}]}
```

**`map()`** (denotes a map of any size)

```Erlang
{type,115,map,any}
```

**`#{}`** (denotes the empty map)

```Erlang
{type,117,map,[]}
```

**`#{size := integer(), integer() => any()}`**

```Erlang
{type,118,map,
      [{type,118,map_field_exact,[{atom,118,size},{type,118,integer,[]}]},
       {type,119,map_field_assoc,[{type,119,integer,[]},{type,119,any,[]}]}]}
```

**`tuple()`**

```Erlang
{type,122,tuple,any}
```

**`{}`**

```Erlang
{type,123,tuple,[]}
```

**`{atom()}`**

```Erlang
{type,124,tuple,[{type,124,atom,[]}]}
```

**`{atom(), integer()}`**

```Erlang
{type,125,tuple,[{type,125,atom,[]},{type,125,integer,[]}]}
```

**`#my_record{}`**

```Erlang
{type,128,record,[{atom,128,my_record}]}
```

**`atom() | integer()`**

```Erlang
{type,131,union,[{type,131,atom,[]},{type,131,integer,[]}]}
```

**`my_type()`**

```Erlang
{user_type,134,my_type,[]}
```

**`maybe(integer())`**

```Erlang
{user_type,135,maybe,[{type,135,integer,[]}]}
```

**`module:type()`**

```Erlang
{remote_type,136,[{atom,136,module},{atom,136,type},[]]}
```

For convenience, the following types are also built-in. They can be
thought as predefined aliases for the type unions shown in the code
comments.

**`term()`** (term() :: any())

```Erlang
{type,151,term,[]}
```

**`binary()`** (binary() :: <<_:_*8>>)

```Erlang
{type,153,binary,[]}
```

**`bitstring()`** (bitstring() :: <<_:_*1>>)

```Erlang
{type,155,bitstring,[]}
```

**`boolean()`** (boolean() :: 'false' | 'true')

```Erlang
{type,157,boolean,[]}
```

**`byte()`** (byte() :: 0..255)

```Erlang
{type,159,byte,[]}
```

**`char()`** (char() :: 0..16#10ffff)

```Erlang
{type,161,char,[]}
```

**`[]`** (nil() :: [])

```Erlang
{type,163,nil,[]}
```

**`number()`** (number() :: integer() | float())

```Erlang
{type,165,number,[]}
```

**`list()`** (list() :: [any()])

```Erlang
{type,167,list,[]}
```

**`maybe_improper_list()`** (maybe_improper_list() :: maybe_improper_list(any(), any()))

```Erlang
{type,169,maybe_improper_list,[]}
```

**`nonempty_list()`** (nonempty_list() :: nonempty_list(any()))

```Erlang
{type,171,nonempty_list,[]}
```

**`string()`** (string() :: [char()])

```Erlang
{type,173,string,[]}
```

**`nonempty_string()`** (nonempty_string() :: [char(),...])

```Erlang
{type,175,nonempty_string,[]}
```

**`iodata()`** (iodata() :: iolist() | binary())

```Erlang
{type,177,iodata,[]}
```

**`iolist()`** (iolist() :: maybe_improper_list(byte() | binary() | iolist(), binary() | []))

```Erlang
{type,179,iolist,[]}
```

**`function()`** (function() :: fun())

```Erlang
{type,181,function,[]}
```

**`module()`** (module() :: atom())

```Erlang
{type,183,module,[]}
```

**`mfa()`** (mfa() :: {module(),atom(),arity()})

```Erlang
{type,185,mfa,[]}
```

**`arity()`** (arity() :: 0..255)

```Erlang
{type,187,arity,[]}
```

**`identifier()`** (identifier() :: pid() | port() | reference())

```Erlang
{type,189,identifier,[]}
```

**`node()`** (node() :: atom())

```Erlang
{type,191,node,[]}
```

**`timeout()`** (timeout() :: 'infinity' | non_neg_integer())

```Erlang
{type,193,timeout,[]}
```

**`no_return()`** (no_return() :: none())

```Erlang
{type,195,no_return,[]}
```

Built-in types; can be thought defined by the syntax...

**`non_neg_integer()`** (non_neg_integer() :: 0..)

```Erlang
{type,201,non_neg_integer,[]}
```

**`pos_integer()`** (pos_integer() :: 1..)

```Erlang
{type,203,pos_integer,[]}
```

**`neg_integer()`** (neg_integer() :: ..-1)

```Erlang
{type,205,neg_integer,[]}
```

Exressions
----------


**`42`**

```Erlang
{integer,212,42}
```

**`3.141592653589`**

```Erlang
{float,213,3.141592653589}
```

**`ok`**

```Erlang
{atom,214,ok}
```

**`[]`**

```Erlang
{nil,215}
```

**`[x,y]`**

```Erlang
{cons,216,{atom,216,x},{cons,216,{atom,216,y},{nil,216}}}
```

**`[x|XS]`**

```Erlang
{cons,217,{atom,217,x},{var,217,'XS'}}
```

**`""`**

```Erlang
{string,218,[]}
```

**`"abc"`**

```Erlang
{string,219,"abc"}
```

**`<<"abc">>`**

```Erlang
{bin,220,[{bin_element,220,{string,220,"abc"},default,default}]}
```

**`<<A:8/integer,B:32/float-little,C/binary>>`**

```Erlang
{bin,221,
     [{bin_element,221,{var,221,'A'},{integer,221,8},[integer]},
      {bin_element,221,{var,221,'B'},{integer,221,32},[float,little]},
      {bin_element,221,{var,221,'C'},default,[binary]}]}
```

**`<<"abc"/utf8,XYZ/utf16>>`**

```Erlang
{bin,222,
     [{bin_element,222,{string,222,"abc"},default,[utf8]},
      {bin_element,222,{var,222,'XYZ'},default,[utf16]}]}
```

**`#{old_key := updated_value,new_key => 42}`**

```Erlang
{map,223,
     [{map_field_exact,223,{atom,223,old_key},{atom,223,updated_value}},
      {map_field_assoc,223,{atom,223,new_key},{integer,223,42}}]}
```

**`{x,y}`**

```Erlang
{tuple,224,[{atom,224,x},{atom,224,y}]}
```

**`#my_record{foo = X}`**

```Erlang
{record,225,my_record,[{record_field,225,{atom,225,foo},{var,225,'X'}}]}
```

**`Rec#my_record.foo`**

```Erlang
{record_field,226,{var,226,'Rec'},my_record,{atom,226,foo}}
```

**`fun f/1`**

```Erlang
{'fun',227,{function,f,1}}
```

**`fun m:f/1`**

```Erlang
{'fun',228,{function,{atom,228,m},{atom,228,f},{integer,228,1}}}
```

**`X`**

```Erlang
{var,231,'X'}
```

**`_`**

```Erlang
{var,232,'_'}
```

**`not true`**

```Erlang
{op,235,'not',{atom,235,true}}
```

**`1 + 1`**

```Erlang
{op,236,'+',{integer,236,1},{integer,236,1}}
```

**`1 == 1`**

```Erlang
{op,237,'==',{integer,237,1},{integer,237,1}}
```

**`X and Y`**

```Erlang
{op,238,'and',{var,238,'X'},{var,238,'Y'}}
```

**`X andalso Y`**

```Erlang
{op,239,'andalso',{var,239,'X'},{var,239,'Y'}}
```

**`f(42)`**

```Erlang
{call,242,{atom,242,f},[{integer,242,42}]}
```

**`m:f(42)`**

```Erlang
{call,243,{remote,243,{atom,243,m},{atom,243,f}},[{integer,243,42}]}
```

**`X = 42`**

```Erlang
{match,244,{var,244,'X'},{integer,244,42}}
```

**`fun(42) -> true; (_) -> false end`**

```Erlang
{'fun',245,
       {clauses,[{clause,245,[{integer,245,42}],[],[{atom,245,true}]},
                 {clause,245,[{var,245,'_'}],[],[{atom,245,false}]}]}}
```

**`fun(X) when is_atom(X) -> X end`**

```Erlang
{'fun',246,
       {clauses,[{clause,246,
                         [{var,246,'X'}],
                         [[{call,246,{atom,246,is_atom},[{var,246,'X'}]}]],
                         [{var,246,'X'}]}]}}
```

**`if P -> hello; true -> ok end`**

```Erlang
{'if',247,
      [{clause,247,[],[[{var,247,'P'}]],[{atom,247,hello}]},
       {clause,247,[],[[{atom,247,true}]],[{atom,247,ok}]}]}
```

**`case foo of bar -> baz; _ -> ok end`**

```Erlang
{'case',248,
        {atom,248,foo},
        [{clause,248,[{atom,248,bar}],[],[{atom,248,baz}]},
         {clause,248,[{var,248,'_'}],[],[{atom,248,ok}]}]}
```

**`begin ok, ok end`**

```Erlang
{block,249,[{atom,249,ok},{atom,249,ok}]}
```

**`Pid ! message`**

```Erlang
{op,250,'!',{var,250,'Pid'},{atom,250,message}}
```

**`receive X -> ok end`**

```Erlang
{'receive',251,[{clause,251,[{var,251,'X'}],[],[{atom,251,ok}]}]}
```

**`receive X -> ok after 1000 -> timeout end`**

```Erlang
{'receive',252,
           [{clause,252,[{var,252,'X'}],[],[{atom,252,ok}]}],
           {integer,252,1000},
           [{atom,252,timeout}]}
```

**`try f() catch error:E -> fail end`**

```Erlang
{'try',253,
       [{call,253,{atom,253,f},[]}],
       [],
       [{clause,253,
                [{tuple,253,[{atom,253,error},{var,253,'E'},{var,253,'_'}]}],
                [],
                [{atom,253,fail}]}],
       []}
```

**`try f(), g() of X when is_integer(X) -> X catch C:E when is_tuple(E) -> fail after afterwards() end`**

```Erlang
{'try',254,
       [{call,254,{atom,254,f},[]},{call,254,{atom,254,g},[]}],
       [{clause,254,
                [{var,254,'X'}],
                [[{call,254,{atom,254,is_integer},[{var,254,'X'}]}]],
                [{var,254,'X'}]}],
       [{clause,254,
                [{tuple,254,[{var,254,'C'},{var,254,'E'},{var,254,'_'}]}],
                [[{call,254,{atom,254,is_tuple},[{var,254,'E'}]}]],
                [{atom,254,fail}]}],
       [{call,254,{atom,254,afterwards},[]}]}
```

**`catch X`**

```Erlang
{'catch',255,{var,255,'X'}}
```

**`[ 2 || is_integer(2) ]`**

```Erlang
{lc,258,{integer,258,2},[{call,258,{atom,258,is_integer},[{integer,258,2}]}]}
```

**`[ X || X <- XS, is_atom(X) ]`**

```Erlang
{lc,259,
    {var,259,'X'},
    [{generate,259,{var,259,'X'},{var,259,'XS'}},
     {call,259,{atom,259,is_atom},[{var,259,'X'}]}]}
```

**`<< <<X:4>> || <<X:4>> <= Bin, X /= 0 >>`**

```Erlang
{bc,260,
    {bin,260,[{bin_element,260,{var,260,'X'},{integer,260,4},default}]},
    [{b_generate,260,
                 {bin,260,
                      [{bin_element,260,
                                    {var,260,'X'},
                                    {integer,260,4},
                                    default}]},
                 {var,260,'Bin'}},
     {op,260,'/=',{var,260,'X'},{integer,260,0}}]}
```

