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

**`-spec my_function(A :: atom()) -> B when B :: boolean().`**

```Erlang
{attribute,48,spec,
    {{my_function,1},
     [{type,48,bounded_fun,
          [{type,48,'fun',
               [{type,48,product,
                    [{ann_type,48,[{var,48,'A'},{type,48,atom,[]}]}]},
                {var,48,'B'}]},
           [{type,48,constraint,
                [{atom,48,is_subtype},
                 [{var,48,'B'},{type,48,boolean,[]}]]}]]}]}}
```

**`my_function() -> ok.`**

```Erlang
{function,49,my_function,0,[{clause,49,[],[],[{atom,49,ok}]}]}
```

**`my_function(X) when X > 2 -> X + 1.`**

```Erlang
{function,50,my_function,1,
          [{clause,50,
                   [{var,50,'X'}],
                   [[{op,50,'>',{var,50,'X'},{integer,50,2}}]],
                   [{op,50,'+',{var,50,'X'},{integer,50,1}}]}]}
```

Record declarations: A record is represented as a form without types. If any fields are typed,
the form is followed by a 'record type' form. Thus, for records with typed
fields, there are two consecutive forms.

**`-record(myrec,{field1 = foo :: atom(), field2 :: undefined, field3 = foo, field4}). `**

```Erlang
{attribute,61,record,
    {myrec,
        [{typed_record_field,
             {record_field,61,{atom,61,field1},{atom,61,foo}},
             {type,61,atom,[]}},
         {typed_record_field,
             {record_field,62,{atom,62,field2}},
             {atom,62,undefined}},
         {record_field,63,{atom,63,field3},{atom,63,foo}},
         {record_field,64,{atom,64,field4}}]}}
```

Types
-----

Types, mostly in the order as listed under "Types and their Syntax"
on http://erlang.org/doc/reference_manual/typespec.html.

Abstract form syntax for types: `{type, Line, TypeName, TypeParams}`.

**`any()`**

```Erlang
{type,75,any,[]}
```

**`none()`**

```Erlang
{type,76,none,[]}
```

**`pid()`**

```Erlang
{type,77,pid,[]}
```

**`port()`**

```Erlang
{type,78,port,[]}
```

**`reference()`**

```Erlang
{type,79,reference,[]}
```

**`[]`**

```Erlang
{type,80,nil,[]}
```

**`atom()`**

```Erlang
{type,81,atom,[]}
```

**`<<>>`**

```Erlang
{type,84,binary,[{integer,84,0},{integer,84,0}]}
```

**`<<_:M>>`** (M is a positive integer)

```Erlang
{type,86,binary,[{var,86,'M'},{integer,86,0}]}
```

**`<<_:_*N>>`** (N is a positive integer)

```Erlang
{type,88,binary,[{integer,88,0},{var,88,'N'}]}
```

**`<<_:M, _:_*N>>`**

```Erlang
{type,89,binary,[{var,89,'M'},{var,89,'N'}]}
```

**`float()`**

```Erlang
{type,91,float,[]}
```

**`fun()`** (any function)

```Erlang
{type,95,'fun',[]}
```

**`fun((...) -> integer())`** (any arity, returning Type)

```Erlang
{type,97,'fun',[{type,97,any},{type,97,integer,[]}]}
```

**`fun(() -> integer())`**

```Erlang
{type,98,'fun',[{type,98,product,[]},{type,98,integer,[]}]}
```

**`fun((atom(), atom()) -> integer())`**

```Erlang
{type,99,'fun',
      [{type,99,product,[{type,99,atom,[]},{type,99,atom,[]}]},
       {type,99,integer,[]}]}
```

**`integer()`**

```Erlang
{type,102,integer,[]}
```

**`42`** (..., -1, 0, 1, ... 42 ...)

```Erlang
{integer,104,42}
```

**`1..10`** (specifies an integer range)

```Erlang
{type,106,range,[{integer,106,1},{integer,106,10}]}
```

**`[integer()]`** (Proper list ([]-terminated))

```Erlang
{type,110,list,[{type,110,integer,[]}]}
```

**`maybe_improper_list(integer(), atom())`** (Type1=contents, Type2=termination)

```Erlang
{type,112,maybe_improper_list,[{type,112,integer,[]},{type,112,atom,[]}]}
```

**`nonempty_improper_list(integer(), atom())`** (Type1 and Type2 as above)

```Erlang
{type,114,nonempty_improper_list,[{type,114,integer,[]},{type,114,atom,[]}]}
```

**`[integer(), ...]`** (Proper non-empty list)

```Erlang
{type,116,nonempty_list,[{type,116,integer,[]}]}
```

**`map()`** (denotes a map of any size)

```Erlang
{type,120,map,any}
```

**`#{}`** (denotes the empty map)

```Erlang
{type,122,map,[]}
```

**`#{size := integer(), integer() => any()}`**

```Erlang
{type,123,map,
      [{type,123,map_field_exact,[{atom,123,size},{type,123,integer,[]}]},
       {type,124,map_field_assoc,[{type,124,integer,[]},{type,124,any,[]}]}]}
```

**`tuple()`**

```Erlang
{type,127,tuple,any}
```

**`{}`**

```Erlang
{type,128,tuple,[]}
```

**`{atom()}`**

```Erlang
{type,129,tuple,[{type,129,atom,[]}]}
```

**`{atom(), integer()}`**

```Erlang
{type,130,tuple,[{type,130,atom,[]},{type,130,integer,[]}]}
```

**`#my_record{}`**

```Erlang
{type,133,record,[{atom,133,my_record}]}
```

**`atom() | integer()`**

```Erlang
{type,136,union,[{type,136,atom,[]},{type,136,integer,[]}]}
```

**`my_type()`**

```Erlang
{user_type,139,my_type,[]}
```

**`maybe(integer())`**

```Erlang
{user_type,140,maybe,[{type,140,integer,[]}]}
```

**`module:type()`**

```Erlang
{remote_type,141,[{atom,141,module},{atom,141,type},[]]}
```

For convenience, the following types are also built-in. They can be
thought as predefined aliases for the type unions shown in the code
comments.

**`term()`** (term() :: any())

```Erlang
{type,156,term,[]}
```

**`binary()`** (binary() :: <<_:_*8>>)

```Erlang
{type,158,binary,[]}
```

**`bitstring()`** (bitstring() :: <<_:_*1>>)

```Erlang
{type,160,bitstring,[]}
```

**`boolean()`** (boolean() :: 'false' | 'true')

```Erlang
{type,162,boolean,[]}
```

**`byte()`** (byte() :: 0..255)

```Erlang
{type,164,byte,[]}
```

**`char()`** (char() :: 0..16#10ffff)

```Erlang
{type,166,char,[]}
```

**`[]`** (nil() :: [])

```Erlang
{type,168,nil,[]}
```

**`number()`** (number() :: integer() | float())

```Erlang
{type,170,number,[]}
```

**`list()`** (list() :: [any()])

```Erlang
{type,172,list,[]}
```

**`maybe_improper_list()`** (maybe_improper_list() :: maybe_improper_list(any(), any()))

```Erlang
{type,174,maybe_improper_list,[]}
```

**`nonempty_list()`** (nonempty_list() :: nonempty_list(any()))

```Erlang
{type,176,nonempty_list,[]}
```

**`string()`** (string() :: [char()])

```Erlang
{type,178,string,[]}
```

**`nonempty_string()`** (nonempty_string() :: [char(),...])

```Erlang
{type,180,nonempty_string,[]}
```

**`iodata()`** (iodata() :: iolist() | binary())

```Erlang
{type,182,iodata,[]}
```

**`iolist()`** (iolist() :: maybe_improper_list(byte() | binary() | iolist(), binary() | []))

```Erlang
{type,184,iolist,[]}
```

**`function()`** (function() :: fun())

```Erlang
{type,186,function,[]}
```

**`module()`** (module() :: atom())

```Erlang
{type,188,module,[]}
```

**`mfa()`** (mfa() :: {module(),atom(),arity()})

```Erlang
{type,190,mfa,[]}
```

**`arity()`** (arity() :: 0..255)

```Erlang
{type,192,arity,[]}
```

**`identifier()`** (identifier() :: pid() | port() | reference())

```Erlang
{type,194,identifier,[]}
```

**`node()`** (node() :: atom())

```Erlang
{type,196,node,[]}
```

**`timeout()`** (timeout() :: 'infinity' | non_neg_integer())

```Erlang
{type,198,timeout,[]}
```

**`no_return()`** (no_return() :: none())

```Erlang
{type,200,no_return,[]}
```

Built-in types; can be thought defined by the syntax...

**`non_neg_integer()`** (non_neg_integer() :: 0..)

```Erlang
{type,206,non_neg_integer,[]}
```

**`pos_integer()`** (pos_integer() :: 1..)

```Erlang
{type,208,pos_integer,[]}
```

**`neg_integer()`** (neg_integer() :: ..-1)

```Erlang
{type,210,neg_integer,[]}
```

Exressions
----------


**`42`**

```Erlang
{integer,217,42}
```

**`3.141592653589`**

```Erlang
{float,218,3.141592653589}
```

**`ok`**

```Erlang
{atom,219,ok}
```

**`[]`**

```Erlang
{nil,220}
```

**`[x,y]`**

```Erlang
{cons,221,{atom,221,x},{cons,221,{atom,221,y},{nil,221}}}
```

**`[x|XS]`**

```Erlang
{cons,222,{atom,222,x},{var,222,'XS'}}
```

**`""`**

```Erlang
{string,223,[]}
```

**`"abc"`**

```Erlang
{string,224,"abc"}
```

**`<<"abc">>`**

```Erlang
{bin,225,[{bin_element,225,{string,225,"abc"},default,default}]}
```

**`<<A:8/integer,B:32/float-little,C/binary>>`**

```Erlang
{bin,226,
     [{bin_element,226,{var,226,'A'},{integer,226,8},[integer]},
      {bin_element,226,{var,226,'B'},{integer,226,32},[float,little]},
      {bin_element,226,{var,226,'C'},default,[binary]}]}
```

**`<<"abc"/utf8,XYZ/utf16>>`**

```Erlang
{bin,227,
     [{bin_element,227,{string,227,"abc"},default,[utf8]},
      {bin_element,227,{var,227,'XYZ'},default,[utf16]}]}
```

**`#{old_key := updated_value,new_key => 42}`**

```Erlang
{map,228,
     [{map_field_exact,228,{atom,228,old_key},{atom,228,updated_value}},
      {map_field_assoc,228,{atom,228,new_key},{integer,228,42}}]}
```

**`{x,y}`**

```Erlang
{tuple,229,[{atom,229,x},{atom,229,y}]}
```

**`#my_record{foo = X}`**

```Erlang
{record,230,my_record,[{record_field,230,{atom,230,foo},{var,230,'X'}}]}
```

**`Rec#my_record.foo`**

```Erlang
{record_field,231,{var,231,'Rec'},my_record,{atom,231,foo}}
```

**`fun f/1`**

```Erlang
{'fun',232,{function,f,1}}
```

**`fun m:f/1`**

```Erlang
{'fun',233,{function,{atom,233,m},{atom,233,f},{integer,233,1}}}
```

**`X`**

```Erlang
{var,236,'X'}
```

**`_`**

```Erlang
{var,237,'_'}
```

**`not true`**

```Erlang
{op,240,'not',{atom,240,true}}
```

**`1 + 1`**

```Erlang
{op,241,'+',{integer,241,1},{integer,241,1}}
```

**`1 == 1`**

```Erlang
{op,242,'==',{integer,242,1},{integer,242,1}}
```

**`X and Y`**

```Erlang
{op,243,'and',{var,243,'X'},{var,243,'Y'}}
```

**`X andalso Y`**

```Erlang
{op,244,'andalso',{var,244,'X'},{var,244,'Y'}}
```

**`f(42)`**

```Erlang
{call,247,{atom,247,f},[{integer,247,42}]}
```

**`m:f(42)`**

```Erlang
{call,248,{remote,248,{atom,248,m},{atom,248,f}},[{integer,248,42}]}
```

**`X = 42`**

```Erlang
{match,249,{var,249,'X'},{integer,249,42}}
```

**`fun(42) -> true; (_) -> false end`**

```Erlang
{'fun',250,
       {clauses,[{clause,250,[{integer,250,42}],[],[{atom,250,true}]},
                 {clause,250,[{var,250,'_'}],[],[{atom,250,false}]}]}}
```

**`fun(X) when is_atom(X) -> X end`**

```Erlang
{'fun',251,
       {clauses,[{clause,251,
                         [{var,251,'X'}],
                         [[{call,251,{atom,251,is_atom},[{var,251,'X'}]}]],
                         [{var,251,'X'}]}]}}
```

**`if P -> hello; true -> ok end`**

```Erlang
{'if',252,
      [{clause,252,[],[[{var,252,'P'}]],[{atom,252,hello}]},
       {clause,252,[],[[{atom,252,true}]],[{atom,252,ok}]}]}
```

**`case foo of bar -> baz; _ -> ok end`**

```Erlang
{'case',253,
        {atom,253,foo},
        [{clause,253,[{atom,253,bar}],[],[{atom,253,baz}]},
         {clause,253,[{var,253,'_'}],[],[{atom,253,ok}]}]}
```

**`begin ok, ok end`**

```Erlang
{block,254,[{atom,254,ok},{atom,254,ok}]}
```

**`Pid ! message`**

```Erlang
{op,255,'!',{var,255,'Pid'},{atom,255,message}}
```

**`receive X -> ok end`**

```Erlang
{'receive',256,[{clause,256,[{var,256,'X'}],[],[{atom,256,ok}]}]}
```

**`receive X -> ok after 1000 -> timeout end`**

```Erlang
{'receive',257,
           [{clause,257,[{var,257,'X'}],[],[{atom,257,ok}]}],
           {integer,257,1000},
           [{atom,257,timeout}]}
```

**`try f() catch error:E -> fail end`**

```Erlang
{'try',258,
       [{call,258,{atom,258,f},[]}],
       [],
       [{clause,258,
                [{tuple,258,[{atom,258,error},{var,258,'E'},{var,258,'_'}]}],
                [],
                [{atom,258,fail}]}],
       []}
```

**`try f(), g() of X when is_integer(X) -> X catch C:E when is_tuple(E) -> fail after afterwards() end`**

```Erlang
{'try',259,
       [{call,259,{atom,259,f},[]},{call,259,{atom,259,g},[]}],
       [{clause,259,
                [{var,259,'X'}],
                [[{call,259,{atom,259,is_integer},[{var,259,'X'}]}]],
                [{var,259,'X'}]}],
       [{clause,259,
                [{tuple,259,[{var,259,'C'},{var,259,'E'},{var,259,'_'}]}],
                [[{call,259,{atom,259,is_tuple},[{var,259,'E'}]}]],
                [{atom,259,fail}]}],
       [{call,259,{atom,259,afterwards},[]}]}
```

**`catch X`**

```Erlang
{'catch',260,{var,260,'X'}}
```

**`[ 2 || is_integer(2) ]`**

```Erlang
{lc,263,{integer,263,2},[{call,263,{atom,263,is_integer},[{integer,263,2}]}]}
```

**`[ X || X <- XS, is_atom(X) ]`**

```Erlang
{lc,264,
    {var,264,'X'},
    [{generate,264,{var,264,'X'},{var,264,'XS'}},
     {call,264,{atom,264,is_atom},[{var,264,'X'}]}]}
```

**`<< <<X:4>> || <<X:4>> <= Bin, X /= 0 >>`**

```Erlang
{bc,265,
    {bin,265,[{bin_element,265,{var,265,'X'},{integer,265,4},default}]},
    [{b_generate,265,
                 {bin,265,
                      [{bin_element,265,
                                    {var,265,'X'},
                                    {integer,265,4},
                                    default}]},
                 {var,265,'Bin'}},
     {op,265,'/=',{var,265,'X'},{integer,265,0}}]}
```

