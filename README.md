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
{attribute,-30,file,{"corpus.erl",30}}
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

**`-error(my_error).`**

```Erlang
{attribute,39,error,my_error}
```

**`-warning(my_warning).`**

```Erlang
{attribute,40,warning,my_warning}
```

**`-type my_type() :: a | b.`**

```Erlang
{attribute,41,type,{my_type,{type,41,union,[{atom,41,a},{atom,41,b}]},[]}}
```

**`-type my_opaque() :: a | b.`**

```Erlang
{attribute,42,type,{my_opaque,{type,42,union,[{atom,42,a},{atom,42,b}]},[]}}
```

**`-callback my_function() -> ok.`**

```Erlang
{attribute,43,callback,
           {{my_function,0},
            [{type,43,'fun',[{type,43,product,[]},{atom,43,ok}]}]}}
```

**`-spec my_function() -> ok.`**

```Erlang
{attribute,44,spec,
           {{my_function,0},
            [{type,44,'fun',[{type,44,product,[]},{atom,44,ok}]}]}}
```

**`my_function() -> ok.`**

```Erlang
{function,45,my_function,0,[{clause,45,[],[],[{atom,45,ok}]}]}
```

**`my_function(X) when X > 2 -> X + 1.`**

```Erlang
{function,46,my_function,1,
          [{clause,46,
                   [{var,46,'X'}],
                   [[{op,46,'>',{var,46,'X'},{integer,46,2}}]],
                   [{op,46,'+',{var,46,'X'},{integer,46,1}}]}]}
```

Record declarations: A record is represented as a form without types. If any fields are typed,
the form is followed by a 'record type' form. Thus, for records with typed
fields, there are two consecutive forms.

**`-record(myrec,{field1 = foo,field2,field3 = foo,field4}). `**

```Erlang
{attribute,53,record,
           {myrec,[{record_field,53,{atom,53,field1},{atom,53,foo}},
                   {record_field,54,{atom,54,field2}},
                   {record_field,55,{atom,55,field3},{atom,55,foo}},
                   {record_field,56,{atom,56,field4}}]}}
```

**`-record(myrec, {field1 = foo :: atom(), field2 :: undefined, field3 = foo, field4}).`**

```Erlang
{attribute,53,type,
    {{record,myrec},
     [{typed_record_field,
          {record_field,53,{atom,53,field1},{atom,53,foo}},
          {type,53,atom,[]}},
      {typed_record_field,
          {record_field,54,{atom,54,field2}},
          {atom,54,undefined}},
      {record_field,55,{atom,55,field3},{atom,55,foo}},
      {record_field,56,{atom,56,field4}}],
     []}}
```

Types
-----

Types, mostly in the order as listed under "Types and their Syntax"
on http://erlang.org/doc/reference_manual/typespec.html.

Abstract form syntax for types: `{type, Line, TypeName, TypeParams}`.

**`any()`**

```Erlang
{type,67,any,[]}
```

**`none()`**

```Erlang
{type,68,none,[]}
```

**`pid()`**

```Erlang
{type,69,pid,[]}
```

**`port()`**

```Erlang
{type,70,port,[]}
```

**`reference()`**

```Erlang
{type,71,reference,[]}
```

**`[]`**

```Erlang
{type,72,nil,[]}
```

**`atom()`**

```Erlang
{type,73,atom,[]}
```

**`<<>>`**

```Erlang
{type,76,binary,[{integer,76,0},{integer,76,0}]}
```

**`<<_:M>>`** (M is a positive integer)

```Erlang
{type,78,binary,[{var,78,'M'},{integer,78,0}]}
```

**`<<_:_*N>>`** (N is a positive integer)

```Erlang
{type,80,binary,[{integer,80,0},{var,80,'N'}]}
```

**`<<_:M, _:_*N>>`**

```Erlang
{type,81,binary,[{var,81,'M'},{var,81,'N'}]}
```

**`float()`**

```Erlang
{type,83,float,[]}
```

**`fun()`** (any function)

```Erlang
{type,87,'fun',[]}
```

**`fun((...) -> integer())`** (any arity, returning Type)

```Erlang
{type,89,'fun',[{type,89,any},{type,89,integer,[]}]}
```

**`fun(() -> integer())`**

```Erlang
{type,90,'fun',[{type,90,product,[]},{type,90,integer,[]}]}
```

**`fun((atom(), atom()) -> integer())`**

```Erlang
{type,91,'fun',
      [{type,91,product,[{type,91,atom,[]},{type,91,atom,[]}]},
       {type,91,integer,[]}]}
```

**`integer()`**

```Erlang
{type,94,integer,[]}
```

**`42`** (..., -1, 0, 1, ... 42 ...)

```Erlang
{integer,96,42}
```

**`1..10`** (specifies an integer range)

```Erlang
{type,98,range,[{integer,98,1},{integer,98,10}]}
```

**`[integer()]`** (Proper list ([]-terminated))

```Erlang
{type,102,list,[{type,102,integer,[]}]}
```

**`maybe_improper_list(integer(), atom())`** (Type1=contents, Type2=termination)

```Erlang
{type,104,maybe_improper_list,[{type,104,integer,[]},{type,104,atom,[]}]}
```

**`nonempty_improper_list(integer(), atom())`** (Type1 and Type2 as above)

```Erlang
{type,106,nonempty_improper_list,[{type,106,integer,[]},{type,106,atom,[]}]}
```

**`[integer(), ...]`** (Proper non-empty list)

```Erlang
{type,108,nonempty_list,[{type,108,integer,[]}]}
```

**`map()`** (denotes a map of any size)

```Erlang
{type,112,map,any}
```

**`#{}`** (denotes the empty map)

```Erlang
{type,114,map,[]}
```

**`#{integer() => any()}`**

```Erlang
{type,115,map,
      [{type,116,map_field_assoc,[{type,116,integer,[]},{type,116,any,[]}]}]}
```

**`tuple()`**

```Erlang
{type,119,tuple,any}
```

**`{}`**

```Erlang
{type,120,tuple,[]}
```

**`{atom()}`**

```Erlang
{type,121,tuple,[{type,121,atom,[]}]}
```

**`{atom(), integer()}`**

```Erlang
{type,122,tuple,[{type,122,atom,[]},{type,122,integer,[]}]}
```

**`atom() | integer()`**

```Erlang
{type,125,union,[{type,125,atom,[]},{type,125,integer,[]}]}
```

**`my_type()`**

```Erlang
{user_type,128,my_type,[]}
```

**`module:type()`**

```Erlang
{remote_type,129,[{atom,129,module},{atom,129,type},[]]}
```

For convenience, the following types are also built-in. They can be
thought as predefined aliases for the type unions shown in the code
comments.

**`term()`** (term() :: any())

```Erlang
{type,144,term,[]}
```

**`binary()`** (binary() :: <<_:_*8>>)

```Erlang
{type,146,binary,[]}
```

**`bitstring()`** (bitstring() :: <<_:_*1>>)

```Erlang
{type,148,bitstring,[]}
```

**`boolean()`** (boolean() :: 'false' | 'true')

```Erlang
{type,150,boolean,[]}
```

**`byte()`** (byte() :: 0..255)

```Erlang
{type,152,byte,[]}
```

**`char()`** (char() :: 0..16#10ffff)

```Erlang
{type,154,char,[]}
```

**`[]`** (nil() :: [])

```Erlang
{type,156,nil,[]}
```

**`number()`** (number() :: integer() | float())

```Erlang
{type,158,number,[]}
```

**`list()`** (list() :: [any()])

```Erlang
{type,160,list,[]}
```

**`maybe_improper_list()`** (maybe_improper_list() :: maybe_improper_list(any(), any()))

```Erlang
{type,162,maybe_improper_list,[]}
```

**`nonempty_list()`** (nonempty_list() :: nonempty_list(any()))

```Erlang
{type,164,nonempty_list,[]}
```

**`string()`** (string() :: [char()])

```Erlang
{type,166,string,[]}
```

**`nonempty_string()`** (nonempty_string() :: [char(),...])

```Erlang
{type,168,nonempty_string,[]}
```

**`iodata()`** (iodata() :: iolist() | binary())

```Erlang
{type,170,iodata,[]}
```

**`iolist()`** (iolist() :: maybe_improper_list(byte() | binary() | iolist(), binary() | []))

```Erlang
{type,172,iolist,[]}
```

**`function()`** (function() :: fun())

```Erlang
{type,174,function,[]}
```

**`module()`** (module() :: atom())

```Erlang
{type,176,module,[]}
```

**`mfa()`** (mfa() :: {module(),atom(),arity()})

```Erlang
{type,178,mfa,[]}
```

**`arity()`** (arity() :: 0..255)

```Erlang
{type,180,arity,[]}
```

**`identifier()`** (identifier() :: pid() | port() | reference())

```Erlang
{type,182,identifier,[]}
```

**`node()`** (node() :: atom())

```Erlang
{type,184,node,[]}
```

**`timeout()`** (timeout() :: 'infinity' | non_neg_integer())

```Erlang
{type,186,timeout,[]}
```

**`no_return()`** (no_return() :: none())

```Erlang
{type,188,no_return,[]}
```

Built-in types; can be thought defined by the syntax...

**`non_neg_integer()`** (non_neg_integer() :: 0..)

```Erlang
{type,194,non_neg_integer,[]}
```

**`pos_integer()`** (pos_integer() :: 1..)

```Erlang
{type,196,pos_integer,[]}
```

**`neg_integer()`** (neg_integer() :: ..-1)

```Erlang
{type,198,neg_integer,[]}
```

Exressions
----------


**`42`**

```Erlang
{integer,205,42}
```

**`3.141592653589`**

```Erlang
{float,206,3.141592653589}
```

**`ok`**

```Erlang
{atom,207,ok}
```

**`[]`**

```Erlang
{nil,208}
```

**`[x,y]`**

```Erlang
{cons,209,{atom,209,x},{cons,209,{atom,209,y},{nil,209}}}
```

**`[x|XS]`**

```Erlang
{cons,210,{atom,210,x},{var,210,'XS'}}
```

**`""`**

```Erlang
{string,211,[]}
```

**`"abc"`**

```Erlang
{string,212,"abc"}
```

**`<<"abc">>`**

```Erlang
{bin,213,[{bin_element,213,{string,213,"abc"},default,default}]}
```

**`<<A:8/integer,B:32/float-little,C/binary>>`**

```Erlang
{bin,214,
     [{bin_element,214,{var,214,'A'},{integer,214,8},[integer]},
      {bin_element,214,{var,214,'B'},{integer,214,32},[float,little]},
      {bin_element,214,{var,214,'C'},default,[binary]}]}
```

**`<<"abc"/utf8,XYZ/utf16>>`**

```Erlang
{bin,215,
     [{bin_element,215,{string,215,"abc"},default,[utf8]},
      {bin_element,215,{var,215,'XYZ'},default,[utf16]}]}
```

**`#{answer => 42}`**

```Erlang
{map,216,[{map_field_assoc,216,{atom,216,answer},{integer,216,42}}]}
```

**`{x,y}`**

```Erlang
{tuple,217,[{atom,217,x},{atom,217,y}]}
```

**`fun f/1`**

```Erlang
{'fun',218,{function,f,1}}
```

**`fun m:f/1`**

```Erlang
{'fun',219,{function,{atom,219,m},{atom,219,f},{integer,219,1}}}
```

**`X`**

```Erlang
{var,222,'X'}
```

**`_`**

```Erlang
{var,223,'_'}
```

**`not true`**

```Erlang
{op,226,'not',{atom,226,true}}
```

**`1 + 1`**

```Erlang
{op,227,'+',{integer,227,1},{integer,227,1}}
```

**`1 == 1`**

```Erlang
{op,228,'==',{integer,228,1},{integer,228,1}}
```

**`X and Y`**

```Erlang
{op,229,'and',{var,229,'X'},{var,229,'Y'}}
```

**`X andalso Y`**

```Erlang
{op,230,'andalso',{var,230,'X'},{var,230,'Y'}}
```

**`f(42)`**

```Erlang
{call,233,{atom,233,f},[{integer,233,42}]}
```

**`m:f(42)`**

```Erlang
{call,234,{remote,234,{atom,234,m},{atom,234,f}},[{integer,234,42}]}
```

**`X = 42`**

```Erlang
{match,235,{var,235,'X'},{integer,235,42}}
```

**`fun(42) -> true; (_) -> false end`**

```Erlang
{'fun',236,
       {clauses,[{clause,236,[{integer,236,42}],[],[{atom,236,true}]},
                 {clause,236,[{var,236,'_'}],[],[{atom,236,false}]}]}}
```

**`fun(X) when is_atom(X) -> X end`**

```Erlang
{'fun',237,
       {clauses,[{clause,237,
                         [{var,237,'X'}],
                         [[{call,237,{atom,237,is_atom},[{var,237,'X'}]}]],
                         [{var,237,'X'}]}]}}
```

**`if P -> hello; true -> ok end`**

```Erlang
{'if',238,
      [{clause,238,[],[[{var,238,'P'}]],[{atom,238,hello}]},
       {clause,238,[],[[{atom,238,true}]],[{atom,238,ok}]}]}
```

**`case foo of bar -> baz; _ -> ok end`**

```Erlang
{'case',239,
        {atom,239,foo},
        [{clause,239,[{atom,239,bar}],[],[{atom,239,baz}]},
         {clause,239,[{var,239,'_'}],[],[{atom,239,ok}]}]}
```

**`begin ok, ok end`**

```Erlang
{block,240,[{atom,240,ok},{atom,240,ok}]}
```

**`Pid ! message`**

```Erlang
{op,241,'!',{var,241,'Pid'},{atom,241,message}}
```

**`receive X -> ok end`**

```Erlang
{'receive',242,[{clause,242,[{var,242,'X'}],[],[{atom,242,ok}]}]}
```

**`receive X -> ok after 1000 -> timeout end`**

```Erlang
{'receive',243,
           [{clause,243,[{var,243,'X'}],[],[{atom,243,ok}]}],
           {integer,243,1000},
           [{atom,243,timeout}]}
```

**`try f() catch error:E -> fail end`**

```Erlang
{'try',244,
       [{call,244,{atom,244,f},[]}],
       [],
       [{clause,244,
                [{tuple,244,[{atom,244,error},{var,244,'E'},{var,244,'_'}]}],
                [],
                [{atom,244,fail}]}],
       []}
```

**`try f() of X when is_integer(X) -> X catch C:E when is_tuple(E) -> fail after afterwards() end`**

```Erlang
{'try',245,
       [{call,245,{atom,245,f},[]}],
       [{clause,245,
                [{var,245,'X'}],
                [[{call,245,{atom,245,is_integer},[{var,245,'X'}]}]],
                [{var,245,'X'}]}],
       [{clause,245,
                [{tuple,245,[{var,245,'C'},{var,245,'E'},{var,245,'_'}]}],
                [[{call,245,{atom,245,is_tuple},[{var,245,'E'}]}]],
                [{atom,245,fail}]}],
       [{call,245,{atom,245,afterwards},[]}]}
```

**`catch X`**

```Erlang
{'catch',246,{var,246,'X'}}
```

**`[ 2 || is_integer(2) ]`**

```Erlang
{lc,249,{integer,249,2},[{call,249,{atom,249,is_integer},[{integer,249,2}]}]}
```

**`[ X || X <- XS, is_atom(X) ]`**

```Erlang
{lc,250,
    {var,250,'X'},
    [{generate,250,{var,250,'X'},{var,250,'XS'}},
     {call,250,{atom,250,is_atom},[{var,250,'X'}]}]}
```

**`<< <<X:4>> || <<X:4>> <= Bin, X /= 0 >>`**

```Erlang
{bc,251,
    {bin,251,[{bin_element,251,{var,251,'X'},{integer,251,4},default}]},
    [{b_generate,251,
                 {bin,251,
                      [{bin_element,251,
                                    {var,251,'X'},
                                    {integer,251,4},
                                    default}]},
                 {var,251,'Bin'}},
     {op,251,'/=',{var,251,'X'},{integer,251,0}}]}
```

