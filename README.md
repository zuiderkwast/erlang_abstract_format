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

**`-opaque my_opaque() :: a | b.`**

```Erlang
{attribute,42,opaque,{my_opaque,{type,42,union,[{atom,42,a},{atom,42,b}]},[]}}
```

**`-type maybe(A) :: {just, A} | nothing.`**

```Erlang
{attribute,43,type,
           {maybe,{type,43,union,
                        [{type,43,tuple,[{atom,43,just},{var,43,'A'}]},
                         {atom,43,nothing}]},
                  [{var,43,'A'}]}}
```

**`-callback my_function() -> ok.`**

```Erlang
{attribute,44,callback,
           {{my_function,0},
            [{type,44,'fun',[{type,44,product,[]},{atom,44,ok}]}]}}
```

**`-spec my_function() -> ok.`**

```Erlang
{attribute,45,spec,
           {{my_function,0},
            [{type,45,'fun',[{type,45,product,[]},{atom,45,ok}]}]}}
```

**`my_function() -> ok.`**

```Erlang
{function,46,my_function,0,[{clause,46,[],[],[{atom,46,ok}]}]}
```

**`my_function(X) when X > 2 -> X + 1.`**

```Erlang
{function,47,my_function,1,
          [{clause,47,
                   [{var,47,'X'}],
                   [[{op,47,'>',{var,47,'X'},{integer,47,2}}]],
                   [{op,47,'+',{var,47,'X'},{integer,47,1}}]}]}
```

Record declarations: A record is represented as a form without types. If any fields are typed,
the form is followed by a 'record type' form. Thus, for records with typed
fields, there are two consecutive forms.

**`-record(myrec,{field1 = foo,field2,field3 = foo,field4}). `**

```Erlang
{attribute,54,record,
           {myrec,[{record_field,54,{atom,54,field1},{atom,54,foo}},
                   {record_field,55,{atom,55,field2}},
                   {record_field,56,{atom,56,field3},{atom,56,foo}},
                   {record_field,57,{atom,57,field4}}]}}
```

**`-record(myrec, {field1 = foo :: atom(), field2 :: undefined, field3 = foo, field4}).`**

```Erlang
{attribute,54,type,
    {{record,myrec},
     [{typed_record_field,
          {record_field,54,{atom,54,field1},{atom,54,foo}},
          {type,54,atom,[]}},
      {typed_record_field,
          {record_field,55,{atom,55,field2}},
          {atom,55,undefined}},
      {record_field,56,{atom,56,field3},{atom,56,foo}},
      {record_field,57,{atom,57,field4}}],
     []}}
```

Types
-----

Types, mostly in the order as listed under "Types and their Syntax"
on http://erlang.org/doc/reference_manual/typespec.html.

Abstract form syntax for types: `{type, Line, TypeName, TypeParams}`.

**`any()`**

```Erlang
{type,68,any,[]}
```

**`none()`**

```Erlang
{type,69,none,[]}
```

**`pid()`**

```Erlang
{type,70,pid,[]}
```

**`port()`**

```Erlang
{type,71,port,[]}
```

**`reference()`**

```Erlang
{type,72,reference,[]}
```

**`[]`**

```Erlang
{type,73,nil,[]}
```

**`atom()`**

```Erlang
{type,74,atom,[]}
```

**`<<>>`**

```Erlang
{type,77,binary,[{integer,77,0},{integer,77,0}]}
```

**`<<_:M>>`** (M is a positive integer)

```Erlang
{type,79,binary,[{var,79,'M'},{integer,79,0}]}
```

**`<<_:_*N>>`** (N is a positive integer)

```Erlang
{type,81,binary,[{integer,81,0},{var,81,'N'}]}
```

**`<<_:M, _:_*N>>`**

```Erlang
{type,82,binary,[{var,82,'M'},{var,82,'N'}]}
```

**`float()`**

```Erlang
{type,84,float,[]}
```

**`fun()`** (any function)

```Erlang
{type,88,'fun',[]}
```

**`fun((...) -> integer())`** (any arity, returning Type)

```Erlang
{type,90,'fun',[{type,90,any},{type,90,integer,[]}]}
```

**`fun(() -> integer())`**

```Erlang
{type,91,'fun',[{type,91,product,[]},{type,91,integer,[]}]}
```

**`fun((atom(), atom()) -> integer())`**

```Erlang
{type,92,'fun',
      [{type,92,product,[{type,92,atom,[]},{type,92,atom,[]}]},
       {type,92,integer,[]}]}
```

**`integer()`**

```Erlang
{type,95,integer,[]}
```

**`42`** (..., -1, 0, 1, ... 42 ...)

```Erlang
{integer,97,42}
```

**`1..10`** (specifies an integer range)

```Erlang
{type,99,range,[{integer,99,1},{integer,99,10}]}
```

**`[integer()]`** (Proper list ([]-terminated))

```Erlang
{type,103,list,[{type,103,integer,[]}]}
```

**`maybe_improper_list(integer(), atom())`** (Type1=contents, Type2=termination)

```Erlang
{type,105,maybe_improper_list,[{type,105,integer,[]},{type,105,atom,[]}]}
```

**`nonempty_improper_list(integer(), atom())`** (Type1 and Type2 as above)

```Erlang
{type,107,nonempty_improper_list,[{type,107,integer,[]},{type,107,atom,[]}]}
```

**`[integer(), ...]`** (Proper non-empty list)

```Erlang
{type,109,nonempty_list,[{type,109,integer,[]}]}
```

**`map()`** (denotes a map of any size)

```Erlang
{type,113,map,any}
```

**`#{}`** (denotes the empty map)

```Erlang
{type,115,map,[]}
```

**`#{integer() => any()}`**

```Erlang
{type,116,map,
      [{type,117,map_field_assoc,[{type,117,integer,[]},{type,117,any,[]}]}]}
```

**`tuple()`**

```Erlang
{type,120,tuple,any}
```

**`{}`**

```Erlang
{type,121,tuple,[]}
```

**`{atom()}`**

```Erlang
{type,122,tuple,[{type,122,atom,[]}]}
```

**`{atom(), integer()}`**

```Erlang
{type,123,tuple,[{type,123,atom,[]},{type,123,integer,[]}]}
```

**`#my_record{}`**

```Erlang
{type,126,record,[{atom,126,my_record}]}
```

**`atom() | integer()`**

```Erlang
{type,129,union,[{type,129,atom,[]},{type,129,integer,[]}]}
```

**`my_type()`**

```Erlang
{user_type,132,my_type,[]}
```

**`maybe(integer())`**

```Erlang
{user_type,133,maybe,[{type,133,integer,[]}]}
```

**`module:type()`**

```Erlang
{remote_type,134,[{atom,134,module},{atom,134,type},[]]}
```

For convenience, the following types are also built-in. They can be
thought as predefined aliases for the type unions shown in the code
comments.

**`term()`** (term() :: any())

```Erlang
{type,149,term,[]}
```

**`binary()`** (binary() :: <<_:_*8>>)

```Erlang
{type,151,binary,[]}
```

**`bitstring()`** (bitstring() :: <<_:_*1>>)

```Erlang
{type,153,bitstring,[]}
```

**`boolean()`** (boolean() :: 'false' | 'true')

```Erlang
{type,155,boolean,[]}
```

**`byte()`** (byte() :: 0..255)

```Erlang
{type,157,byte,[]}
```

**`char()`** (char() :: 0..16#10ffff)

```Erlang
{type,159,char,[]}
```

**`[]`** (nil() :: [])

```Erlang
{type,161,nil,[]}
```

**`number()`** (number() :: integer() | float())

```Erlang
{type,163,number,[]}
```

**`list()`** (list() :: [any()])

```Erlang
{type,165,list,[]}
```

**`maybe_improper_list()`** (maybe_improper_list() :: maybe_improper_list(any(), any()))

```Erlang
{type,167,maybe_improper_list,[]}
```

**`nonempty_list()`** (nonempty_list() :: nonempty_list(any()))

```Erlang
{type,169,nonempty_list,[]}
```

**`string()`** (string() :: [char()])

```Erlang
{type,171,string,[]}
```

**`nonempty_string()`** (nonempty_string() :: [char(),...])

```Erlang
{type,173,nonempty_string,[]}
```

**`iodata()`** (iodata() :: iolist() | binary())

```Erlang
{type,175,iodata,[]}
```

**`iolist()`** (iolist() :: maybe_improper_list(byte() | binary() | iolist(), binary() | []))

```Erlang
{type,177,iolist,[]}
```

**`function()`** (function() :: fun())

```Erlang
{type,179,function,[]}
```

**`module()`** (module() :: atom())

```Erlang
{type,181,module,[]}
```

**`mfa()`** (mfa() :: {module(),atom(),arity()})

```Erlang
{type,183,mfa,[]}
```

**`arity()`** (arity() :: 0..255)

```Erlang
{type,185,arity,[]}
```

**`identifier()`** (identifier() :: pid() | port() | reference())

```Erlang
{type,187,identifier,[]}
```

**`node()`** (node() :: atom())

```Erlang
{type,189,node,[]}
```

**`timeout()`** (timeout() :: 'infinity' | non_neg_integer())

```Erlang
{type,191,timeout,[]}
```

**`no_return()`** (no_return() :: none())

```Erlang
{type,193,no_return,[]}
```

Built-in types; can be thought defined by the syntax...

**`non_neg_integer()`** (non_neg_integer() :: 0..)

```Erlang
{type,199,non_neg_integer,[]}
```

**`pos_integer()`** (pos_integer() :: 1..)

```Erlang
{type,201,pos_integer,[]}
```

**`neg_integer()`** (neg_integer() :: ..-1)

```Erlang
{type,203,neg_integer,[]}
```

Exressions
----------


**`42`**

```Erlang
{integer,210,42}
```

**`3.141592653589`**

```Erlang
{float,211,3.141592653589}
```

**`ok`**

```Erlang
{atom,212,ok}
```

**`[]`**

```Erlang
{nil,213}
```

**`[x,y]`**

```Erlang
{cons,214,{atom,214,x},{cons,214,{atom,214,y},{nil,214}}}
```

**`[x|XS]`**

```Erlang
{cons,215,{atom,215,x},{var,215,'XS'}}
```

**`""`**

```Erlang
{string,216,[]}
```

**`"abc"`**

```Erlang
{string,217,"abc"}
```

**`<<"abc">>`**

```Erlang
{bin,218,[{bin_element,218,{string,218,"abc"},default,default}]}
```

**`<<A:8/integer,B:32/float-little,C/binary>>`**

```Erlang
{bin,219,
     [{bin_element,219,{var,219,'A'},{integer,219,8},[integer]},
      {bin_element,219,{var,219,'B'},{integer,219,32},[float,little]},
      {bin_element,219,{var,219,'C'},default,[binary]}]}
```

**`<<"abc"/utf8,XYZ/utf16>>`**

```Erlang
{bin,220,
     [{bin_element,220,{string,220,"abc"},default,[utf8]},
      {bin_element,220,{var,220,'XYZ'},default,[utf16]}]}
```

**`#{answer => 42}`**

```Erlang
{map,221,[{map_field_assoc,221,{atom,221,answer},{integer,221,42}}]}
```

**`{x,y}`**

```Erlang
{tuple,222,[{atom,222,x},{atom,222,y}]}
```

**`#my_record{foo = X}`**

```Erlang
{record,223,my_record,[{record_field,223,{atom,223,foo},{var,223,'X'}}]}
```

**`Rec#my_record.foo`**

```Erlang
{record_field,224,{var,224,'Rec'},my_record,{atom,224,foo}}
```

**`fun f/1`**

```Erlang
{'fun',225,{function,f,1}}
```

**`fun m:f/1`**

```Erlang
{'fun',226,{function,{atom,226,m},{atom,226,f},{integer,226,1}}}
```

**`X`**

```Erlang
{var,229,'X'}
```

**`_`**

```Erlang
{var,230,'_'}
```

**`not true`**

```Erlang
{op,233,'not',{atom,233,true}}
```

**`1 + 1`**

```Erlang
{op,234,'+',{integer,234,1},{integer,234,1}}
```

**`1 == 1`**

```Erlang
{op,235,'==',{integer,235,1},{integer,235,1}}
```

**`X and Y`**

```Erlang
{op,236,'and',{var,236,'X'},{var,236,'Y'}}
```

**`X andalso Y`**

```Erlang
{op,237,'andalso',{var,237,'X'},{var,237,'Y'}}
```

**`f(42)`**

```Erlang
{call,240,{atom,240,f},[{integer,240,42}]}
```

**`m:f(42)`**

```Erlang
{call,241,{remote,241,{atom,241,m},{atom,241,f}},[{integer,241,42}]}
```

**`X = 42`**

```Erlang
{match,242,{var,242,'X'},{integer,242,42}}
```

**`fun(42) -> true; (_) -> false end`**

```Erlang
{'fun',243,
       {clauses,[{clause,243,[{integer,243,42}],[],[{atom,243,true}]},
                 {clause,243,[{var,243,'_'}],[],[{atom,243,false}]}]}}
```

**`fun(X) when is_atom(X) -> X end`**

```Erlang
{'fun',244,
       {clauses,[{clause,244,
                         [{var,244,'X'}],
                         [[{call,244,{atom,244,is_atom},[{var,244,'X'}]}]],
                         [{var,244,'X'}]}]}}
```

**`if P -> hello; true -> ok end`**

```Erlang
{'if',245,
      [{clause,245,[],[[{var,245,'P'}]],[{atom,245,hello}]},
       {clause,245,[],[[{atom,245,true}]],[{atom,245,ok}]}]}
```

**`case foo of bar -> baz; _ -> ok end`**

```Erlang
{'case',246,
        {atom,246,foo},
        [{clause,246,[{atom,246,bar}],[],[{atom,246,baz}]},
         {clause,246,[{var,246,'_'}],[],[{atom,246,ok}]}]}
```

**`begin ok, ok end`**

```Erlang
{block,247,[{atom,247,ok},{atom,247,ok}]}
```

**`Pid ! message`**

```Erlang
{op,248,'!',{var,248,'Pid'},{atom,248,message}}
```

**`receive X -> ok end`**

```Erlang
{'receive',249,[{clause,249,[{var,249,'X'}],[],[{atom,249,ok}]}]}
```

**`receive X -> ok after 1000 -> timeout end`**

```Erlang
{'receive',250,
           [{clause,250,[{var,250,'X'}],[],[{atom,250,ok}]}],
           {integer,250,1000},
           [{atom,250,timeout}]}
```

**`try f() catch error:E -> fail end`**

```Erlang
{'try',251,
       [{call,251,{atom,251,f},[]}],
       [],
       [{clause,251,
                [{tuple,251,[{atom,251,error},{var,251,'E'},{var,251,'_'}]}],
                [],
                [{atom,251,fail}]}],
       []}
```

**`try f() of X when is_integer(X) -> X catch C:E when is_tuple(E) -> fail after afterwards() end`**

```Erlang
{'try',252,
       [{call,252,{atom,252,f},[]}],
       [{clause,252,
                [{var,252,'X'}],
                [[{call,252,{atom,252,is_integer},[{var,252,'X'}]}]],
                [{var,252,'X'}]}],
       [{clause,252,
                [{tuple,252,[{var,252,'C'},{var,252,'E'},{var,252,'_'}]}],
                [[{call,252,{atom,252,is_tuple},[{var,252,'E'}]}]],
                [{atom,252,fail}]}],
       [{call,252,{atom,252,afterwards},[]}]}
```

**`catch X`**

```Erlang
{'catch',253,{var,253,'X'}}
```

**`[ 2 || is_integer(2) ]`**

```Erlang
{lc,256,{integer,256,2},[{call,256,{atom,256,is_integer},[{integer,256,2}]}]}
```

**`[ X || X <- XS, is_atom(X) ]`**

```Erlang
{lc,257,
    {var,257,'X'},
    [{generate,257,{var,257,'X'},{var,257,'XS'}},
     {call,257,{atom,257,is_atom},[{var,257,'X'}]}]}
```

**`<< <<X:4>> || <<X:4>> <= Bin, X /= 0 >>`**

```Erlang
{bc,258,
    {bin,258,[{bin_element,258,{var,258,'X'},{integer,258,4},default}]},
    [{b_generate,258,
                 {bin,258,
                      [{bin_element,258,
                                    {var,258,'X'},
                                    {integer,258,4},
                                    default}]},
                 {var,258,'Bin'}},
     {op,258,'/=',{var,258,'X'},{integer,258,0}}]}
```

