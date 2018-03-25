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

For a complete insight, see the files corpus.erl and generate_docs.escript

Every form is a tuple. The second element is the line number in the source
file.

Types
-----

Types, mostly in the order as listed under "Types and their Syntax"
on http://erlang.org/doc/reference_manual/typespec.html.

Abstract form syntax for types: `{type, Line, TypeName, TypeParams}`.

**`any()`**

```Erlang
{type,31,any,[]}
```

**`none()`**

```Erlang
{type,32,none,[]}
```

**`pid()`**

```Erlang
{type,33,pid,[]}
```

**`port()`**

```Erlang
{type,34,port,[]}
```

**`reference()`**

```Erlang
{type,35,reference,[]}
```

**`[]`**

```Erlang
{type,36,nil,[]}
```

**`atom()`**

```Erlang
{type,37,atom,[]}
```

**`<<>>`**

```Erlang
{type,40,binary,[{integer,40,0},{integer,40,0}]}
```

**`<<_:M>>`** (M is a positive integer)

```Erlang
{type,42,binary,[{var,42,'M'},{integer,42,0}]}
```

**`<<_:_*N>>`** (N is a positive integer)

```Erlang
{type,44,binary,[{integer,44,0},{var,44,'N'}]}
```

**`<<_:M, _:_*N>>`**

```Erlang
{type,45,binary,[{var,45,'M'},{var,45,'N'}]}
```

**`float()`**

```Erlang
{type,47,float,[]}
```

**`fun()`** (any function)

```Erlang
{type,51,'fun',[]}
```

**`fun((...) -> integer())`** (any arity, returning Type)

```Erlang
{type,53,'fun',[{type,53,any},{type,53,integer,[]}]}
```

**`fun(() -> integer())`**

```Erlang
{type,54,'fun',[{type,54,product,[]},{type,54,integer,[]}]}
```

**`fun((atom(), atom()) -> integer())`**

```Erlang
{type,55,'fun',
      [{type,55,product,[{type,55,atom,[]},{type,55,atom,[]}]},
       {type,55,integer,[]}]}
```

**`integer()`**

```Erlang
{type,58,integer,[]}
```

**`42`** (..., -1, 0, 1, ... 42 ...)

```Erlang
{integer,60,42}
```

**`1..10`** (specifies an integer range)

```Erlang
{type,62,range,[{integer,62,1},{integer,62,10}]}
```

**`[integer()]`** (Proper list ([]-terminated))

```Erlang
{type,66,list,[{type,66,integer,[]}]}
```

**`maybe_improper_list(integer(), atom())`** (Type1=contents, Type2=termination)

```Erlang
{type,68,maybe_improper_list,[{type,68,integer,[]},{type,68,atom,[]}]}
```

**`nonempty_improper_list(integer(), atom())`** (Type1 and Type2 as above)

```Erlang
{type,70,nonempty_improper_list,[{type,70,integer,[]},{type,70,atom,[]}]}
```

**`[integer(), ...]`** (Proper non-empty list)

```Erlang
{type,72,nonempty_list,[{type,72,integer,[]}]}
```

**`map()`** (denotes a map of any size)

```Erlang
{type,76,map,any}
```

**`#{}`** (denotes the empty map)

```Erlang
{type,78,map,[]}
```

**`#{integer() => any()}`**

```Erlang
{type,79,map,
      [{type,80,map_field_assoc,[{type,80,integer,[]},{type,80,any,[]}]}]}
```

**`tuple()`**

```Erlang
{type,83,tuple,any}
```

**`{}`**

```Erlang
{type,84,tuple,[]}
```

**`{atom()}`**

```Erlang
{type,85,tuple,[{type,85,atom,[]}]}
```

**`{atom(), integer()}`**

```Erlang
{type,86,tuple,[{type,86,atom,[]},{type,86,integer,[]}]}
```

**`atom() | integer()`**

```Erlang
{type,89,union,[{type,89,atom,[]},{type,89,integer,[]}]}
```

For convenience, the following types are also built-in. They can be
thought as predefined aliases for the type unions shown in the code
comments.

**`term()`** (term() :: any())

```Erlang
{type,104,term,[]}
```

**`binary()`** (binary() :: <<_:_*8>>)

```Erlang
{type,106,binary,[]}
```

**`bitstring()`** (bitstring() :: <<_:_*1>>)

```Erlang
{type,108,bitstring,[]}
```

**`boolean()`** (boolean() :: 'false' | 'true')

```Erlang
{type,110,boolean,[]}
```

**`byte()`** (byte() :: 0..255)

```Erlang
{type,112,byte,[]}
```

**`char()`** (char() :: 0..16#10ffff)

```Erlang
{type,114,char,[]}
```

**`[]`** (nil() :: [])

```Erlang
{type,116,nil,[]}
```

**`number()`** (number() :: integer() | float())

```Erlang
{type,118,number,[]}
```

**`list()`** (list() :: [any()])

```Erlang
{type,120,list,[]}
```

**`maybe_improper_list()`** (maybe_improper_list() :: maybe_improper_list(any(), any()))

```Erlang
{type,122,maybe_improper_list,[]}
```

**`nonempty_list()`** (nonempty_list() :: nonempty_list(any()))

```Erlang
{type,124,nonempty_list,[]}
```

**`string()`** (string() :: [char()])

```Erlang
{type,126,string,[]}
```

**`nonempty_string()`** (nonempty_string() :: [char(),...])

```Erlang
{type,128,nonempty_string,[]}
```

**`iodata()`** (iodata() :: iolist() | binary())

```Erlang
{type,130,iodata,[]}
```

**`iolist()`** (iolist() :: maybe_improper_list(byte() | binary() | iolist(), binary() | []))

```Erlang
{type,132,iolist,[]}
```

**`function()`** (function() :: fun())

```Erlang
{type,134,function,[]}
```

**`module()`** (module() :: atom())

```Erlang
{type,136,module,[]}
```

**`mfa()`** (mfa() :: {module(),atom(),arity()})

```Erlang
{type,138,mfa,[]}
```

**`arity()`** (arity() :: 0..255)

```Erlang
{type,140,arity,[]}
```

**`identifier()`** (identifier() :: pid() | port() | reference())

```Erlang
{type,142,identifier,[]}
```

**`node()`** (node() :: atom())

```Erlang
{type,144,node,[]}
```

**`timeout()`** (timeout() :: 'infinity' | non_neg_integer())

```Erlang
{type,146,timeout,[]}
```

**`no_return()`** (no_return() :: none())

```Erlang
{type,148,no_return,[]}
```

Built-in types; can be thought defined by the syntax...

**`non_neg_integer()`** (non_neg_integer() :: 0..)

```Erlang
{type,154,non_neg_integer,[]}
```

**`pos_integer()`** (pos_integer() :: 1..)

```Erlang
{type,156,pos_integer,[]}
```

**`neg_integer()`** (neg_integer() :: ..-1)

```Erlang
{type,158,neg_integer,[]}
```

Record declarations
-------------------


**`-record(myrec,{field1 = foo,field2,field3 = foo,field4}).
`** (A record is represented as a form without types. If any fields are typed,
the form is followed by a 'record type' form. Thus, for records with typed
fields, there are two consecutive forms.)

```Erlang
{attribute,168,record,
           {myrec,[{record_field,168,{atom,168,field1},{atom,168,foo}},
                   {record_field,169,{atom,169,field2}},
                   {record_field,170,{atom,170,field3},{atom,170,foo}},
                   {record_field,171,{atom,171,field4}}]}}
```

**`-record(myrec,{field1 = foo :: atom(),
               field2 :: undefined,
               field3 = foo,
               field4}).`**

```Erlang
{attribute,168,type,
    {{record,myrec},
     [{typed_record_field,
          {record_field,168,{atom,168,field1},{atom,168,foo}},
          {type,168,atom,[]}},
      {typed_record_field,
          {record_field,169,{atom,169,field2}},
          {atom,169,undefined}},
      {record_field,170,{atom,170,field3},{atom,170,foo}},
      {record_field,171,{atom,171,field4}}],
     []}}
```

