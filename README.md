
Erlang Abstract Format
======================

The Erlang Abstract Format is used in parse trees in various tools, such as the compiler and preprocessor modules shipped with Erlang/OTP. A brief documentation is available at http://erlang.org/doc/apps/erts/absform.html. However, the complete syntax of abstract format is not documented.

This is an attempt to document every langugage construct by examples. The examples are generated using Erlang source code, which is parsed to generate the abstract form, and pretty-printed again to generate the Erlang code examples.

For a complete insight, see the files corpus.erl and generate_docs.escript

Every form is a tuple. The second element is the line number in the source file.

Types
-----

Types, mostly in the order as listed under "Types and their Syntax" on http://erlang.org/doc/reference_manual/typespec.html.

Abstract form syntax for types: `{type, Line, TypeName, TypeParams}`.

## `any()`

```Erlang
{type,28,any,[]}
```

## `none()`

```Erlang
{type,29,none,[]}
```

## `pid()`

```Erlang
{type,30,pid,[]}
```

## `port()`

```Erlang
{type,31,port,[]}
```

## `reference()`

```Erlang
{type,32,reference,[]}
```

## `[]`

```Erlang
{type,33,nil,[]}
```

## `atom()`

```Erlang
{type,34,atom,[]}
```

## `<<>>`

```Erlang
{type,37,binary,[{integer,37,0},{integer,37,0}]}
```

## `<<_:M>>`

```Erlang
{type,38,binary,[{var,38,'M'},{integer,38,0}]}
```

## `<<_:_*N>>`

```Erlang
{type,39,binary,[{integer,39,0},{var,39,'N'}]}
```

## `<<_:M, _:_*N>>`

```Erlang
{type,40,binary,[{var,40,'M'},{var,40,'N'}]}
```

## `float()`

```Erlang
{type,42,float,[]}
```

## `fun()`

```Erlang
{type,45,'fun',[]}
```

## `fun((...) -> integer())`

```Erlang
{type,46,'fun',[{type,46,any},{type,46,integer,[]}]}
```

## `fun(() -> integer())`

```Erlang
{type,47,'fun',[{type,47,product,[]},{type,47,integer,[]}]}
```

## `fun((atom(), atom()) -> integer())`

```Erlang
{type,48,'fun',
      [{type,48,product,[{type,48,atom,[]},{type,48,atom,[]}]},
       {type,48,integer,[]}]}
```

## `integer()`

```Erlang
{type,51,integer,[]}
```

## `42`

```Erlang
{integer,52,42}
```

## `1..10`

```Erlang
{type,53,range,[{integer,53,1},{integer,53,10}]}
```

## `[integer()]`

```Erlang
{type,56,list,[{type,56,integer,[]}]}
```

## `maybe_improper_list(integer(), atom())`

```Erlang
{type,57,maybe_improper_list,[{type,57,integer,[]},{type,57,atom,[]}]}
```

## `nonempty_improper_list(integer(), atom())`

```Erlang
{type,58,nonempty_improper_list,[{type,58,integer,[]},{type,58,atom,[]}]}
```

## `[integer(), ...]`

```Erlang
{type,59,nonempty_list,[{type,59,integer,[]}]}
```

## `map()`

```Erlang
{type,62,map,any}
```

## `#{}`

```Erlang
{type,63,map,[]}
```

## `#{integer() => any()}`

```Erlang
{type,64,map,
      [{type,65,map_field_assoc,[{type,65,integer,[]},{type,65,any,[]}]}]}
```

## `tuple()`

```Erlang
{type,68,tuple,any}
```

## `{}`

```Erlang
{type,69,tuple,[]}
```

## `{atom()}`

```Erlang
{type,70,tuple,[{type,70,atom,[]}]}
```

## `{atom(), integer()}`

```Erlang
{type,71,tuple,[{type,71,atom,[]},{type,71,integer,[]}]}
```

## `atom() | integer()`

```Erlang
{type,74,union,[{type,74,atom,[]},{type,74,integer,[]}]}
```

For convenience, the following types are also built-in. They can be thought as predefined aliases for the type unions shown in the code comments.

## `term()`

```Erlang
{type,88,term,[]} %% term() :: any()
```

## `binary()`

```Erlang
{type,90,binary,[]} %% binary() :: <<_:_*8>>
```

## `bitstring()`

```Erlang
{type,92,bitstring,[]} %% bitstring() :: <<_:_*1>>
```

## `boolean()`

```Erlang
{type,94,boolean,[]} %% boolean() :: 'false' | 'true'
```

## `byte()`

```Erlang
{type,96,byte,[]} %% byte() :: 0..255
```

## `char()`

```Erlang
{type,98,char,[]} %% char() :: 0..16#10ffff
```

## `[]`

```Erlang
{type,100,nil,[]} %% nil() :: []
```

## `number()`

```Erlang
{type,102,number,[]} %% number() :: integer() | float()
```

## `list()`

```Erlang
{type,104,list,[]} %% list() :: [any()]
```

## `maybe_improper_list()`

```Erlang
{type,106,maybe_improper_list,[]} %% maybe_improper_list() :: maybe_improper_list(any(), any())
```

## `nonempty_list()`

```Erlang
{type,108,nonempty_list,[]} %% nonempty_list() :: nonempty_list(any())
```

## `string()`

```Erlang
{type,110,string,[]} %% string() :: [char()]
```

## `nonempty_string()`

```Erlang
{type,112,nonempty_string,[]} %% nonempty_string() :: [char(),...]
```

## `iodata()`

```Erlang
{type,114,iodata,[]} %% iodata() :: iolist() | binary()
```

## `iolist()`

```Erlang
{type,116,iolist,[]} %% iolist() :: maybe_improper_list(byte() | binary() | iolist(), binary() | [])
```

## `function()`

```Erlang
{type,118,function,[]} %% function() :: fun()
```

## `module()`

```Erlang
{type,120,module,[]} %% module() :: atom()
```

## `mfa()`

```Erlang
{type,122,mfa,[]} %% mfa() :: {module(),atom(),arity()}
```

## `arity()`

```Erlang
{type,124,arity,[]} %% arity() :: 0..255
```

## `identifier()`

```Erlang
{type,126,identifier,[]} %% identifier() :: pid() | port() | reference()
```

## `node()`

```Erlang
{type,128,node,[]} %% node() :: atom()
```

## `timeout()`

```Erlang
{type,130,timeout,[]} %% timeout() :: 'infinity' | non_neg_integer()
```

## `no_return()`

```Erlang
{type,132,no_return,[]} %% no_return() :: none()
```

Built-in types; can be thought defined by the syntax...

## `non_neg_integer()`

```Erlang
{type,138,non_neg_integer,[]} %% non_neg_integer() :: 0..
```

## `pos_integer()`

```Erlang
{type,140,pos_integer,[]} %% pos_integer() :: 1..
```

## `neg_integer()`

```Erlang
{type,142,neg_integer,[]} %% neg_integer() :: ..-1
```

Record declarations
-------------------

A record is represented as a form without types. If any fields are typed, the form is followed by a 'record type' form. Thus, for records with typed fields, there are two consecutive forms.

## `-record(myrec,{field1 = foo,field2,field3 = foo,field4}).
`

```Erlang
{attribute,150,record,
           {myrec,[{record_field,150,{atom,150,field1},{atom,150,foo}},
                   {record_field,151,{atom,151,field2}},
                   {record_field,152,{atom,152,field3},{atom,152,foo}},
                   {record_field,153,{atom,153,field4}}]}}
```

## `-record(myrec,{field1 = foo :: atom(),
               field2 :: undefined,
               field3 = foo,
               field4}).`

```Erlang
{attribute,150,type,
    {{record,myrec},
     [{typed_record_field,
          {record_field,150,{atom,150,field1},{atom,150,foo}},
          {type,150,atom,[]}},
      {typed_record_field,
          {record_field,151,{atom,151,field2}},
          {atom,151,undefined}},
      {record_field,152,{atom,152,field3},{atom,152,foo}},
      {record_field,153,{atom,153,field4}}],
     []}}
```

