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

<table>
<tr><td>
```Erlang
any()
```
</td><td>
```Erlang
{type,31,any,[]}
```
</td><td></td></tr>
<tr><td>
```Erlang
none()
```
</td><td>
```Erlang
{type,32,none,[]}
```
</td><td></td></tr>
<tr><td>
```Erlang
pid()
```
</td><td>
```Erlang
{type,33,pid,[]}
```
</td><td></td></tr>
<tr><td>
```Erlang
port()
```
</td><td>
```Erlang
{type,34,port,[]}
```
</td><td></td></tr>
<tr><td>
```Erlang
reference()
```
</td><td>
```Erlang
{type,35,reference,[]}
```
</td><td></td></tr>
<tr><td>
```Erlang
[]
```
</td><td>
```Erlang
{type,36,nil,[]}
```
</td><td></td></tr>
<tr><td>
```Erlang
atom()
```
</td><td>
```Erlang
{type,37,atom,[]}
```
</td><td></td></tr>
<tr><td>
```Erlang
<<>>
```
</td><td>
```Erlang
{type,40,binary,[{integer,40,0},{integer,40,0}]}
```
</td><td></td></tr>
<tr><td>
```Erlang
<<_:M>>
```
</td><td>
```Erlang
{type,42,binary,[{var,42,'M'},{integer,42,0}]}
```
</td><td>M is a positive integer</td></tr>
<tr><td>
```Erlang
<<_:_*N>>
```
</td><td>
```Erlang
{type,44,binary,[{integer,44,0},{var,44,'N'}]}
```
</td><td>N is a positive integer</td></tr>
<tr><td>
```Erlang
<<_:M, _:_*N>>
```
</td><td>
```Erlang
{type,45,binary,[{var,45,'M'},{var,45,'N'}]}
```
</td><td></td></tr>
<tr><td>
```Erlang
float()
```
</td><td>
```Erlang
{type,47,float,[]}
```
</td><td></td></tr>
<tr><td>
```Erlang
fun()
```
</td><td>
```Erlang
{type,51,'fun',[]}
```
</td><td>any function</td></tr>
<tr><td>
```Erlang
fun((...) -> integer())
```
</td><td>
```Erlang
{type,53,'fun',[{type,53,any},{type,53,integer,[]}]}
```
</td><td>any arity, returning Type</td></tr>
<tr><td>
```Erlang
fun(() -> integer())
```
</td><td>
```Erlang
{type,54,'fun',[{type,54,product,[]},{type,54,integer,[]}]}
```
</td><td></td></tr>
<tr><td>
```Erlang
fun((atom(), atom()) -> integer())
```
</td><td>
```Erlang
{type,55,'fun',
      [{type,55,product,[{type,55,atom,[]},{type,55,atom,[]}]},
       {type,55,integer,[]}]}
```
</td><td></td></tr>
<tr><td>
```Erlang
integer()
```
</td><td>
```Erlang
{type,58,integer,[]}
```
</td><td></td></tr>
<tr><td>
```Erlang
42
```
</td><td>
```Erlang
{integer,60,42}
```
</td><td>..., -1, 0, 1, ... 42 ...</td></tr>
<tr><td>
```Erlang
1..10
```
</td><td>
```Erlang
{type,62,range,[{integer,62,1},{integer,62,10}]}
```
</td><td>specifies an integer range</td></tr>
<tr><td>
```Erlang
[integer()]
```
</td><td>
```Erlang
{type,66,list,[{type,66,integer,[]}]}
```
</td><td>Proper list ([]-terminated)</td></tr>
<tr><td>
```Erlang
maybe_improper_list(integer(), atom())
```
</td><td>
```Erlang
{type,68,maybe_improper_list,[{type,68,integer,[]},{type,68,atom,[]}]}
```
</td><td>Type1=contents, Type2=termination</td></tr>
<tr><td>
```Erlang
nonempty_improper_list(integer(), atom())
```
</td><td>
```Erlang
{type,70,nonempty_improper_list,[{type,70,integer,[]},{type,70,atom,[]}]}
```
</td><td>Type1 and Type2 as above</td></tr>
<tr><td>
```Erlang
[integer(), ...]
```
</td><td>
```Erlang
{type,72,nonempty_list,[{type,72,integer,[]}]}
```
</td><td>Proper non-empty list</td></tr>
<tr><td>
```Erlang
map()
```
</td><td>
```Erlang
{type,76,map,any}
```
</td><td>denotes a map of any size</td></tr>
<tr><td>
```Erlang
#{}
```
</td><td>
```Erlang
{type,78,map,[]}
```
</td><td>denotes the empty map</td></tr>
<tr><td>
```Erlang
#{integer() => any()}
```
</td><td>
```Erlang
{type,79,map,
      [{type,80,map_field_assoc,[{type,80,integer,[]},{type,80,any,[]}]}]}
```
</td><td></td></tr>
<tr><td>
```Erlang
tuple()
```
</td><td>
```Erlang
{type,83,tuple,any}
```
</td><td></td></tr>
<tr><td>
```Erlang
{}
```
</td><td>
```Erlang
{type,84,tuple,[]}
```
</td><td></td></tr>
<tr><td>
```Erlang
{atom()}
```
</td><td>
```Erlang
{type,85,tuple,[{type,85,atom,[]}]}
```
</td><td></td></tr>
<tr><td>
```Erlang
{atom(), integer()}
```
</td><td>
```Erlang
{type,86,tuple,[{type,86,atom,[]},{type,86,integer,[]}]}
```
</td><td></td></tr>
<tr><td>
```Erlang
atom() | integer()
```
</td><td>
```Erlang
{type,89,union,[{type,89,atom,[]},{type,89,integer,[]}]}
```
</td><td></td></tr>
</table>

For convenience, the following types are also built-in. They can be
thought as predefined aliases for the type unions shown in the code
comments.

<table>
<tr><td>
```Erlang
term()
```
</td><td>
```Erlang
{type,104,term,[]}
```
</td><td>term() :: any()</td></tr>
<tr><td>
```Erlang
binary()
```
</td><td>
```Erlang
{type,106,binary,[]}
```
</td><td>binary() :: <<_:_*8>></td></tr>
<tr><td>
```Erlang
bitstring()
```
</td><td>
```Erlang
{type,108,bitstring,[]}
```
</td><td>bitstring() :: <<_:_*1>></td></tr>
<tr><td>
```Erlang
boolean()
```
</td><td>
```Erlang
{type,110,boolean,[]}
```
</td><td>boolean() :: 'false' | 'true'</td></tr>
<tr><td>
```Erlang
byte()
```
</td><td>
```Erlang
{type,112,byte,[]}
```
</td><td>byte() :: 0..255</td></tr>
<tr><td>
```Erlang
char()
```
</td><td>
```Erlang
{type,114,char,[]}
```
</td><td>char() :: 0..16#10ffff</td></tr>
<tr><td>
```Erlang
[]
```
</td><td>
```Erlang
{type,116,nil,[]}
```
</td><td>nil() :: []</td></tr>
<tr><td>
```Erlang
number()
```
</td><td>
```Erlang
{type,118,number,[]}
```
</td><td>number() :: integer() | float()</td></tr>
<tr><td>
```Erlang
list()
```
</td><td>
```Erlang
{type,120,list,[]}
```
</td><td>list() :: [any()]</td></tr>
<tr><td>
```Erlang
maybe_improper_list()
```
</td><td>
```Erlang
{type,122,maybe_improper_list,[]}
```
</td><td>maybe_improper_list() :: maybe_improper_list(any(), any())</td></tr>
<tr><td>
```Erlang
nonempty_list()
```
</td><td>
```Erlang
{type,124,nonempty_list,[]}
```
</td><td>nonempty_list() :: nonempty_list(any())</td></tr>
<tr><td>
```Erlang
string()
```
</td><td>
```Erlang
{type,126,string,[]}
```
</td><td>string() :: [char()]</td></tr>
<tr><td>
```Erlang
nonempty_string()
```
</td><td>
```Erlang
{type,128,nonempty_string,[]}
```
</td><td>nonempty_string() :: [char(),...]</td></tr>
<tr><td>
```Erlang
iodata()
```
</td><td>
```Erlang
{type,130,iodata,[]}
```
</td><td>iodata() :: iolist() | binary()</td></tr>
<tr><td>
```Erlang
iolist()
```
</td><td>
```Erlang
{type,132,iolist,[]}
```
</td><td>iolist() :: maybe_improper_list(byte() | binary() | iolist(), binary() | [])</td></tr>
<tr><td>
```Erlang
function()
```
</td><td>
```Erlang
{type,134,function,[]}
```
</td><td>function() :: fun()</td></tr>
<tr><td>
```Erlang
module()
```
</td><td>
```Erlang
{type,136,module,[]}
```
</td><td>module() :: atom()</td></tr>
<tr><td>
```Erlang
mfa()
```
</td><td>
```Erlang
{type,138,mfa,[]}
```
</td><td>mfa() :: {module(),atom(),arity()}</td></tr>
<tr><td>
```Erlang
arity()
```
</td><td>
```Erlang
{type,140,arity,[]}
```
</td><td>arity() :: 0..255</td></tr>
<tr><td>
```Erlang
identifier()
```
</td><td>
```Erlang
{type,142,identifier,[]}
```
</td><td>identifier() :: pid() | port() | reference()</td></tr>
<tr><td>
```Erlang
node()
```
</td><td>
```Erlang
{type,144,node,[]}
```
</td><td>node() :: atom()</td></tr>
<tr><td>
```Erlang
timeout()
```
</td><td>
```Erlang
{type,146,timeout,[]}
```
</td><td>timeout() :: 'infinity' | non_neg_integer()</td></tr>
<tr><td>
```Erlang
no_return()
```
</td><td>
```Erlang
{type,148,no_return,[]}
```
</td><td>no_return() :: none()</td></tr>
</table>

Built-in types; can be thought defined by the syntax...

<table>
<tr><td>
```Erlang
non_neg_integer()
```
</td><td>
```Erlang
{type,154,non_neg_integer,[]}
```
</td><td>non_neg_integer() :: 0..</td></tr>
<tr><td>
```Erlang
pos_integer()
```
</td><td>
```Erlang
{type,156,pos_integer,[]}
```
</td><td>pos_integer() :: 1..</td></tr>
<tr><td>
```Erlang
neg_integer()
```
</td><td>
```Erlang
{type,158,neg_integer,[]}
```
</td><td>neg_integer() :: ..-1</td></tr>
</table>

Record declarations
-------------------


<table>
<tr><td>
```Erlang
-record(myrec,{field1 = foo,field2,field3 = foo,field4}).

```
</td><td>
```Erlang
{attribute,168,record,
           {myrec,[{record_field,168,{atom,168,field1},{atom,168,foo}},
                   {record_field,169,{atom,169,field2}},
                   {record_field,170,{atom,170,field3},{atom,170,foo}},
                   {record_field,171,{atom,171,field4}}]}}
```
</td><td>A record is represented as a form without types. If any fields are typed,
the form is followed by a 'record type' form. Thus, for records with typed
fields, there are two consecutive forms.</td></tr>
<tr><td>
```Erlang
-record(myrec,{field1 = foo :: atom(),
               field2 :: undefined,
               field3 = foo,
               field4}).
```
</td><td>
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
</td><td></td></tr>
</table>
