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
<tr><td><div class="highlight highlight-source-erlang"><pre>
any()
</pre></div></td>
<td><div class="highlight highlight-source-erlang"><pre>
{type,31,any,[]}
</pre></div></td>
<td></td></tr>
<tr><td><div class="highlight highlight-source-erlang"><pre>
none()
</pre></div></td>
<td><div class="highlight highlight-source-erlang"><pre>
{type,32,none,[]}
</pre></div></td>
<td></td></tr>
<tr><td><div class="highlight highlight-source-erlang"><pre>
pid()
</pre></div></td>
<td><div class="highlight highlight-source-erlang"><pre>
{type,33,pid,[]}
</pre></div></td>
<td></td></tr>
<tr><td><div class="highlight highlight-source-erlang"><pre>
port()
</pre></div></td>
<td><div class="highlight highlight-source-erlang"><pre>
{type,34,port,[]}
</pre></div></td>
<td></td></tr>
<tr><td><div class="highlight highlight-source-erlang"><pre>
reference()
</pre></div></td>
<td><div class="highlight highlight-source-erlang"><pre>
{type,35,reference,[]}
</pre></div></td>
<td></td></tr>
<tr><td><div class="highlight highlight-source-erlang"><pre>
[]
</pre></div></td>
<td><div class="highlight highlight-source-erlang"><pre>
{type,36,nil,[]}
</pre></div></td>
<td></td></tr>
<tr><td><div class="highlight highlight-source-erlang"><pre>
atom()
</pre></div></td>
<td><div class="highlight highlight-source-erlang"><pre>
{type,37,atom,[]}
</pre></div></td>
<td></td></tr>
<tr><td><div class="highlight highlight-source-erlang"><pre>
<<>>
</pre></div></td>
<td><div class="highlight highlight-source-erlang"><pre>
{type,40,binary,[{integer,40,0},{integer,40,0}]}
</pre></div></td>
<td></td></tr>
<tr><td><div class="highlight highlight-source-erlang"><pre>
<<_:M>>
</pre></div></td>
<td><div class="highlight highlight-source-erlang"><pre>
{type,42,binary,[{var,42,'M'},{integer,42,0}]}
</pre></div></td>
<td>M is a positive integer</td></tr>
<tr><td><div class="highlight highlight-source-erlang"><pre>
<<_:_*N>>
</pre></div></td>
<td><div class="highlight highlight-source-erlang"><pre>
{type,44,binary,[{integer,44,0},{var,44,'N'}]}
</pre></div></td>
<td>N is a positive integer</td></tr>
<tr><td><div class="highlight highlight-source-erlang"><pre>
<<_:M, _:_*N>>
</pre></div></td>
<td><div class="highlight highlight-source-erlang"><pre>
{type,45,binary,[{var,45,'M'},{var,45,'N'}]}
</pre></div></td>
<td></td></tr>
<tr><td><div class="highlight highlight-source-erlang"><pre>
float()
</pre></div></td>
<td><div class="highlight highlight-source-erlang"><pre>
{type,47,float,[]}
</pre></div></td>
<td></td></tr>
<tr><td><div class="highlight highlight-source-erlang"><pre>
fun()
</pre></div></td>
<td><div class="highlight highlight-source-erlang"><pre>
{type,51,'fun',[]}
</pre></div></td>
<td>any function</td></tr>
<tr><td><div class="highlight highlight-source-erlang"><pre>
fun((...) -> integer())
</pre></div></td>
<td><div class="highlight highlight-source-erlang"><pre>
{type,53,'fun',[{type,53,any},{type,53,integer,[]}]}
</pre></div></td>
<td>any arity, returning Type</td></tr>
<tr><td><div class="highlight highlight-source-erlang"><pre>
fun(() -> integer())
</pre></div></td>
<td><div class="highlight highlight-source-erlang"><pre>
{type,54,'fun',[{type,54,product,[]},{type,54,integer,[]}]}
</pre></div></td>
<td></td></tr>
<tr><td><div class="highlight highlight-source-erlang"><pre>
fun((atom(), atom()) -> integer())
</pre></div></td>
<td><div class="highlight highlight-source-erlang"><pre>
{type,55,'fun',
      [{type,55,product,[{type,55,atom,[]},{type,55,atom,[]}]},
       {type,55,integer,[]}]}
</pre></div></td>
<td></td></tr>
<tr><td><div class="highlight highlight-source-erlang"><pre>
integer()
</pre></div></td>
<td><div class="highlight highlight-source-erlang"><pre>
{type,58,integer,[]}
</pre></div></td>
<td></td></tr>
<tr><td><div class="highlight highlight-source-erlang"><pre>
42
</pre></div></td>
<td><div class="highlight highlight-source-erlang"><pre>
{integer,60,42}
</pre></div></td>
<td>..., -1, 0, 1, ... 42 ...</td></tr>
<tr><td><div class="highlight highlight-source-erlang"><pre>
1..10
</pre></div></td>
<td><div class="highlight highlight-source-erlang"><pre>
{type,62,range,[{integer,62,1},{integer,62,10}]}
</pre></div></td>
<td>specifies an integer range</td></tr>
<tr><td><div class="highlight highlight-source-erlang"><pre>
[integer()]
</pre></div></td>
<td><div class="highlight highlight-source-erlang"><pre>
{type,66,list,[{type,66,integer,[]}]}
</pre></div></td>
<td>Proper list ([]-terminated)</td></tr>
<tr><td><div class="highlight highlight-source-erlang"><pre>
maybe_improper_list(integer(), atom())
</pre></div></td>
<td><div class="highlight highlight-source-erlang"><pre>
{type,68,maybe_improper_list,[{type,68,integer,[]},{type,68,atom,[]}]}
</pre></div></td>
<td>Type1=contents, Type2=termination</td></tr>
<tr><td><div class="highlight highlight-source-erlang"><pre>
nonempty_improper_list(integer(), atom())
</pre></div></td>
<td><div class="highlight highlight-source-erlang"><pre>
{type,70,nonempty_improper_list,[{type,70,integer,[]},{type,70,atom,[]}]}
</pre></div></td>
<td>Type1 and Type2 as above</td></tr>
<tr><td><div class="highlight highlight-source-erlang"><pre>
[integer(), ...]
</pre></div></td>
<td><div class="highlight highlight-source-erlang"><pre>
{type,72,nonempty_list,[{type,72,integer,[]}]}
</pre></div></td>
<td>Proper non-empty list</td></tr>
<tr><td><div class="highlight highlight-source-erlang"><pre>
map()
</pre></div></td>
<td><div class="highlight highlight-source-erlang"><pre>
{type,76,map,any}
</pre></div></td>
<td>denotes a map of any size</td></tr>
<tr><td><div class="highlight highlight-source-erlang"><pre>
#{}
</pre></div></td>
<td><div class="highlight highlight-source-erlang"><pre>
{type,78,map,[]}
</pre></div></td>
<td>denotes the empty map</td></tr>
<tr><td><div class="highlight highlight-source-erlang"><pre>
#{integer() => any()}
</pre></div></td>
<td><div class="highlight highlight-source-erlang"><pre>
{type,79,map,
      [{type,80,map_field_assoc,[{type,80,integer,[]},{type,80,any,[]}]}]}
</pre></div></td>
<td></td></tr>
<tr><td><div class="highlight highlight-source-erlang"><pre>
tuple()
</pre></div></td>
<td><div class="highlight highlight-source-erlang"><pre>
{type,83,tuple,any}
</pre></div></td>
<td></td></tr>
<tr><td><div class="highlight highlight-source-erlang"><pre>
{}
</pre></div></td>
<td><div class="highlight highlight-source-erlang"><pre>
{type,84,tuple,[]}
</pre></div></td>
<td></td></tr>
<tr><td><div class="highlight highlight-source-erlang"><pre>
{atom()}
</pre></div></td>
<td><div class="highlight highlight-source-erlang"><pre>
{type,85,tuple,[{type,85,atom,[]}]}
</pre></div></td>
<td></td></tr>
<tr><td><div class="highlight highlight-source-erlang"><pre>
{atom(), integer()}
</pre></div></td>
<td><div class="highlight highlight-source-erlang"><pre>
{type,86,tuple,[{type,86,atom,[]},{type,86,integer,[]}]}
</pre></div></td>
<td></td></tr>
<tr><td><div class="highlight highlight-source-erlang"><pre>
atom() | integer()
</pre></div></td>
<td><div class="highlight highlight-source-erlang"><pre>
{type,89,union,[{type,89,atom,[]},{type,89,integer,[]}]}
</pre></div></td>
<td></td></tr>
</table>

For convenience, the following types are also built-in. They can be
thought as predefined aliases for the type unions shown in the code
comments.

<table>
<tr><td><div class="highlight highlight-source-erlang"><pre>
term()
</pre></div></td>
<td><div class="highlight highlight-source-erlang"><pre>
{type,104,term,[]}
</pre></div></td>
<td>term() :: any()</td></tr>
<tr><td><div class="highlight highlight-source-erlang"><pre>
binary()
</pre></div></td>
<td><div class="highlight highlight-source-erlang"><pre>
{type,106,binary,[]}
</pre></div></td>
<td>binary() :: <<_:_*8>></td></tr>
<tr><td><div class="highlight highlight-source-erlang"><pre>
bitstring()
</pre></div></td>
<td><div class="highlight highlight-source-erlang"><pre>
{type,108,bitstring,[]}
</pre></div></td>
<td>bitstring() :: <<_:_*1>></td></tr>
<tr><td><div class="highlight highlight-source-erlang"><pre>
boolean()
</pre></div></td>
<td><div class="highlight highlight-source-erlang"><pre>
{type,110,boolean,[]}
</pre></div></td>
<td>boolean() :: 'false' | 'true'</td></tr>
<tr><td><div class="highlight highlight-source-erlang"><pre>
byte()
</pre></div></td>
<td><div class="highlight highlight-source-erlang"><pre>
{type,112,byte,[]}
</pre></div></td>
<td>byte() :: 0..255</td></tr>
<tr><td><div class="highlight highlight-source-erlang"><pre>
char()
</pre></div></td>
<td><div class="highlight highlight-source-erlang"><pre>
{type,114,char,[]}
</pre></div></td>
<td>char() :: 0..16#10ffff</td></tr>
<tr><td><div class="highlight highlight-source-erlang"><pre>
[]
</pre></div></td>
<td><div class="highlight highlight-source-erlang"><pre>
{type,116,nil,[]}
</pre></div></td>
<td>nil() :: []</td></tr>
<tr><td><div class="highlight highlight-source-erlang"><pre>
number()
</pre></div></td>
<td><div class="highlight highlight-source-erlang"><pre>
{type,118,number,[]}
</pre></div></td>
<td>number() :: integer() | float()</td></tr>
<tr><td><div class="highlight highlight-source-erlang"><pre>
list()
</pre></div></td>
<td><div class="highlight highlight-source-erlang"><pre>
{type,120,list,[]}
</pre></div></td>
<td>list() :: [any()]</td></tr>
<tr><td><div class="highlight highlight-source-erlang"><pre>
maybe_improper_list()
</pre></div></td>
<td><div class="highlight highlight-source-erlang"><pre>
{type,122,maybe_improper_list,[]}
</pre></div></td>
<td>maybe_improper_list() :: maybe_improper_list(any(), any())</td></tr>
<tr><td><div class="highlight highlight-source-erlang"><pre>
nonempty_list()
</pre></div></td>
<td><div class="highlight highlight-source-erlang"><pre>
{type,124,nonempty_list,[]}
</pre></div></td>
<td>nonempty_list() :: nonempty_list(any())</td></tr>
<tr><td><div class="highlight highlight-source-erlang"><pre>
string()
</pre></div></td>
<td><div class="highlight highlight-source-erlang"><pre>
{type,126,string,[]}
</pre></div></td>
<td>string() :: [char()]</td></tr>
<tr><td><div class="highlight highlight-source-erlang"><pre>
nonempty_string()
</pre></div></td>
<td><div class="highlight highlight-source-erlang"><pre>
{type,128,nonempty_string,[]}
</pre></div></td>
<td>nonempty_string() :: [char(),...]</td></tr>
<tr><td><div class="highlight highlight-source-erlang"><pre>
iodata()
</pre></div></td>
<td><div class="highlight highlight-source-erlang"><pre>
{type,130,iodata,[]}
</pre></div></td>
<td>iodata() :: iolist() | binary()</td></tr>
<tr><td><div class="highlight highlight-source-erlang"><pre>
iolist()
</pre></div></td>
<td><div class="highlight highlight-source-erlang"><pre>
{type,132,iolist,[]}
</pre></div></td>
<td>iolist() :: maybe_improper_list(byte() | binary() | iolist(), binary() | [])</td></tr>
<tr><td><div class="highlight highlight-source-erlang"><pre>
function()
</pre></div></td>
<td><div class="highlight highlight-source-erlang"><pre>
{type,134,function,[]}
</pre></div></td>
<td>function() :: fun()</td></tr>
<tr><td><div class="highlight highlight-source-erlang"><pre>
module()
</pre></div></td>
<td><div class="highlight highlight-source-erlang"><pre>
{type,136,module,[]}
</pre></div></td>
<td>module() :: atom()</td></tr>
<tr><td><div class="highlight highlight-source-erlang"><pre>
mfa()
</pre></div></td>
<td><div class="highlight highlight-source-erlang"><pre>
{type,138,mfa,[]}
</pre></div></td>
<td>mfa() :: {module(),atom(),arity()}</td></tr>
<tr><td><div class="highlight highlight-source-erlang"><pre>
arity()
</pre></div></td>
<td><div class="highlight highlight-source-erlang"><pre>
{type,140,arity,[]}
</pre></div></td>
<td>arity() :: 0..255</td></tr>
<tr><td><div class="highlight highlight-source-erlang"><pre>
identifier()
</pre></div></td>
<td><div class="highlight highlight-source-erlang"><pre>
{type,142,identifier,[]}
</pre></div></td>
<td>identifier() :: pid() | port() | reference()</td></tr>
<tr><td><div class="highlight highlight-source-erlang"><pre>
node()
</pre></div></td>
<td><div class="highlight highlight-source-erlang"><pre>
{type,144,node,[]}
</pre></div></td>
<td>node() :: atom()</td></tr>
<tr><td><div class="highlight highlight-source-erlang"><pre>
timeout()
</pre></div></td>
<td><div class="highlight highlight-source-erlang"><pre>
{type,146,timeout,[]}
</pre></div></td>
<td>timeout() :: 'infinity' | non_neg_integer()</td></tr>
<tr><td><div class="highlight highlight-source-erlang"><pre>
no_return()
</pre></div></td>
<td><div class="highlight highlight-source-erlang"><pre>
{type,148,no_return,[]}
</pre></div></td>
<td>no_return() :: none()</td></tr>
</table>

Built-in types; can be thought defined by the syntax...

<table>
<tr><td><div class="highlight highlight-source-erlang"><pre>
non_neg_integer()
</pre></div></td>
<td><div class="highlight highlight-source-erlang"><pre>
{type,154,non_neg_integer,[]}
</pre></div></td>
<td>non_neg_integer() :: 0..</td></tr>
<tr><td><div class="highlight highlight-source-erlang"><pre>
pos_integer()
</pre></div></td>
<td><div class="highlight highlight-source-erlang"><pre>
{type,156,pos_integer,[]}
</pre></div></td>
<td>pos_integer() :: 1..</td></tr>
<tr><td><div class="highlight highlight-source-erlang"><pre>
neg_integer()
</pre></div></td>
<td><div class="highlight highlight-source-erlang"><pre>
{type,158,neg_integer,[]}
</pre></div></td>
<td>neg_integer() :: ..-1</td></tr>
</table>

Record declarations
-------------------


<table>
<tr><td><div class="highlight highlight-source-erlang"><pre>
-record(myrec,{field1 = foo,field2,field3 = foo,field4}).

</pre></div></td>
<td><div class="highlight highlight-source-erlang"><pre>
{attribute,168,record,
           {myrec,[{record_field,168,{atom,168,field1},{atom,168,foo}},
                   {record_field,169,{atom,169,field2}},
                   {record_field,170,{atom,170,field3},{atom,170,foo}},
                   {record_field,171,{atom,171,field4}}]}}
</pre></div></td>
<td>A record is represented as a form without types. If any fields are typed,
the form is followed by a 'record type' form. Thus, for records with typed
fields, there are two consecutive forms.</td></tr>
<tr><td><div class="highlight highlight-source-erlang"><pre>
-record(myrec,{field1 = foo :: atom(),
               field2 :: undefined,
               field3 = foo,
               field4}).
</pre></div></td>
<td><div class="highlight highlight-source-erlang"><pre>
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
</pre></div></td>
<td></td></tr>
</table>
