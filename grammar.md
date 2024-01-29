Grammar is based on the following sources:
* https://slebok.github.io/zoo/fortran/f90/waite-cordy/extracted/index.html#Primary
* https://flang.llvm.org/docs/f2018-grammar.html

The syntax of the document is EBNF-like.
Note that the parser/lexer may accept syntax that is not allowed by the grammar (keywords are case insensitive, both GO TO and GOTO is allowed)

* `[nterm]` - nterm is optional
* `{nterm}` - nterm repeats zero or more times
* `a | b` - a or b
* `"text"` - text is a terminal
* `'t'` - t is a terminal

```
program =
        "PROGRAM" program-name
        block
        "END PROGRAM"

block = {execution-part-construct}

execution-part-construct = [label] executable-construct
executable-construct =
       action-stmt |
       if-construct |
       do-construct 

action-stmt =
       assignment-stmt | 
       goto-stmt | 
       if-stmt | 
       write-stmt | read-stmt | 
       computed-goto-stmt 

assignment-stmt = variable '=' expr
if-stmt = "IF" '(' expr ')' label ',' label ',' label
write-stmt = "WRITE" "(*,*)" [','] [output-item-list]
output-item-list = expr {"," expr}
read-stmt = "READ" "(*,*)" [','] [input-item-list]
input-item-lsit = variable {"," variable}
computed-goto-stmt = "GO TO" '(' label-list ')' [,] expr
goto-stmt = "GO TO" label

if-construct =
        "IF" '(' expr ')' "THEN" block {else-if-stmt block} [else-stmt block]
        "END" "IF"
else-if-stmt = "ELSE" "IF" '(' expr ')' "THEN" 
else-stmt -> "ELSE"

do-construct = "DO" [loop-control] block "END" "DO"
loop-control = [','] variable '=' expr ',' expr [',' expr]



expr = equivalence
equivalence = equiv-operand {equiv-op equiv-operand}
equiv-operand = or-operand {or-op or-operand}
or-operand = and-operand {and-op and-operand}
and-operand = [not-op] level-4-expr
level-4-expr = level-2-expr {rel-op level-2-expr}
level-2-expr = [sign] add-operand {add-op add-operand}
add-operand = mult-operand {mult-op mult-operand}
mult-operand = primary [power-op mult-operand]

primary = literal | variable | '(' expr ')' | function-call
function-call = name '(' [args-list] ')'
args-list = expr {',' expr}

equiv-op = ".eqv." | ".neqv."
or-op = ".or." 
and-op = ".and."
not-op = ".not."
rel-op = ".eq." | ".ne." | ".lt." | ".le." | ".gt." | ".ge."
add-op = '+' | '-'
mult-op = '*' | '/'
power-op = "**"



literal = integer | float | string
integer = digit {digit}
string = '"' {ascii-char-except-for-newline-or-double-quotes} '"'
variable = ascii-letter {ascii-letter | digit}
float = integer ['.' integer]

program-name = variable
label = integer
```
