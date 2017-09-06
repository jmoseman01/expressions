(* ============================================================================================== *) 
datatype lexresult  = SHELL of string * string * {line: word, column: word};
val error           = fn x => TextIO.output(TextIO.stdOut,x ^ "\n")
val eof             = fn () => SHELL("","eof",getNextTokenPos(""))
val counter         = ref 0;
(* ============================================================================================== *)
(* ------------------------------------------------------------------ *)
(* assumes that ">" does not occur as part of a nonterminal symbol *)
fun generateSchemaTokenName( yytext ) =
    let
        fun split(x, []   ) =  raise General.Fail("an_error")
          | split(x, y::ys) = if x=y then ys else split(x,ys);
                                                    
        fun splitFirst(symbol,[])    =     [] (* symbol was not in the input list *)
          | splitFirst(symbol,x::xs) =     if x = symbol 
                        then (* found split point *)
                            []
                        else (* keep looking      *)
                            x::splitFirst(symbol,xs);
                                                                        
        val s0   = explode(yytext);
        val s1   = split(#"<",s0);
        val s2   = splitFirst(#">",s1);  
    in
        implode(explode("!#schema_variable_") @ s2)        
    end;
    
(* ------------------------------------------------------------------ *)

(* ============================================================================================== *)
%%
%header (functor Target_LexFn(val getNextTokenPos : string -> {line: word, column: word}));

alpha        = [A-Za-z];
digit        = [0-9];
alphanumeric = [A-Za-z0-9_];
ws           = [\  \t \n];
symbol       = [<];
boolean      = "true" | "false";
identifier   = {alpha}{alphanumeric}*;
string       = \"{alphanumeric}*\";
symbolic_id  = {symbol}+{alphanumeric}*;
number       = {digit}+(\.{digit}+)?;
schema_id    = "<" {alpha}{alphanumeric}* ">_" {alphanumeric}+;
ws           = [\  \t \n];


%s COMMENT;
%%

<COMMENT> "(*"                      => ( counter := !counter + 1; getNextTokenPos(yytext); lex()         );
<COMMENT> "*)"                      => ( counter := !counter - 1; 
                                         if !counter = 0 then YYBEGIN INITIAL 
                                         else (); 
                                         getNextTokenPos(yytext); lex()                                  );
<COMMENT> "\n"                      => ( getNextTokenPos(yytext); lex()                                  );
<COMMENT> .              	        => ( getNextTokenPos(yytext); lex()                                  );


<INITIAL> "(*"                      => ( YYBEGIN COMMENT; 
                                         counter := !counter + 1; 
                                         getNextTokenPos(yytext); lex()                                  );

<INITIAL> ";"                       => ( SHELL(yytext                            , yytext,     getNextTokenPos(yytext))    );
<INITIAL> ","                       => ( SHELL(yytext                            , yytext,     getNextTokenPos(yytext))    );
<INITIAL> "("                       => ( SHELL(yytext                            , yytext,     getNextTokenPos(yytext))    );
<INITIAL> ")"                       => ( SHELL(yytext                            , yytext,     getNextTokenPos(yytext))    );

<INITIAL> "<"                       => ( SHELL(yytext                            , yytext,     getNextTokenPos(yytext))    );
<INITIAL> "<="                      => ( SHELL(yytext                            , yytext,     getNextTokenPos(yytext))    );
<INITIAL> ">"                       => ( SHELL(yytext                            , yytext,     getNextTokenPos(yytext))    );
<INITIAL> ">="                      => ( SHELL(yytext                            , yytext,     getNextTokenPos(yytext))    );
<INITIAL> "="                       => ( SHELL(yytext                            , yytext,     getNextTokenPos(yytext))    );
<INITIAL> "<>"                      => ( SHELL(yytext                            , yytext,     getNextTokenPos(yytext))    );

<INITIAL> "+"                       => ( SHELL(yytext                            , yytext,     getNextTokenPos(yytext))    );
<INITIAL> "-"                       => ( SHELL(yytext                            , yytext,     getNextTokenPos(yytext))    );

<INITIAL> "~"                       => ( SHELL(yytext                            , yytext,     getNextTokenPos(yytext))    );
<INITIAL> "*"                       => ( SHELL(yytext                            , yytext,     getNextTokenPos(yytext))    );
<INITIAL> "->"                      => ( SHELL(yytext                            , yytext,     getNextTokenPos(yytext))    );


<INITIAL> "andalso"                 => ( SHELL(yytext                            , yytext,     getNextTokenPos(yytext))    );
<INITIAL> "bool"                    => ( SHELL(yytext                            , yytext,     getNextTokenPos(yytext))    );
<INITIAL> "correct"                 => ( SHELL(yytext                            , yytext,     getNextTokenPos(yytext))    );
<INITIAL> "div"                     => ( SHELL(yytext                            , yytext,     getNextTokenPos(yytext))    );
<INITIAL> "else"                    => ( SHELL(yytext                            , yytext,     getNextTokenPos(yytext))    );
<INITIAL> "error"                   => ( SHELL(yytext                            , yytext,     getNextTokenPos(yytext))    );
<INITIAL> "if"                      => ( SHELL(yytext                            , yytext,     getNextTokenPos(yytext))    );
<INITIAL> "incorrect"               => ( SHELL(yytext                            , yytext,     getNextTokenPos(yytext))    );
<INITIAL> "int"                     => ( SHELL(yytext                            , yytext,     getNextTokenPos(yytext))    );
<INITIAL> "mod"                     => ( SHELL(yytext                            , yytext,     getNextTokenPos(yytext))    );
<INITIAL> "orelse"                  => ( SHELL(yytext                            , yytext,     getNextTokenPos(yytext))    );
<INITIAL> "then"                    => ( SHELL(yytext                            , yytext,     getNextTokenPos(yytext))    );


<INITIAL> {boolean}                 => ( SHELL("boolean_value"                   , yytext,     getNextTokenPos(yytext))    );
<INITIAL> "~"? {digit}+             => ( SHELL("integer_value"                   , yytext,     getNextTokenPos(yytext))    );
<INITIAL> {identifier}              => ( SHELL("ident"                           , yytext,     getNextTokenPos(yytext))    );

<INITIAL> {ws}+                     => ( getNextTokenPos(yytext); lex()  );
<INITIAL> {schema_id}               => ( SHELL(generateSchemaTokenName(yytext), yytext, getNextTokenPos(yytext))    );
<INITIAL> "[:]"                     => ( SHELL(""        , yytext, getNextTokenPos(yytext))    );

<INITIAL> "~~"                      => ( raise Fail("unbound variable or constructor: " ^ yytext) );

<INITIAL> .                         => ( error("ignored an unprintable character: " ^ yytext); getNextTokenPos(yytext); lex()  );
