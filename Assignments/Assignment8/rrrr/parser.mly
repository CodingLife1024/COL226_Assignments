// parser.mly

// Header section
%{
    open Interpreter;;
%}

// Token definitions
%token <string> VAR CONS  // Variable and constructor tokens carry string values
%token <int> NUM// Number token carries an integer value
%token LP RP LB RB COMMA// Punctuation tokens
%token EQ NOT_EQ// Comparison operator tokens
%token ENDL CUT COND PIPE // Control flow and list operator tokens
%token PLUS MINUS MULT DIV // Arithmetic operator tokens
%token GT LT EOF// Comparison operator tokens and end-of-file token

// Operator precedence and associativity
%left COMMA // Comma has left associativity
%nonassoc EQ PIPE LT GT // Comparison operators are non-associative
%left PLUS MINUS// Addition and subtraction have left associativity
%left MULT DIV// Multiplication and division have left associativity
%nonassoc ENDL// End of line token

// Entry points and types
%start program goal// Define the start symbols and their types
%type <Interpreter.program> program
%type <Interpreter.goal> goal

// Grammar rules
%%

// Program is composed of a list of clauses, terminated by EOF
program:
    EOF {[]}// Empty program
  | clause_list EOF {$1}// Non-empty program
;

// A clause list is either a single clause or a clause followed by another clause list
clause_list:
    clause {[$1]}// Single clause
  | clause clause_list {($1)::$2}  // Clause followed by another clause list
;

// A clause can be a fact or a rule
clause:
    atom ENDL {F(H($1))}// Fact: Head followed by end-of-line
  | atom COND atom_list ENDL {R(H($1), B($3))} // Rule: Head, condition, and body followed by end-of-line
;

// A goal is a list of atoms, terminated by an end-of-line
goal:
    atom_list ENDL {G($1)}// List of atoms followed by end-of-line
;

// An atom list is either a single atom or an atom followed by another atom list
atom_list:
    atom {[$1]} // Single atom
  | atom COMMA atom_list {($1)::$3} // Atom followed by another atom list
;

// An atom represents a predicate or an operation
atom:
    | CONS {A($1, [])}// Constructor with no arguments
  | CONS LP term_list RP {A($1, $3)} // Constructor with arguments
  | term EQ term {A("_eq", [$1; $3])} // Equality comparison
  | term NOT_EQ term {A("_not_eq", [$1; $3])} // Inequality comparison
  | term LT term {A("<", [$1; $3])} // Less than comparison
  | term GT term {A(">", [$1; $3])} // Greater than comparison
  | CUT {A("_cut", [])} // Cut operation
;

// A term list is either a single term or a term followed by another term list
term_list:
    term {[$1]}                     // Single term
  | term COMMA term_list {($1)::$3} // Term followed by another term list
;

// A term represents a value, variable, or operation
term:
    LP term RP {$2}// Term enclosed in parentheses
  | VAR {V($1)} // Variable
  | CONS {Node($1, [])}// Constructor with no arguments
  | NUM {Num($1)}// Number
  | CONS LP term_list RP {Node($1, $3)} // Constructor with arguments
  | term PLUS term {Node("+", [$1; $3])} // Addition
  | term MINUS term {Node("-", [$1; $3])} // Subtraction
  | term MULT term {Node("*", [$1; $3])} // Multiplication
  | term DIV term {Node("/", [$1; $3])} // Division
  | list {$1}// List
;

// A list is either an empty list or a list with elements
list:
    LB RB {Node("_empty_list", [])} // Empty list
  | LB list_body RB {$2}// List with elements
;

// A list body is either a single element or an element followed by another list body
list_body:
    term {Node("_list", [$1; Node("_empty_list", [])])} // Single element
  | term COMMA list_body {Node("_list", [$1; $3])} // Element followed by another list body
  | term PIPE term {Node("_list", [$1; $3])} // List concatenation
;
