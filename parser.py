import ply.yacc as yacc
import ply.lex as lex
from calclex import tokens

tokens = [
   "ARROW",
   "CONCAT",
   "NONTERMINAL",
   "TERMINAL",
   "SEPARATOR",
   "EPS"
]

t_ARROW       = r"-\>"
t_CONCAT      = r"\,"
t_NONTERMINAL = r"[A-Z]+"
t_TERMINAL    = r"[a-z]"
t_SEPARATOR   = r"\n"
t_EPS         = r"@"
t_ignore      = " \t"

my_error = ""

def t_error(t):
    my_error = "Illegal character '%s'" % t.value[0]
    t.lexer.skip(1)

lexer = lex.lex()



def p_split_conversion(p):
    "conversions : conversions conversion"
    p[0] = list(p[1])
    p[0].append(p[2])

def p_last_conversion(p):
    "conversions : conversion"
    p[0] = []
    p[0].append(p[1])

def p_parse_conversion(p):
    "conversion : NONTERMINAL ARROW right SEPARATOR"
    p[0] = (p[1], p[3])

def p_split_thing(p):
    "right : thing CONCAT right"
    p[0] = p[1] + p[3]

def p_last_thing(p):
    "right : thing"
    p[0] = p[1]

def p_thing(p):
    """thing : TERMINAL
             | NONTERMINAL"""
    p[0] = []
    p[0].append(p[1])

def p_thing_eps(p):
    "thing : EPS"
    p[0] = []

def p_error(p):
    print("Syntax error in input!")

parser = yacc.yacc()
