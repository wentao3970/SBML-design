#Student Name:Wentao Wu; Student ID#:112524704
class Node:
    def __init__(self):
        print("init node")

    def evaluate(self):
        return 0

    def execute(self):
        return 0

class PrintNode(Node):
    def __init__(self, v):
        self.value = v
    def execute(self):
        if (isinstance(self.value, StringNode) or
            isinstance(self.value, StringConcatNode)):
           print('\'' + self.value.evaluate() + '\'')
        elif (isinstance(self.value, IndexNodeL) or
              isinstance(self.value, IndexNodeS)):
           if isinstance(self.value.evaluate(), str):
               print('\'' + self.value.evaluate() + '\'')
           else:
               print(self.value.evaluate())
        else:
            print(self.value.evaluate())

class NumberNode(Node):
    def __init__(self, v):
        if('.' in v):
            self.value = float(v)
        else:
            self.value = int(v)
    def evaluate(self):
        return self.value

class StringNode(Node):
    def __init__(self, v):
        self.value = str(v)
    def evaluate(self):
        return self.value

class TupleNode(Node):
    def __init__(self, v):
        self.value = v
    def evaluate(self):
        return self.value

class IndexNodeT(Node):
    def __init__(self, v1, v2):
        self.v1 = v1
        self.v2 = v2
    def evaluate(self):
        return self.v1.evaluate()[self.v2.evaluate()-1]

class BlankListNode(Node):
    def __init__(self, v):
        self.value = v
    def evaluate(self):
        return self.value

class ListNode(Node):
    def __init__(self, v):
        self.value = v
    def evaluate(self):
        return self.value

class BooleanNode(Node):
    def __init__(self, v):
        if v == 'true' or v == 'True':
            self.value = bool(True)
        elif v == 'false' or v == 'False':
            self.value = bool(False)
    def evaluate(self):
        return self.value

class IndexNodeL(Node):
    def __init__(self, v1, v2):
        self.v1 = v1.evaluate()
        self.v2 = v2.evaluate()
    def evaluate(self):
        return self.v1[self.v2]

class IndexNodeS(Node):
    def __init__(self, v1, v2):
        self.v1 = v1.evaluate()
        self.v2 = v2.evaluate()
    def evaluate(self):
        return self.v1[self.v2]

class BopNode(Node):
    def __init__(self, op, v1, v2):
        self.v1 = v1.evaluate()
        self.v2 = v2.evaluate()
        self.op = op
    def evaluate(self):
        if (self.op == '+'):
            return self.v1 + self.v2
        elif (self.op == '-'):
            return self.v1 - self.v2
        elif (self.op == '*'):
            return self.v1 * self.v2
        elif (self.op == '/'):
            return self.v1 / self.v2
        elif (self.op == '**'):
            return self.v1 ** self.v2
        elif (self.op == 'div'):
            return self.v1 // self.v2
        elif (self.op == 'mod'):
            return self.v1 % self.v2

class UnaryNode(Node):
    def __init__(self, v):
        self.v = v.evaluate()
    def evaluate(self):
        return -self.v


class BooleanInNode(Node):
    def __init__(self, op, v1, v2):
        self.v1 = v1.evaluate()
        self.v2 = v2.evaluate()
        self.op = op
    def evaluate(self):
        return self.v1 in self.v2

class ElementConcatNode(Node):
    def __init__(self, op, v1, v2):
        self.v1 = v1.evaluate()
        self.v2 = v2.evaluate()
        self.op = op
    def evaluate(self):
        return [self.v1] + self.v2

class StringConcatNode(Node):
    def __init__(self, op, v1, v2):
        self.v1 = v1.evaluate()
        self.v2 = v2.evaluate()
        self.op = op
    def evaluate(self):
        return self.v1 + self.v2

class ListConcatNode(Node):
    def __init__(self, op, v1, v2):
        self.v1 = v1.evaluate()
        self.v2 = v2.evaluate()
        self.op = op
    def evaluate(self):
        return self.v1 + self.v2

class BooleanNode1(Node):
    def __init__(self, op, v1, v2):
        self.v1 = v1.evaluate()
        self.v2 = v2.evaluate()
        self.op = op
    def evaluate(self):
        if (self.op == '<>'):
            return self.v1 != self.v2
        elif (self.op == '>'):
            return self.v1 > self.v2
        elif (self.op == '<'):
            return self.v1 < self.v2
        elif (self.op == '>='):
            return self.v1 >= self.v2
        elif (self.op == '<='):
            return self.v1 <= self.v2
        elif (self.op == '=='):
            return self.v1 == self.v2

class BooleanNotNode(Node):
    def __init__(self, v):
        self.v = v
    def evaluate(self):
        return not self.v.evaluate()

class BooleanNode2(Node):
    def __init__(self, op, v1, v2):
        self.v1 = v1.evaluate()
        self.v2 = v2.evaluate()
        self.op = op
    def evaluate(self):
        if (self.op == 'andalso'):
            return self.v1 and self.v2
        elif (self.op == 'orelse'):
            return self.v1 or self.v2

tokens = (
    'NUMBER','STRING',
    'PLUS','MINUS','TIMES','DIVIDE','DIV','MOD','POWER',
    'EQUALS','LT', 'LE', 'GT', 'GE', 'NE',
    'BOOLEAN','AND','OR','NOT','IN','CONCAT',
    'LPAREN','RPAREN','LBRACKET','RBRACKET',
    'COMMA','SEMICOLON','TUPLEINDEX'
    )

t_PLUS = r'\+'
t_MINUS = r'-'
t_TIMES = r'\*'
t_DIVIDE = r'/'
t_DIV = r'div'
t_MOD = r'mod'
t_POWER   =r'\*\*'

t_NE = r'<>'
t_EQUALS  = r'=='
t_LT = r'<'
t_LE = r'<='
t_GT = r'>'
t_GE = r'>='

t_NOT = r'\bnot\b'
t_AND = r'(\bandalso\b)'
t_OR = r'\borelse\b'
t_IN = r'\bin\b'
t_CONCAT = r'\:\:'

t_LPAREN = r'\('
t_RPAREN = r'\)'
t_LBRACKET = r'\['
t_RBRACKET = r'\]'

t_COMMA = r'\,'
t_SEMICOLON = r';'
t_TUPLEINDEX = r'\#'

def t_NUMBER(t):
    r'-?\d*(\d\.|\.\d)\d*([eE][-+]? \d+)? | \d+'
    try:
        t.value = NumberNode(t.value)
    except ValueError:
        print("NUMBER value too large %d", t.value)
        t.value = 0
    return t

def t_STRING(t):
    r'(\"(([^\"]|\\\"|\\\')*[^\\])?\")|(\'(([^\']|\\\"|\\\')*[^\\])?\')'
    t.value = StringNode(t.value[1:-1])
    return t

def t_BOOLEAN(t):
    r'\btrue\b | \bfalse\b | \bTrue\b | \bFalse\b'
    t.value = BooleanNode(t.value)
    return t

# Ignored characters
t_ignore = " \t"

def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")

def t_error(t):
#    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

# Build the lexer
import ply.lex as lex
lex.lex(debug=0)

# Precedence rules for operators
precedence = (
    ('left','NOT','AND','OR'),
    ('left','EQUALS','LT','LE','NE','GT','GE'),
    ('left','CONCAT'),
    ('left','IN'),
    ('left','PLUS','MINUS'),
    ('left','TIMES','DIVIDE','DIV','MOD'),
    ('right','POWER'),
    ('left','TUPLEINDEX'),
    ('right','UMINUS')
    )

def p_print_smt(p):
    'print_smt : expression SEMICOLON'
    p[0] = PrintNode(p[1])

#parenthesized expression
def p_expression_group(p):
    'expression : LPAREN expression RPAREN'
    p[0] = p[2]

#Tuple Rules
#Create a tuple
def p_expression_tuple(p):
    'expression : tuple'
    p[0] = p[1]

def p_tuple1(p):
    'tuple : LPAREN expression tupletail'
    p[0] = [p[2].evaluate()] + p[3]
    p[0] = tuple(p[0])
    p[0] = TupleNode(p[0])

def p_tuple2(p):
    'tupletail : COMMA expression tupletail'
    p[0] = [p[2].evaluate()] + p[3]

def p_tupletail(p):
    'tupletail : RPAREN'
    p[0] = list()

def p_tuple_index(p):
    'expression : TUPLEINDEX expression tuple'
    p[0] = IndexNodeT(p[3], p[2])

#Arithmetic Opreations
def p_expression_binop(p):
    '''expression : expression PLUS expression
                  | expression MINUS expression'''
    p[0] = BopNode(p[2], p[1], p[3])

def p_expression_term(p):
    'expression : term'
    p[0] = p[1]

def p_expression_binop1(p):
    '''term : term TIMES factor
            | term DIVIDE factor
            | factor POWER term
            | term DIV factor
            | term MOD factor'''
    p[0] = BopNode(p[2], p[1], p[3])

def p_term_factor(p):
    'term : factor'
    p[0] = p[1]

def p_factor_group(p):
    'factor : LPAREN expression RPAREN'
    p[0] = p[2]

def p_factor_number(p):
    'factor : NUMBER'
    p[0] = p[1]

def p_factor_uminus(p):
    'factor : MINUS factor %prec UMINUS'
    p[0] = UnaryNode(p[2])

#List
#Crate List
def p_expression_list(p):
    'expression : list'
    p[0] = p[1]

def p_list0(p):
    'list : LBRACKET tail'
    p[0] = list()
    p[0] = BlankListNode(p[0])

def p_list1(p):
    'list : LBRACKET expression tail'
    p[0] = [p[2].evaluate()] + p[3]
    p[0] = ListNode(p[0])

def p_list2(p):
    'tail : COMMA expression tail'
    p[0] = [p[2].evaluate()] + p[3]


def p_tail(p):
    'tail : RBRACKET'
    p[0] = list()

#List Concatenation
def p_list_concat(p):
    'list : list PLUS list'
    p[0] = ListConcatNode(p[2], p[1], p[3])

#List Indexing

def p_list_index0(p):
    'expression : list LBRACKET expression RBRACKET'
    p[0] = IndexNodeL(p[1], p[3])

def p_list_index1(p):
    'list : list LBRACKET expression RBRACKET'
    p[0] = IndexNodeL(p[1], p[3])

#String rules
def p_expression_string(p):
    'expression : string'
    p[0] = p[1]

def p_string_s(p):
    'string : STRING'
    p[0] = p[1]

#String concatenation
def p_string_concatenation(p):
    'expression : string PLUS expression'
    p[0] = StringConcatNode(p[2], p[1], p[3])

#String indexing
def p_string_index(p):
    'expression : string LBRACKET expression RBRACKET'
    p[0] = IndexNodeS(p[1], p[3])


#Boolean in rule
def p_boolean_in(p):
    'boolean : expression IN expression'
    p[0] = BooleanInNode(p[2], p[1], p[3])


#Element concatenation (e::list)
def p_expression_concat(p):
    'list : expression CONCAT list'
    p[0] = ElementConcatNode(p[2], p[1], p[3])


#Comparison (numbers and strings)
def p_compare(p):
    '''boolean : expression LT expression
               | expression LE expression
               | expression EQUALS expression
               | expression NE expression
               | expression GT expression
               | expression GE expression'''
    p[0] = BooleanNode1(p[2], p[1], p[3])


#boolean not, and, or
def p_expression_boolean(p):
    'expression : boolean'
    p[0] = p[1]

def p_boolean(p):
    'boolean : BOOLEAN'
    p[0] = p[1]

def p_boolean_not(p):
    'expression : NOT expression'
    p[0] = BooleanNotNode(p[2])

def p_boolean_op(p):
    '''expression : expression AND expression
                  | expression OR expression'''
    p[0] = BooleanNode2(p[2], p[1], p[3])

def p_error(p):
#    print("Syntax error at '%s'" % p.value)
    p.parser.skip(1)


import ply.yacc as yacc
yacc.yacc()
import sys

if (len(sys.argv) != 2):
    sys.exit("invalid arguments")
fd = open(sys.argv[1], 'r')
code = ""

for line in fd:
    code = line.strip()
    try:
        lex.input(code)
    except Exception:
        print('SYNTAX ERROR')
        continue
    if not line.strip():
        continue
    try:
        ast = yacc.parse(code)
    except Exception:
        print('SYNTAX ERROR')
        continue
    try:
        ast.execute()
    except Exception:
        print('SEMANTIC ERROR')

'''
sys.path.insert(0, "../..")

if sys.version_info[0] >= 3:
    raw_input = input

while 1:
    try:
        s = raw_input('calc > ')
    except EOFError:
        break
    if not s:
        continue
    ast=yacc.parse(s)
    ast.execute()
'''
