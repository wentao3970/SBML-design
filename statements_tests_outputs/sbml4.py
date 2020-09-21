#Student Name:Wentao Wu; Student ID#:112524704

class Node:
    def __init__(self):
        print("init node")

    def evaluate(self):
        return 0

    def execute(self):
        return 0

class BlockNode(Node):
    def __init__(self,sl):
        self.statementList = sl

    def evaluate(self):
         for statement in self.statementList:
             statement.evaluate()

class AssignNode1(Node):
    def __init__(self, v1, v2):
        self.name = v1
        self.value = v2

    def evaluate(self):
        if self.name in d:
            if self.value in d:
                d[self.name] = d[self.value]
            else:
                d[self.name] = self.value.evaluate()
        else:
            if self.value in d:
                d[self.name.evaluate()] = d[self.value]
            else:
                d[self.name.evaluate()] = self.value.evaluate()


class AssignNode2(Node):
    def __init__(self, v1, i, v2):
        self.name = v1
        self.index = i
        self.value = v2

    def evaluate(self):
        if self.name in d:
            name = self.name
            if self.index in d:
                index = d[self.index]
                if self.value in d:
                    value = d[self.value]
                else:
                    value = self.value.evaluate()
            else:
                index = self.index.evaluate()
                if self.value in d:
                    value = d[self.value]
                else:
                    value = self.value.evaluate()
            d[name][index] = value
        else:
            print('SEMANTIC ERROR')


class IfNode(Node):
    def __init__(self, v1, v2):
        self.boolean = v1
        self.block = v2

    def evaluate(self):
        if self.boolean in d:
            if d[self.boolean]:
                self.block.evaluate()
            else:
                pass
        else:
            if self.boolean.evaluate():
                self.block.evaluate()
            else:
                pass

class IfElseNode(Node):
    def __init__(self, v1, v2, v3):
        self.boolean = v1
        self.block1 = v2
        self.block2 = v3

    def evaluate(self):
        if self.boolean in d:
            if d[self.boolean]:
                self.block1.evaluate()
            else:
                self.block2.evaluate()
        else:
            if self.boolean.evaluate():
                self.block1.evaluate()
            else:
                self.block2.evaluate()

class WhileNode(Node):
    def __init__(self, v1, v2):
        self.boolean = v1
        self.block = v2

    def evaluate(self):
        if self.boolean in d:
            while d[self.boolean]:
                self.block.evaluate()
        else:
            while self.boolean.evaluate():
                self.block.evaluate()

class PrintNode(Node):
    def __init__(self, e):
        self.e = e

    def evaluate(self):
        if self.e in d:
            print (d[self.e])
        else:
            print(self.e.evaluate())

class EmptyNode(Node):
    def __init__(self, v):
        self.v = v

    def evaluate(self):
        return None

class VariableNode(Node):
    def __init__(self, k):
        self.name = k

    def evaluate(self):
        return self.name

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
        i=0
        n = []
        while i<len(self.value):
            if self.value[i] in d:
                n = n + [d[self.value[i]]]
            else:
                n =  n + [self.value[i].evaluate()]
            i = i + 1
        return n

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
        self.v1 = v1
        self.v2 = v2

    def evaluate(self):
        if self.v1 in d:
            v1 = d[self.v1]
            if self.v2 in d:
                v2 = d[self.v2]
            else:
                v2 = self.v2.evaluate()
        else:
            v1 = self.v1.evaluate()
            if self.v2 in d:
                v2 = d[self.v2]
            else:
                v2 = self.v2.evaluate()
        return v1[v2]

class IndexNodeS(Node):
    def __init__(self, v1, v2):
        self.v1 = v1
        self.v2 = v2

    def evaluate(self):
        if self.v1 in d:
            v1 = d[self.v1]
            if self.v2 in d:
                v2 = d[self.v2]
            else:
                v2 = self.v2.evaluate()
        else:
            v1 = self.v1.evaluate()
            if self.v2 in d:
                v2 = d[self.v2]
            else:
                v2 = self.v2.evaluate()
            return v1[v2]

class BopNode(Node):
    def __init__(self, op, v1, v2):
        self.v1 = v1
        self.v2 = v2
        self.op = op

    def evaluate(self):
        if self.v1 in d :
            v1 = d[self.v1]
            if self.v2 in d:
                v2 = d[self.v2]
            else:
                v2 = self.v2.evaluate()
        else:
            v1 = self.v1.evaluate()
            if self.v2 in d:
                v2 = d[self.v2]
            else:
                v2 = self.v2.evaluate()

        if (self.op == '+'):
            return v1 + v2
        elif (self.op == '-'):
            return v1 - v2
        elif (self.op == '*'):
            return v1 * v2
        elif (self.op == '/'):
            return v1 / v2
        elif (self.op == '**'):
            return v1 ** v2
        elif (self.op == 'div'):
            return v1 // v2
        elif (self.op == 'mod'):
            return v1 % v2

class UnaryNode(Node):
    def __init__(self, v):
        self.v = v

    def evaluate(self):
        return -self.v.evaluate()

class BooleanInNode(Node):
    def __init__(self, op, v1, v2):
        self.v1 = v1
        self.v2 = v2
        self.op = op

    def evaluate(self):
        if self.v1 in d :
            v1 = d[self.v1]
            if self.v2 in d:
                v2 = d[self.v2]
            else:
                v2 = self.v2.evaluate()
        else:
            v1 = self.v1.evaluate()
            if self.v2 in d:
                v2 = d[self.v2]
            else:
                v2 = self.v2.evaluate()
        return v1 in v2

class ElementConcatNode(Node):
    def __init__(self, op, v1, v2):
        self.v1 = v1
        self.v2 = v2
        self.op = op

    def evaluate(self):
        return [self.v1.evaluate()] + self.v2.evaluate()

class StringConcatNode(Node):
    def __init__(self, op, v1, v2):
        self.v1 = v1
        self.v2 = v2
        self.op = op
    def evaluate(self):
        if self.v1 in d :
            v1 = d[self.v1]
            if self.v2 in d:
                v2 = d[self.v2]
            else:
                v2 = self.v2
        else:
            v1 = self.v1
            if self.v2 in d:
                v2 = d[self.v2]
            else:
                v2 = self.v2
        return v1.evaluate() + v2.evaluate()

class ListConcatNode(Node):
    def __init__(self, op, v1, v2):
        self.v1 = v1
        self.v2 = v2
        self.op = op

    def evaluate(self):
        if self.v1 in d :
            v1 = d[self.v1]
            if self.v2 in d:
                v2 = d[self.v2]
            else:
                v2 = self.v2.evaluate()
        else:
            v1 = self.v1.evaluate()
            if self.v2 in d:
                v2 = d[self.v2]
            else:
                v2 = self.v2.evaluate()
        return v1 + v2

class BooleanNode1(Node):
    def __init__(self, op, v1, v2):
        self.v1 = v1
        self.v2 = v2
        self.op = op

    def evaluate(self):
        if self.v1 in d :
            v1 = d[self.v1]
            if self.v2 in d:
                v2 = d[self.v2]
            else:
                v2 = self.v2.evaluate()
        else:
            v1 = self.v1.evaluate()
            if self.v2 in d:
                v2 = d[self.v2]
            else:
                v2 = self.v2.evaluate()

        if (self.op == '<>'):
            return v1 != v2
        elif (self.op == '>'):
            return v1 > v2
        elif (self.op == '<'):
            return v1 < v2
        elif (self.op == '>='):
            return v1 >= v2
        elif (self.op == '<='):
            return v1 <= v2
        elif (self.op == '=='):
            return v1 == v2

class BooleanNotNode(Node):
    def __init__(self, v):
        self.value = v

    def evaluate(self):
        if self.value in d:
            return not d[self.value]
        else:
            return not self.value.evaluate()

class BooleanNode2(Node):
    def __init__(self, op, v1, v2):
        self.v1 = v1
        self.v2 = v2
        self.op = op
    def evaluate(self):
        if self.v1 in d :
            v1 = d[self.v1]
            if self.v2 in d:
                v2 = d[self.v2]
            else:
                v2 = self.v2.evaluate()
        else:
            v1 = self.v1.evaluate()
            if self.v2 in d:
                v2 = d[self.v2]
            else:
                v2 = self.v2.evaluate()

        if (self.op == 'andalso'):
            return v1 and v2
        elif (self.op == 'orelse'):
            return v1 or v2

reserved = {
    'if' : 'IF',
    'else' : 'ELSE',
    'while' : 'WHILE',
    'print' : 'PRINT',
    'div' : 'DIV',
    'mod' : 'MOD',
    'not' : 'NOT',
    'andalso' : 'AND',
    'orelse' : 'OR',
    'in' : 'IN'
 }

tokens = list(reserved.values()) + [
    'NUMBER', 'STRING',
    'PLUS', 'MINUS', 'TIMES', 'DIVIDE', 'POWER', 'CONCAT',
    'EQUALS', 'LT', 'LE', 'GT', 'GE', 'NE', 'BOOLEAN',
    'LPAREN', 'RPAREN', 'LBRACKET', 'RBRACKET', 'LCURLY', 'RCURLY',
    'COMMA', 'SEMICOLON', 'TUPLEINDEX',
    'ID', 'ASSIGN']

t_PLUS = r'\+'
t_MINUS = r'-'
t_TIMES = r'\*'
t_DIVIDE = r'/'
t_POWER   =r'\*\*'
t_CONCAT = r'\:\:'

t_NE = r'<>'
t_EQUALS  = r'=='
t_LT = r'<'
t_LE = r'<='
t_GT = r'>'
t_GE = r'>='

t_LPAREN = r'\('
t_RPAREN = r'\)'
t_LBRACKET = r'\['
t_RBRACKET = r'\]'
t_LCURLY = r'{'
t_RCURLY = r'}'

t_COMMA = r'\,'
t_SEMICOLON = r';'
t_TUPLEINDEX = r'\#'

t_ASSIGN = '='

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

def t_ID(t):
     r'[a-zA-Z_][a-zA-Z_0-9]*'
     t.type = reserved.get(t.value,'ID')
     return t

# Ignored characters
t_ignore = " \t"

def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")

def t_error(t):
#    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(0)

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

def p_block(p):
    'block : LCURLY statement_list RCURLY'
    p[0] = BlockNode(p[2])

def p_statement_list(p):
    'statement_list : statement_list statement'
    p[0] = p[1] + [p[2]]

def p_statement_list_val(p):
    '''statement_list : statement'''
    p[0] = [p[1]]

def p_statement(p):
    '''statement : assign_statement
                 | print_statement
                 | conditional_statement
                 | while_statement
                 | empty_statement
                 | block'''
    p[0] = p[1]

# dictionary of variables
d = {}
def p_variable_ID(p):
    'variable : ID'
    p[0] = VariableNode(p[1])

def p_assign_statement1(p):
    'assign_statement : variable ASSIGN expression SEMICOLON'
    p[0] = AssignNode1(p[1], p[3])

def p_assign_statement2(p):
    'assign_statement : ID LBRACKET expression RBRACKET ASSIGN expression SEMICOLON'
    p[0] = AssignNode2(p[1], p[3], p[6])


#Print statement
def p_print_statement(p):
    '''print_statement : PRINT LPAREN expression RPAREN SEMICOLON'''
    p[0] = PrintNode(p[3])


#conditional statements, including if and ifelse statements
def p_conditional_statement(p):
    '''conditional_statement : if_statement
                             | if_else_statement'''
    p[0] = p[1]

def p_if_statement(p):
    'if_statement : IF LPAREN expression RPAREN block'
    p[0] = IfNode(p[3], p[5])

def p_if_else_statement(p):
    'if_else_statement : IF LPAREN expression RPAREN block ELSE block'
    p[0] = IfElseNode(p[3], p[5], p[7])

def p_while_statement(p):
    'while_statement : WHILE LPAREN expression RPAREN block'
    p[0] = WhileNode(p[3], p[5])

def p_empty_statement(p):
    'empty_statement : '
    p[0] = EmptyNode(None)

#ID reductions
def p_expression_variable(p):
    '''factor     : ID
       list       : ID
       string     : ID
       expression : ID'''
    p[0] = p[1]

#Parenthesized expression
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


#List
#List Creation
def p_expression_list(p):
    'expression : list'
    p[0] = p[1]

def p_list0(p):
    'list : LBRACKET tail'
    p[0] = list()
    p[0] = BlankListNode(p[0])

def p_list1(p):
    'list : LBRACKET expression tail'
    p[0] = [p[2]] + p[3]
    p[0] = ListNode(p[0])

def p_list2(p):
    'tail : COMMA expression tail'
    p[0] = [p[2]] + p[3]

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


#Boolean_in rule
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
yacc.yacc(debug=0)
import sys

with open(sys.argv[1], 'r') as myfile:
    data = myfile.read().replace('\n', '')

try:
    lex.input(data)
except Exception:
    print('SYNTAX ERROR')
    sys.exit()
try:
    root = yacc.parse(data)
except Exception:
    print('SYNTAX ERROR')
    sys.exit()
try:
    root.evaluate()
except Exception:
    print('SEMANTIC ERROR')
    sys.exit()
