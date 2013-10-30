import re
from ply import yacc
from zlexer import ZLexer
import zast as ast
from plyparser import PLYParser, Coord, ParseError
from zgenerator import *
import pdb

class ZParser(PLYParser):

    def __init__(self):
        self.lexer = ZLexer(self._push_scope,self._pop_scope)
        self.lexer.build()
        self.gen = ZGenerator()
        self.tokens = self.lexer.tokens
        self.parser = yacc.yacc(module=self,start="program")

        self._scope_stack = [dict()]

    def _push_scope(self):
        self._scope_stack.append(dict())

    def _pop_scope(self):
        self._scope_stack.pop()

    def _insert_symbol_table(self,name,value):
        if 'symbol' in self._scope_stack[-1]:
            self._scope_stack[-1]['symbol'][name] = value
        else:
            self._scope_stack[-1]['symbol'] = {name:value}

    def p_program(self,p):
        """
            program  :  file_input_star ENDMARKER
        """
        p[0] = p[1]

    def p_file_input_star_1(self,p):
        """
            file_input_star : NEWLINE
        """
        pass

    def p_file_input_star_2(self,p):
        """
            file_input_star : stmt
        """
        p[0] = ast.Program(stmts=p[1])

    def p_file_input_star_3(self,p):
        """
            file_input_star : file_input_star stmt
        """
        p[0] = p[1].add(p[2])


    def p_stmt_3(self,p):
        """
            stmt : single_stmt 
        """
        p[0] = ast.StmtList(p[1])

    def p_stmt_4(self,p):
        """
            stmt : compound_stmt
        """
        p[0] = ast.StmtList(p[1])

    def p_stmt_1(self,p):
        """
            stmt    : stmt single_stmt
        """
        p[0] = p[1].add(p[2])

    def p_stmt_2(self,p):
        """
            stmt    : stmt compound_stmt 
        """
        p[0] = p[1].add(p[2])


    def p_single_stmt_1(self,p):
        """
            single_stmt     : complex_stmt
        """
        p[0] = p[1]

    def p_single_stmt_2(self,p):
        """
            single_stmt :    small_stmt NEWLINE
        """
        p[0] = p[1]

    def p_small_stmt_1(self,p):
        """
            small_stmt : small_assign_stmt
        """
        p[0] = p[1]

    def p_small_stmt_2(self,p):
        """
            small_stmt : trailer_item
        """
        if not (p[1].v2 is not None and p[1].v2.ts[-1].op == '('):
            self._p_error("small_stmt is not func_call")
        else:
            p[0] = ast.Stmt('fcall',p[1])

    def p_small_stmt_3(self,p):
        """
            small_stmt : return_stmt
        """
        p[0] = p[1]

    def p_small_stmt_4(self,p):
        """
            small_stmt : print_stmt
        """
        p[0] = p[1]

    def p_small_stmt_5(self,p):
        """
            small_stmt : break_stmt
        """
        p[0] = p[1]

    def p_small_stmt_6(self,p):
        """
            small_stmt : continue_stmt
        """
        p[0] = p[1]

    def p_return_stmt(self,p):
        """
            return_stmt : RETURN small_expression
        """
        p[0] = ast.Stmt('return',p[2])

    def p_break_stmt(self,p):
        """
             break_stmt : BREAK
        """
        p[0] = ast.Stmt('break')

    def p_continue_stmt(self,p):
        """
            continue_stmt : CONTINUE
        """
        p[0] = ast.Stmt('continue')

    def p_paramlist(self,p):
        """
            paramlist : paramlist COMMA  param
        """
        p[0] = p[1].add(p[3])

    def p_paramlist_2(self,p):
        """
            paramlist :  param
        """
        p[0] = ast.ParamList(p[1])

    def p_param_1(self,p):
        """
            param   : NAME EQUAL constants
        """
        p[0] = ast.Param('value',p[1],p[3])

    def p_param_2(self,p):
        """
            param : NAME 
        """
        p[0] = ast.Param('default',p[1])

    def p_param_3(self,p):
        """
            param : constants
        """
        p[0] = ast.Param('direct',p[1])

    def p_callparamlist_1(self,p):
        """
            callparamlist : callparamlist COMMA callparam
        """
        p[0] = p[1].add(p[3])

    def p_callparamlist_2(self,p):
        """
            callparamlist :  callparam
        """
        p[0] = ast.ParamList(p[1])

    def p_callparam_1(self,p):
        """
            callparam   : NAME EQUAL arith_expression
        """
        p[0] = ast.Param('value',p[1],p[3])

    def p_callparam_2(self,p):
        """
            callparam : arith_expression
        """
        p[0] = ast.Param('default',p[1])

    def p_small_assign_stmt(self,p):
        """
            small_assign_stmt : trailer_item EQUAL small_expression
        """
        p[0] = ast.Stmt('assign',[p[1],p[3]])
        self._insert_symbol_table(self.gen.visit(p[1]),'name')

    def p_small_expression(self,p):
        """
            small_expression : arith_expression 
        """
        p[0] = p[1]

    def p_complex_stmt(self,p):
        """
            complex_stmt : trailer_item EQUAL complex_expression
        """
        p[0] = ast.Stmt('classFunc',[p[1],p[3]])
        self._scope_stack[-1]['type'] = p[3].type

    def p_complex_expression_1(self,p):
        """
            complex_expression : class_def
                                | func_def
        """
        p[0] = p[1]

    def p_func_def_1(self,p):
        """
            func_def : LPAR RPAR ARROW suite
        """
        p[0] = ast.Func(None,p[4])

    def p_func_def_2(self,p):
        """
            func_def : LPAR paramlist RPAR ARROW suite
        """
        p[0] = ast.Func(p[2],p[5])

    def p_class_def_1(self,p):
        """
            class_def : CLASS LPAR RPAR ARROW suite
        """
        p[0] = ast.Class(None,p[5])

    def p_class_def_2(self,p):
        """
            class_def : CLASS LPAR namelist RPAR ARROW suite
        """
        p[0] = ast.Class(p[3],p[6])

    def p_namelist_1(self,p):
        """
            namelist : namelist COMMA NAME
        """
        p[0] = p[1].add(p[3])

    def p_namelist_2(self,p):
        """
            namelist : NAME
        """
        p[0] = ast.NameList(p[1])

    def p_arith_expression(self,p):
        """
            arith_expression    : term PLUS term 
                                | term MINUS term
                                | term
        """
        if len(p) == 2:
            p[0] = p[1]
        else:
            p[0] = ast.Arith(p[2],p[1],p[3])

    def p_term(self,p):
        """
            term : factor STAR factor
                    | factor SLASH factor
                    | factor
        """
        if len(p) == 2:
            p[0] = p[1]
        else:
            p[0] = ast.Arith(p[2],p[1],p[3])

    def p_factor_1(self,p):
        """
            factor : trailer_item
        """
        p[0] = p[1]

    def p_factor_4(self,p):
        """
            factor : LPAR arith_expression RPAR
        """
        p[0] = ast.Arith(p[1],p[2])

    def p_print_stmt(self,p):
        """
            print_stmt : PRINT small_expression
        """
        p[0] = ast.Stmt('print',p[2])

    def p_constants(self,p):
        """
            constants : NUMBER
                        | STRING
        """
        p[0] = ast.Const(p[1])

    def p_compound_stmt(self,p):
        """
            compound_stmt   : if_stmt
                            | for_stmt
                            | while_stmt
        """
        p[0] = p[1]

    def p_if_stmt_1(self,p):
        """
            if_stmt : IF test COLON suite
        """
        p[0] = ast.Stmt('if',[p[2],p[4]])

    def p_if_stmt_2(self,p):
        """ 
            if_stmt : IF test COLON suite NEWLINE ELSE suite
        """
        p[0] = ast.Stmt('ifelse',[p[2],p[4],p[7]])

    def p_while_stmt(self,p):
        """
            while_stmt : WHILE test COLON suite
        """
        p[0] = ast.Stmt('while',[p[2],p[4]])

    def p_for_stmt(self,p):
        """ 
            for_stmt : FOR NAME IN small_expression COLON suite
        """
        p[0] = ast.Stmt('for',[p[2],p[4],p[6]])

    def p_suite(self,p):
        """
            suite : NEWLINE INDENT stmt DEDENT
        """
        p[0] = ast.Suite(p[3])

    def p_test(self,p):
        """
            test : test_add AND test_add
                    | test_add
        """
        if len(p) == 2:
            p[0] = p[1]            
        else:
            p[0] = ast.Test('and',p[1],p[3])

    def p_test_add(self,p):
        """
            test_add : test_or OR test_or
                    | test_or
        """
        if len(p) == 2:
            p[0] = p[1]
        else:
            p[0] = ast.Test('or',p[1],p[3])

    def p_test_or(self,p):
        """
            test_or : test_not
                    | test_factor
        """
        p[0] = p[1]

    def p_test_not(self,p):
        """
            test_not : NOT test_factor
        """
        p[0] = ast.Test('not',p[2])

    def p_test_factor_1(self,p):
        """
            test_factor : arith_expression cmp_op arith_expression
        """
        p[0] = ast.Test(p[2],p[1],p[3])

    def p_test_factor_2(self,p):
        """
            test_factor : arith_expression
        """
        p[0] = ast.Test('direct',p[1])

    def p_cmp_op(self,p):
        """
            cmp_op  : EQEQUAL 
                    | NOTEQUAL 
                    | LESSEQUAL 
                    | GREATEREQUAL 
                    | LESS 
                    | GREATER
        """
        p[0] = p[1]

    def p_trailer_0(self,p):
        """
            trailer : DOT NAME 
        """
        p[0] = ast.Trailer('.',p[2])

    def p_trailer_1(self,p):
        """
            trailer : LSQB subscript RSQB
        """
        p[0] = ast.Trailer('[',p[2])

    def p_trailer_2(self,p):
        """
            trailer : LPAR RPAR
        """
        p[0] = ast.Trailer('(',None)

    def p_trailer_3(self,p):
        """
            trailer : LPAR callparamlist RPAR
        """
        p[0] = ast.Trailer('(',p[2])

    def p_trailer_star_1(self,p):
        """
            trailer_star : trailer
        """
        p[0] = ast.TrailerList(p[1])

    def p_trailer_star_2(self,p):
        """
            trailer_star : trailer_star trailer
        """
        p[0] = p[1].add(p[2])

    def p_trailer_item_1(self,p):
        """
            trailer_item : atom
        """
        p[0] = ast.Item('direct',p[1])

    def p_trailer_item_2(self,p):
        """
            trailer_item : atom trailer_star
        """
        p[0] = ast.Item('trailer',p[1],p[2])

    def p_atom_0(self,p):
        """
            atom : AT NAME
        """
        p[0] = ast.Atom('@',p[2])

    def p_atom_1(self,p):
        """
            atom : NAME
        """
        p[0] = ast.Atom('name',p[1])

    def p_atom_2(self,p):
        """
            atom : constants
        """
        p[0] = ast.Atom('constants',p[1])

    def p_subscript(self,p):
        """
            subscript : NUMBER 
        """
        p[0] = ast.Const(p[1])

    def p_empty(self,p):
        """
            empty : 
        """

    def p_error(self,p):
        if p:
            self._parse_error(
                'before: %s' % p.value,
                '')
        else:
            self._parse_error('At end of input', '')


    def parse(self,source, filename="<string>"):
        # There is a bug in PLY 2.3; it doesn't like the empty string.
        # Bug reported and will be fixed for 2.4.
        # http://groups.google.com/group/ply-hack/msg/cbbfc50837818a29
        if not source:
            source = "\n" 
        try: 
            parse_tree = self.parser.parse(source, lexer=self.lexer,debug=2)
        except SyntaxError, err: 
            # Insert the missing data and reraise
            assert hasattr(err, "lineno"), "SytaxError is missing lineno"
            geek_lineno = err.lineno - 1
            start_of_line = lexer.lexer.line_offsets[geek_lineno]
            end_of_line = lexer.lexer.line_offsets[geek_lineno+1]-1
            text = source[start_of_line:end_of_line]
            err.filename = filename
            err.text = text 
            raise
        return parse_tree
#------------------------------------------------------------------------------
if __name__ == "__main__":
    import pprint
    import time, sys

    z = ZParser()
    g = ZGenerator()
    fname = 'test/test.z'
    fp = open(fname,'r')
    t = z.parse(fp.read(),fname)
    out = g.visit(t)
    print out
    fp.close()
