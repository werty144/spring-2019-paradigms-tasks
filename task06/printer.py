from model import *


def place_semicolon_ifneeded(s):
    if not s.endswith(('}\n', ';\n', '}', ';')):
        s += ';'
    return s


class PrettyPrinter(ASTNodeVisitor):
    def __init__(self, indentation_level=0):
        self.indentation_level = indentation_level

    def string_with_indentation(self, s):
        return '\t' * self.indentation_level + s

    def pretty_code(self, program):
        s = program.accept(self)
        s = place_semicolon_ifneeded(s)
        return s

    def visit_number(self, node):
        return str(node.value)

    def visit_reference(self, node):
        return node.name

    def visit_function_definition(self, node):
        function = node.function
        s = "def " + node.name + '('
        s += ', '.join(function.args)
        s += ") {\n"
        self.indentation_level += 1
        for statement in function.body:
            s += self.string_with_indentation(statement.accept(self)) + '\n'
        s += "}"
        self.indentation_level -= 1
        return s

    def visit_conditional(self, node):
        s = "if (" + node.condition.accept(self) + ") {\n"
        self.indentation_level += 1
        if node.if_true:
            for command in node.if_true:
                s += self.string_with_indentation(command.accept(self))
                s = place_semicolon_ifneeded(s)
                s += '\n'
        self.indentation_level -= 1
        s += self.string_with_indentation('}')
        if node.if_false:
            s += " else {\n"
            self.indentation_level += 1
            for command in node.if_false:
                s += self.string_with_indentation(command.accept(self))
                s = place_semicolon_ifneeded(s)
                s += '\n'
            self.indentation_level -= 1
            s += self.string_with_indentation('}')
        return s

    def visit_print(self, node):
        return "print " + node.expr.accept(self) + ";"

    def visit_read(self, node):
        return "read " + node.name + ";"

    def visit_binary_operation(self, node):
        return '(' + node.lhs.accept(self) + ") " + \
               node.op + " (" + node.rhs.accept(self) + ")"

    def visit_unary_operation(self, node):
        return '(' + node.op + '(' + node.expr.accept(self) + "))"

    def visit_function_call(self, node):
        expr = node.fun_expr.accept(self)
        s = expr + '('
        s += ', '.join(map(lambda x: x.accept(self), node.args))
        s += ")"
        return s


def pretty_print(program):
    printer = PrettyPrinter()
    s = printer.pretty_code(program)
    print(s)
