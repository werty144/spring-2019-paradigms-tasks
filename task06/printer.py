from model import *


class PrettyPrinter(ASTNodeVisitor):
    def __init__(self, tabs=0):
        self.tabs = tabs

    def pretty_code(self, program):
        s = program.accept(self)
        if not s.endswith(('}\n', ";\n", "}", ';')):
            s += ';'
        return s

    def visit_number(self, node):
        return str(node.value)

    def visit_reference(self, node):
        return node.name

    def visit_function_definition(self, node):
        function = node.function
        s = "def " + node.name + '('
        if len(function.args):
            for arg in function.args:
                s += arg + ', '
            s = s[:-2]
        s += ") {\n"
        self.tabs += 1
        for statement in function.body:
            s += '  ' * self.tabs + statement.accept(self) + '\n'
        s += "}"
        self.tabs -= 1
        return s

    def visit_conditional(self, node):
        s = "if (" + node.condition.accept(self) + ") {\n"
        self.tabs += 1
        for command in node.if_true:
            s += '  ' * self.tabs + command.accept(self)
            if not s.endswith(('}\n', ";\n", "}", ';')):
                s += ';'
            s += '\n'
        self.tabs -= 1
        s += '  ' * self.tabs + '}'
        if node.if_false:
            s += " else {\n"
            self.tabs += 1
            for command in node.if_false:
                s += '  ' * self.tabs + command.accept(self)
                if not s.endswith(('}\n', ";\n", "}", ';')):
                    s += ';'
                s += '\n'
            self.tabs -= 1
            s += '  ' * self.tabs + '}'
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
        if len(node.args):
            for arg in node.args:
                s += arg.accept(self) + ', '
            s = s[:-2]
        s += ")"
        return s


def pretty_print(program):
    pretty = PrettyPrinter()
    s = pretty.pretty_code(program)
    print(s)


def main():
    pretty_print(FunctionDefinition('main', Function(['arg1'], [
        Read('x'),
        Print(Reference('x')),
        Conditional(
            BinaryOperation(Number(2), '==', Number(3)),
            [
                Conditional(Number(1), [], [])
            ],
            [
                FunctionCall(Reference('exit'), [
                    UnaryOperation('-', Reference('arg1'))
                ])
            ],
        ),
    ])))


if __name__ == '__main__':
    main()
