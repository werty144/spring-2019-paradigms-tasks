import pytest
from printer import *


def test_conditional():
    printer = PrettyPrinter()
    check = printer.pretty_code(Conditional(Number(42), [], []))
    assert check == 'if (42) {\n}'


def test_function_definition():
    printer = PrettyPrinter()
    check = printer.pretty_code(FunctionDefinition("foo", Function([], [])))
    assert check == 'def foo() {\n}'


def test_print():
    printer = PrettyPrinter()
    check = printer.pretty_code(Print(Number(42)))
    assert check == 'print 42;'


def test_read():
    printer = PrettyPrinter()
    check = printer.pretty_code(Read('book'))
    assert check == 'read book;'


def test_number():
    printer = PrettyPrinter()
    check = printer.pretty_code(Number(42))
    assert check == '42;'


def test_reference():
    printer = PrettyPrinter()
    check = printer.pretty_code(Reference('otsilochka'))
    assert check == 'otsilochka;'


def test_bin_operation():
    printer = PrettyPrinter()
    add = BinaryOperation(Number(2), '+', Number(3))
    mul = BinaryOperation(Number(1), '*', add)
    check = printer.pretty_code(mul)
    assert check == '(1) * ((2) + (3));'


def test_un_operation():
    printer = PrettyPrinter()
    check = printer.pretty_code(UnaryOperation('!', Number(42)))
    assert check == '(!(42));'


def test_function_call():
    printer = PrettyPrinter()
    check = printer.pretty_code(FunctionCall(Reference('foo'),
                                             [Number(1), Number(2), Number(3)]))
    assert check == 'foo(1, 2, 3);'


def test_all(capsys):
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
    assert capsys.readouterr().out == \
        'def main(arg1) {\n' + \
        '  read x;\n' + \
        '  print x;\n' + \
        '  if ((2) == (3)) {\n' + \
        '    if (1) {\n' + \
        '    }\n' + \
        '  } else {\n' + \
        '    exit((-(arg1)));\n' + \
        '  }\n' + \
        '}\n'


if __name__ == "__main__":
    pytest.main()
