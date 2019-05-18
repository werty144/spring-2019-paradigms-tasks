#!/usr/bin/env python3
import pytest
from printer import *


def test_conditional():
    printer = PrettyPrinter()
    actual = printer.prettify_code(Conditional(Number(42), [], []))
    assert actual == 'if (42) {\n}'


def test_function_definition():
    printer = PrettyPrinter()
    actual = printer.prettify_code(FunctionDefinition("foo", Function([], [])))
    assert actual == 'def foo() {\n}'


def test_print():
    printer = PrettyPrinter()
    actual = printer.prettify_code(Print(Number(42)))
    assert actual == 'print 42;'


def test_read():
    printer = PrettyPrinter()
    actual = printer.prettify_code(Read('book'))
    assert actual == 'read book;'


def test_number():
    printer = PrettyPrinter()
    actual = printer.prettify_code(Number(42))
    assert actual == '42;'


def test_reference():
    printer = PrettyPrinter()
    actual = printer.prettify_code(Reference('otsilochka'))
    assert actual == 'otsilochka;'


def test_bin_operation():
    printer = PrettyPrinter()
    add = BinaryOperation(Number(2), '+', Number(3))
    mul = BinaryOperation(Number(1), '*', add)
    actual = printer.prettify_code(mul)
    assert actual == '(1) * ((2) + (3));'


def test_un_operation():
    printer = PrettyPrinter()
    actual = printer.prettify_code(UnaryOperation('!', Number(42)))
    assert actual == '(!(42));'


def test_function_call():
    printer = PrettyPrinter()
    actual = printer.prettify_code(FunctionCall(Reference('foo'),
                                                [Number(1),
                                                 Number(2), Number(3)]))
    assert actual == 'foo(1, 2, 3);'


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
        '\tread x;\n' + \
        '\tprint x;\n' + \
        '\tif ((2) == (3)) {\n' + \
        '\t\tif (1) {\n' + \
        '\t\t}\n' + \
        '\t} else {\n' + \
        '\t\texit((-(arg1)));\n' + \
        '\t}\n' + \
        '}\n'


if __name__ == "__main__":
    pytest.main()
