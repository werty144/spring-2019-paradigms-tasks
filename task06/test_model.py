import pytest
from model import *


def test_sample():
    a, b, c = object(), object(), object()
    parent = Scope()
    parent['foo'] = a
    parent['bar'] = b
    scope = Scope(parent)
    ans = scope['bar']
    assert ans == b
    ans = parent['bar']
    assert ans == b
    scope['bar'] = c
    ans = scope['bar']
    assert ans == c
    ans = parent['bar']
    assert ans == b
    ans = scope['foo']
    assert ans == a


def test_recursion():
    a = object()
    grandpa = Scope()
    dad = Scope(grandpa)
    son = Scope(dad)
    grandpa["Jit'"] = a
    ans = son["Jit'"]
    assert ans == a


def test_exception():
    a = object()
    scope = Scope()
    scope['v epohu'] = a
    with pytest.raises(KeyError):
        ans = scope['sversheniy']


def test_print(capsys):
    scope = Scope()
    value = Print(Number(42))
    value.evaluate(scope)
    captured = capsys.readouterr()
    assert captured.out == '42\n'


def test_read(monkeypatch):
    scope = Scope()
    r = Read('imeya')
    monkeypatch.setattr('builtins.input', lambda: '144')
    num = r.evaluate(scope)
    assert num.value == 144
    assert scope['imeya']


def test_function_definition():
    scope = Scope()
    f = FunctionDefinition('my func', Function(['variable'],
                                               [
                                                   Print(Reference
                                                         ('variable')),
                                               ]
                                               ))
    f.evaluate(scope)
    assert scope['my func']


def test_function_call(capsys):
    operation1 = FunctionDefinition(
        'vozvishenniy',
        Function(['param1', 'param2'],
                 [
                     Print(BinaryOperation(Reference('param1'),
                                           '+', Reference('param2'))),
                 ]
                 )
    )
    operation2 = FunctionCall(Reference('vozvishenniy'), [
        Number(1),
        BinaryOperation(Number(2), "+", Number(3))
    ])
    s = Scope()
    operation1.evaluate(s)
    operation2.evaluate(s)
    assert capsys.readouterr().out == '6\n'


def test_function_sample(capsys):
    operation1 = FunctionDefinition("foo", Function(["a", "b"],
                                                    [
                                                        Print(
                                                            BinaryOperation
                                                            (Reference("a"),
                                                             "+",
                                                             Reference(
                                                                 "b"))),
                                                    ]
                                                    ))
    operation2 = FunctionCall(Reference("foo"), [
        Number(1),
        BinaryOperation(Number(2), "+", Number(3))
    ])
    s = Scope()
    operation1.evaluate(s)
    operation2.evaluate(s)
    assert capsys.readouterr().out == '6\n'


def test_bin_operations():
    a = Number(10)
    b = Number(15)
    scope = Scope()
    assert BinaryOperation(a, '+', b).evaluate(scope).value == 25
    assert BinaryOperation(a, '*', b).evaluate(scope).value == 150
    assert BinaryOperation(a, '-', b).evaluate(scope).value == -5
    assert BinaryOperation(a, '&&', b).evaluate(scope).value == 15
    assert BinaryOperation(a, '//', b).evaluate(scope).value == 0
    assert BinaryOperation(b, '//', a).evaluate(scope).value == 1
    a = Number(0)
    assert not BinaryOperation(a, '&&', b).evaluate(scope).value
    assert BinaryOperation(a, '||', b).evaluate(scope).value
    assert not BinaryOperation(a, '>', b).evaluate(scope).value


def test_true_num():
    with pytest.raises(TypeError):
        a = Number(True)


def test_unary_operation():
    fml = Number(366)
    scope = Scope()
    assert UnaryOperation('-', fml).evaluate(scope).value == -366
    red = Number(0)
    assert UnaryOperation('!', red).evaluate(scope).value


def test_reference():
    scope = Scope()
    scope['nrav'] = -5
    assert Reference('nrav').evaluate(scope) == -5


def test_conditional():
    conditional = Conditional(BinaryOperation(Number(0), '>', Number(1)),
                              [Number(12321)], [Number(322)])
    scope = Scope()
    res = conditional.evaluate(scope)
    assert res.value == 322
    conditional = Conditional(BinaryOperation(Number(1), '<', Number(0)),
                              [Number(1), Number(2)])
    res = conditional.evaluate(scope)
    assert res.value == 0


def test_factorial(capsys):
    func = FunctionDefinition(
        'factorial',
        Function(['num'],
                 [
                     Conditional(BinaryOperation(Reference('num'), '>',
                                                 Number(1)),
                                 [BinaryOperation(
                                     FunctionCall(
                                         Reference('factorial'),
                                         [BinaryOperation(Reference('num'),
                                                          '-', Number(1))]),
                                     '*', Reference('num'))
                                  ],
                                 [Number(1)]
                                 )
                 ]
                 )
    )

    scope = Scope()
    func_call = FunctionCall(Reference('factorial'), [Number(7)])
    func.evaluate(scope)
    ret = func_call.evaluate(scope)
    p = Print(Number(ret.value))
    p.evaluate(scope)
    captured = capsys.readouterr()
    assert captured.out == '5040\n'


if __name__ == "__main__":
    pytest.main()
