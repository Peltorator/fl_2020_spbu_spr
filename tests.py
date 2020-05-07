import parser
import pytest

def test_easy():
    assert parser.parser.parse("S -> a\n") == [("S", ["a"])]

def test_eps():
    assert parser.parser.parse("S -> @\n") == [("S", [])]

def test_nonterm():
    assert parser.parser.parse("S -> ABACABA\nABACABA -> a\n") == [("S", ["ABACABA"]), ("ABACABA", ["a"])]

def test_many():
    assert parser.parser.parse("S -> @,a,  T, @, x, T, T, Q\nQ -> @\nT -> a,a,a,T,Q,T,a\n") == [("S", ["a", "T", "x", "T", "T", "Q"]), ("Q", []), ("T", ["a", "a", "a", "T", "Q", "T", "a"])]


if __name__ == "__main__":
    pytest.main([__file__])
