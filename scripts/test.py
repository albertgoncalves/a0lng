#!/usr/bin/env python3

from unittest import main, TestCase
from subprocess import run

__unittest = True


class Tests(TestCase):
    def into_test(self, file, expected):
        result = run(
            f"\"$WD\"/bin/main \"$WD\"/ex/{file}.a0".encode(),
            capture_output=True,
            shell=True,
        )
        self.assertEqual(result.returncode, 0)
        self.assertEqual(result.stdout.decode(), f"{expected}\n")

    def test_ackermann_peter(self):
        self.into_test("ackermann_peter", 7)

    def test_another_closure(self):
        self.into_test("another_closure", -1)

    def test_closure(self):
        self.into_test("closure", 2)

    def test_curry(self):
        self.into_test("curry", 1)

    def test_fib_lazy(self):
        self.into_test("fib_lazy", 1134903170)

    def test_fib_loop(self):
        self.into_test("fib_loop", -1134903171)

    def test_fib_rec(self):
        self.into_test("fib_rec", 34)

    def test_hello_world(self):
        self.into_test("hello_world", "\"Hello, world!\"")

    def test_if(self):
        self.into_test("if", -3)

    def test_object(self):
        self.into_test("object", 3946)

    def test_return_if(self):
        self.into_test("return_if", -123)


if __name__ == "__main__":
    main()
