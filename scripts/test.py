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

    def test_closure(self):
        self.into_test("closure", 2)

    def test_curry(self):
        self.into_test("curry", 1)

    def test_fib_loop(self):
        self.into_test("fib_loop", 6766)

    def test_fib_rec(self):
        self.into_test("fib_rec", 13)

    def test_if(self):
        self.into_test("if", -3)


if __name__ == "__main__":
    main()

