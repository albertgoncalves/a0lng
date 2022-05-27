# a0lng

Small toy interpreter with vaguely ML-style syntax. Supports closures, nested functions, currying, objects, and mutual recursion. No type-checking implemented for now, but that would be nice one day.

Needed things
---
* [Nix](https://nixos.org/download.html)

Quick start
---
```console
$ cd path/to/a0lng
$ nix-shell
[nix-shell:path/to/a0lng]$ ./scripts/build.sh       # build interpreter
[nix-shell:path/to/a0lng]$ ./scripts/test.py        # run tests
[nix-shell:path/to/a0lng]$ cat > script.a0 << EOF   # create example script
> fib := (\n ->
>     return 0 if n == 0;
>     return 1 if n == 1;
>     fib (n - 2) + fib (n - 1)
> );
>
> print (toString (fib 9));
> print "\n"
> EOF
[nix-shell:path/to/a0lng]$ ./bin/main script.a0     # run program
34
```

---
Head over to `path/to/a0lng/ex` for more example programs!
