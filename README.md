# cl-flashfill
### Vitor Santos <vhsoo at proton dot me>

This is a project to do ... something.

## Demo

```lisp
(synthesize '(("Camila Mino" . "C. Mino")
              ("John Smith" . "J. Smith")
              ("Hugo Santos" . "H. Santos")))
```

synthesizes:

```lisp
(CONCAT (CONCAT (SUB-STR 0 1) (LITERAL "."))
        (CONCAT (LITERAL " ") (SPLIT-IDX " " 1)))
```

i.e. "take the first letter, add '. ', append the second word" — found at
depth 3 in under a tenth of a second.

## License

MIT

