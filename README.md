TBD

Compiling:

```
fswatch -o code/  | xargs -n1 -I{} elm make code/maps/*.elm code/Main.elm --output elm.js
```
