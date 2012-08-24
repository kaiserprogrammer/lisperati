# lisperati

is a fast templating engine

You can either inline templates in your code or you can compile and reuse the templates when rendering.

## Inlining

When inlining you can use the variables in lexical scope.

```lisp
(let ((count 1))
  (inline-template "count is (=(float count))"))
;; => "count is 1.0"
```

You could also inline a file template.

```lisp
(let ((count 1))
  (inline-file-template "/path/to/file/"))
```

## Compiling

When compiling you can only use special variables most of the time denoted with \*earmuffs\*. When a template is compiled you can render it with render-template.

```lisp
(defvar *count*) ;; declare special variable

(let ((compiled-template (compile-template "count is (=*count*)")))
  (let ((*count* 1))
    (render-template compiled-template)))
;; => "count is 1"
```

You can also compile file templates, again here you can only refer to special variables

```lisp
(defvar *count*)

(let ((*count* 1)
      (compiled-template (compile-file-template "/path/to/file/")))
  (render-template compiled-template))
```

## Templating

Templating works in to ways you can either insert the result of the expression with "(=expr)" or have a more advanced control-structure with "(expr)"

### (=expr)

```lisp
(let ((count 0))
  (inline-template "i have (=count) cat(=(if (= 1 count) \"\" \"s\"))"))
;; => "i have 0 cats"
```

### (expr) and with-template

```lisp
(let ((count 0))
  (lisperati:inline-template
"i have ((with-template (=count)))  cat((if (not (= 1 count)) (with-template s)))"))
;; => "i have 0 cats"
```

When using (expr) only the text following after **with-template** will be inserted.
Notice how you can freely mix (=expr), (expr) and with-template, which is also a means of escaping