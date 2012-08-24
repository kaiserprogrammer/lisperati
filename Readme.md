# lisperati

is a fast templating engine

You can either inline templates in your code or you can compile and reuse the templates when rendering.

## Inlining

When inlining you can use the variables in lexical scope.

```lisp
(let ((count 1))
  (inline-template "count is (=count)"))
;; => "count is 1"
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