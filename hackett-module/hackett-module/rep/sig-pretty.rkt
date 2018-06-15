#lang racket/base
(provide
 sig->string
 sig->datum)

(require
 racket/list
 racket/pretty
 racket/syntax
 syntax/parse
 syntax/id-table
 (only-in syntax/parse [attribute @])
 hackett/private/typecheck
 "../util/stx.rkt"
 "../util/stx-traverse.rkt"
 "../util/stx-subst.rkt"
 "../namespace/reqprov.rkt"
 (for-template "sig-literals.rkt"
               "reinterpret.rkt"
               (only-in (unmangle-in #:only "../dot/dot-m.rkt")
                        [#%dot #%dot_m])
               (only-in (unmangle-in #:only "../dot/dot-t.rkt")
                        [#%dot #%dot_τ])))


;; Sig -> String
;; Sig Int -> String
(define (sig->string s [indent 0])
  ; explicitly manage port so we can create "virtual" indentation level
  (define port (open-output-string))
  (port-count-lines! port)
  (set-port-next-location! port 1 indent (add1 indent))

  ; render with modified style table to handle Π etc.
  (parameterize ([pretty-print-current-style-table
                  (pretty-print-extend-style-table
                   (pretty-print-current-style-table)
                   '(sig   val    type   Π)
                   '(begin define define let))])
    (pretty-display (sig->datum s) port #:newline? #f))

  (begin0 (get-output-string port)
    (close-output-port port)))


;; Sig -> Any
(define sig->datum
  (syntax-parser
    #:literal-sets [sig-literals]
    [(#%sig ids:hash-literal decls:hash-literal)
     #:do [(define mapping
             (make-immutable-free-id-table
              (map cons (@ ids.values) (map namespaced-symbol (@ ids.keys)))))
           (define decls*
             (for/list ([(k d) (in-hash (@ decls.value))])
               (decl->datum (namespaced-symbol k) (stx-substs d mapping))))]
     `(sig ,@decls*)]

    [(#%pi-sig ([x:id A]) B)
     ;; TODO: we could keep around the original `x` in e.g.
     ;;   a syntax property just for the purpose of pretty printing?
     `(Π ([,(syntax-e #'x)
           ,(sig->datum #'A)])
        ,(sig->datum #'B))]

    [:id (syntax->datum this-syntax)]))

;; Sym Decl -> Any
(define (decl->datum name stx)
  (syntax-parse stx
    #:literal-sets [sig-literals]
    [(#%val-decl t) `(val ,name : ,(type->string/sig #'t))]
    [(#%constructor-decl t) `(constructor ,name : ,(type->string/sig #'t))]
    [(#%type-decl (#%alias () t)) `(type ,name = ,(type->string/sig #'t))]
    [(#%type-decl (#%alias (x ...+) t))
     `(type (,name ,@(syntax->datum #'(x ...))) = ,(type->string/sig #'t))]
    [(#%type-decl (#%opaque ())) `(type ,name)]
    [(#%type-decl (#%opaque (x ...+)))
     `(type (,name ,@(syntax->datum #'(x ...))))]
    [(#%type-decl (#%data () c ...))
     ;; TODO: use the types of the constructors to print the variants
     `(data ,name ....)]
    [(#%type-decl (#%data (x ...+) c ...))
     ;; TODO: use the types of the constructors to print the variants
     `(data (,name ,@(syntax->datum #'(x ...))) ....)]
    [(#%module-decl signature)
     `(module ,name : ,(sig->datum #'signature))]))

;; like Hackett's type->string, but handles module-specific forms (like #%dot)
(define (type->string/sig t)
  (type->string
   (let traverse ([t t])
     (syntax-parse t
       #:literal-sets [type-literals]
       #:literals [#%dot_m #%dot_τ #%apply-type]
       [(~#%type:app* (#%type:con #%dot_m) m:expr (#%type:con x:id))
        (format-id #f "~a.~a"
                   (syntax->datum (traverse #'m))
                   #'x)]
       [(~#%type:app* (#%type:con #%dot_τ) m:expr (#%type:con x:id))
        (format-id #f "~a.~a"
                   (syntax->datum (traverse #'m))
                   #'x)]
       [(~#%type:app* (#%type:con #%apply-type) t:expr)
        (traverse #'t)]
       [(~#%type:app* (#%type:con #%apply-type) t:expr a ...)
        (traverse #'(t a ...))]
       [_ (traverse-stx/recur t traverse)]))))
