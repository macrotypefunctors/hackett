#lang racket/base

(provide block/defer-reconstruct)

(require racket/block
         syntax/parse/define
         (for-syntax racket/base
                     racket/match
                     syntax/intdef
                     hackett/private/expand+elaborate))

;; A lot of this code is copied from `racket/block`.

(begin-for-syntax
  ;; make-context copied from `racket/block`
  (define make-context
    (let-values ([[struct: mk ? ref set]
                  (make-struct-type
                   'in-liberal-define-context #f 0 0 #f
                   (list (cons prop:liberal-define-context #t)))])
      mk))

  (define stoplist (list #'begin #'define-syntaxes #'define-values
                         #'quote #'set! #'#%plain-lambda #'case-lambda
                         #'let-values #'letrec-values #'if #'begin0
                         #'with-continuation-mark #'letrec-syntaxes+values
                         #'#%plain-app #'#%expression #'#%top
                         #'#%variable-reference))

  ;; partially-expand-in-ctxs/defer :
  ;; [Listof Syntax] IntdefCtx LibdefCtx -> [Listof Syntax]
  ;; copied from the `(let loop ([todo init-exprs] [r '()]) ....)`
  ;; in `racket/block`, then modified to use
  ;; `local-expand/defer-elaborate`, before getting the
  ;; define-values / define-syntaxes forms, and not any further.
  ;; This corresponds to "head expansion", the first pass where
  ;; it figures out which definitions exist.
  (define (partially-expand-in-ctxs/defer init-exprs def-ctx ctx)
    (let loop ([todo init-exprs] [r '()])
      (match todo
        ['()  (reverse r)]
        [(cons expr todo)
         (let ([expr (local-expand/defer-elaborate expr
                                                   ctx
                                                   stoplist
                                                   (list def-ctx))])
           (syntax-parse expr
             #:literals [begin define-syntaxes define-values]
             [(begin)
              (loop todo r)]
             [(begin . rest)
              (loop (append (syntax->list #'rest) todo) r)]
             [(define-syntaxes (id:id ...) rhs)
              (with-syntax ([rhs (local-transformer-expand
                                  #'rhs 'expression null)])
                (syntax-local-bind-syntaxes
                 (syntax->list #'(id ...))
                 #'rhs def-ctx)
                (with-syntax ([(id ...) (map syntax-local-identifier-as-binding
                                             (syntax->list #'(id ...)))])
                  (loop todo (cons #'(define-syntaxes (id ...) rhs) r))))]
             [(define-values (id:id ...) rhs)
              (let ([ids (syntax->list #'(id ...))])
                (syntax-local-bind-syntaxes ids #f def-ctx)
                (with-syntax ([(id ...) (map syntax-local-identifier-as-binding
                                             (syntax->list #'(id ...)))])
                  (loop todo (cons #'(define-values (id ...) rhs) r))))]
             [else (loop todo (cons expr r))]))])))

  ;; continue-expand-in-ctxs/defer :
  ;; [Listof Syntax] IntdefCtx LibdefCtx -> [Listof Syntax]
  ;; The second pass, after partially-expand-in-ctxs/defer
  ;; has found all the definitions that these could refer
  ;; to.
  (define (continue-expand-in-ctxs/defer init-exprs def-ctx ctx)
    (let loop ([todo init-exprs] [r '()])
      (match todo
        ['()  (reverse r)]
        [(cons expr todo)
         (syntax-parse expr
           #:literals [begin define-syntaxes define-values]
           [(begin)
            (loop todo r)]
           [(begin . rest)
            (loop (append (syntax->list #'rest) todo) r)]
           [(define-syntaxes (id:id ...) rhs)
            ;; the ids are already registered in `def-ctx`,
            ;; along with everything else defined before or
            ;; after
            (loop todo (cons expr r))]
           [(define-values (id:id ...) rhs)
            ;; the ids are already registered in `def-ctx`,
            ;; along with everything else defined before or
            ;; after
            #:with rhs* (local-expand/defer-elaborate #'rhs
                                                      'expression
                                                      '()
                                                      (list def-ctx))
            (loop todo (cons #'(define-values (id ...) rhs*) r))]
           [else
            #:with expr* (local-expand/defer-elaborate expr
                                                       'expression
                                                       '()
                                                       (list def-ctx))
            (loop todo (cons #'expr* r))])])))

  )

(define-syntax-parser block/defer-reconstruct
  [(head form:expr ...+)
   ;; The forms can have mixed exprs and defns. Wrap expressions with
  ;; `(define-values () ... (values))' as needed.
  (match (syntax-local-elaborate-pass)
     [(or 'expand 'elaborate)
      (define def-ctx (syntax-local-make-definition-context))
      (define ctx (list (make-context)))
      (define init-exprs (attribute form))
      (define exprs/1 (partially-expand-in-ctxs/defer init-exprs def-ctx ctx))
      (define exprs/2 (continue-expand-in-ctxs/defer exprs/1 def-ctx ctx))
      ;; reconstruct the block with the partially expanded forms
      (syntax-local-elaborate-defer
       (internal-definition-context-track
        def-ctx
        #`(head . #,exprs/2)))]
     ['finalize
      #'(block form ...)])])

