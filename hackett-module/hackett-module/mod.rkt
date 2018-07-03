#lang racket/base
(require
 "rep/sig-literals.rkt"
 "rep/apply-type.rkt"
 "namespace/reqprov.rkt"
 racket/pretty
 syntax/parse/define
 (except-in hackett/private/type-language
            ~type
            type-namespace-introduce
            value-namespace-introduce)
 (only-in hackett/private/adt
          data-constructor?
          data-constructor-arity
          data-constructor-make-match-pat
          data-constructor-spec
          type-constructor-spec)
 (prefix-in hkt: hackett/base)
 (only-in hackett/private/base [core-def hkt:core-def])
 (prefix-in sig: (unmangle-in #:no-introduce "sig.rkt"))
 (prefix-in mod: (unmangle-in #:no-introduce "def.rkt"))
 (prefix-in l: "link/mod.rkt")
 "util/block-defer-reconstruct.rkt"
 (for-syntax racket/base
             racket/match
             racket/list
             syntax/parse
             syntax/kerncase
             syntax/id-set
             syntax/id-table
             hackett/private/expand+elaborate
             "rep/sig.rkt"
             "rep/resugar.rkt"
             "check/expand-check.rkt"
             "namespace/namespace.rkt"
             "util/stx-traverse.rkt"))

(provide
 (module-out mod))

(begin-for-syntax
  (define disappeared-binding 'disappeared-binding)

  (define-literal-set mod-stop-literals
    #:literal-sets [kernel-literals]
    [hkt:core-def hkt:: hkt:type hkt:data mod:def-module])

  (define mod-stop-ids
    (list #'hkt:core-def
          #'hkt::
          #'hkt:type
          #'hkt:data
          #'mod:def-module
          #'define-values
          #'define-syntaxes
          #'begin
          #'#%require
          #'#%provide
          ))

  (define kernel-id-set
    (immutable-free-id-set (kernel-form-identifier-list)))
  (define kernel-known-expr-id-set
    (immutable-free-id-set (list #'#%expression
                                 #'#%plain-lambda
                                 #'case-lambda
                                 #'if
                                 #'begin0 ; not begin!
                                 #'let-values
                                 #'letrec-values
                                 #'set!
                                 #'quote
                                 #'quote-syntax
                                 #'with-continuation-mark
                                 #'#%plain-app
                                 #'#%top
                                 #'#%variable-reference)))

  (define-syntax-class hackett-module-component
    #:attributes [sig-entry [val-id 1] [type-id 1] [mod-id 1] residual]
    #:literal-sets [mod-stop-literals]

    ;; NOTE: we are not introducing the type namespace
    ;;   here, because they will be introduced by `sig:val`
    ;;   and `sig:type` (the surface syntax).
    [pattern (hkt:core-def ~! id:id _ hkt:: type:expr {~optional #:exact} _ ...)
             #:with sig-entry #'(sig:val id : type)
             #:with [val-id ...] #'[id]
             #:with [type-id ...] #'[]
             #:with [mod-id ...] #'[]
             #:with residual (syntax-property #'(values)
                                              disappeared-binding
                                              (syntax-local-introduce #'id))]

    [pattern (hkt:data {~and {~seq head-stuff ...}
                             {~seq head:type-constructor-spec}}
                       {~and {~seq variant-stuff ...}
                             {~seq variant:data-constructor-spec}}
                       ...)
             #:with {~type T*} #'head.tag
             #:with sig-entry #'(sig:data head-stuff ... variant-stuff ... ...)
             #:with [val-id ...] #'[variant.tag ...]
             #:with [type-id ...] #'[T*]
             #:with [mod-id ...] #'[]
             ;; TODO: attach the other things
             #:with residual #'(values)]

    [pattern (hkt:type ~! {~and spec {~or x:id (x:id . _)}} rhs:expr)
             #:with {~type T*} #'x
             #:with sig-entry #'(sig:type spec = rhs)
             #:with [val-id ...] #'[]
             #:with [type-id ...] #'[T*]
             #:with [mod-id ...] #'[]
             #:with residual (syntax-property #'(values)
                                              disappeared-binding
                                              (syntax-local-introduce #'x))]

    [pattern (mod:def-module ~! M:id body:expr)
             #:with {~module M*} #'M
             #:with {~module body*} #'body
             #:with [_ sig] (sig⇒ #'body*)
             #:do [(define key (namespaced:module (syntax-e #'M*)))]
             #:with sig-entry #`(sig:#%internal-decl
                                 #,(sig:internal-decl-struct
                                    key #'M* #'(#%module-decl sig)))
             #:with [val-id ...] #'[]
             #:with [type-id ...] #'[]
             #:with [mod-id ...] #'[M*]
             #:with residual #'(values)]

    )

  ;; Id -> Bool
  (define (variable-id? x)
    (define sym (gensym 'fail))
    ;; bound
    (and (identifier-binding x)
         ;; not a macro
         (eq? sym (syntax-local-value x (λ () sym)))
         ;; not a core form
         (not (free-id-set-member? kernel-id-set x))))

  (define-syntax-class pass-through
    #:literal-sets [mod-stop-literals]
    ;; pass-through these types of definitions
    [pattern ({~or define-values
                   define-syntaxes
                   }
              . _)]
    ;; pass-through known expressions
    [pattern x:id  #:when (variable-id? #'x)]
    [pattern (head:id . _)
             #:when
             (or (free-id-set-member? kernel-known-expr-id-set #'head)
                 (variable-id? #'head))])

  (define-syntax-class disallowed
    #:literal-sets [mod-stop-literals]
    [pattern ({~or #%provide
                   #%require
                   begin-for-syntax
                   module
                   module*
                   }
              . _)])

  )

; The mod/acc form parses and expands definitions using the "trampolining #%module-begin" technique;
; we insert `begin` expressions so that the outer Racket expander will handle the introduction of
; bindings.
;
; When mod/acc is done parsing the definitions, it will leave behind an expression that generates
; a `l:mod` struct for the module. This expression will have a `sig⇒` syntax property on it containing
; the inferred signature.

(begin-for-syntax
  (define mod/acc-sig-prop (gensym 'sig⇒))

  (define-syntax-class mod/acc-sig
    #:attributes [sig]
    ;; base case, when the expression with sig⇒ is found
    [pattern stx
             #:when (syntax-property #'stx mod/acc-sig-prop)
             #:attr sig (syntax-local-introduce
                         (syntax-property #'stx mod/acc-sig-prop))]

    ;; recursive case for an intermediate binding expression such as
    ;; (let-values ([....]) ....), (letrec-values ([....]) ....),
    ;; (letrec-syntaxes+values ([....]) ....), etc.
    [pattern (stuff ... next)
             #:with :mod/acc-sig #'next]))

(define-syntax-parser mod/acc
  [(_ [sig-entry/rev ...] [val-id/rev ...] [type-id/rev ...] [mod-id/rev ...])

   #:with [val-id ...] (reverse (attribute val-id/rev))
   #:with [[val-sym/id ...] ...] #'[['val-id val-id] ...]

   #:do [(define ctor-ids/vals
           (for*/list ([val-id (in-list (attribute val-id))]
                       [val-val (in-value (syntax-local-value val-id (λ () #f)))]
                       #:when (data-constructor? val-val))
             (list val-id val-val)))]
   #:with [[ctor-sym/pat ...] ...]
   (for/list ([id/v (in-list ctor-ids/vals)])
     (match-define (list id v) id/v)
     (define n (data-constructor-arity v))
     (define sub-ids (generate-temporaries (range n)))
     (define make-match-pat (data-constructor-make-match-pat v))
     (list #`'#,id #`(l:make-pat-info #,(make-match-pat sub-ids) #,sub-ids)))

   #:with [mod-id ...] (reverse (attribute mod-id/rev))
   #:with [[submod-sym/id ...] ...] #'[['mod-id/rev mod-id/rev] ...]

   #:with [sig-entry ...] (reverse (attribute sig-entry/rev))
   #:do [(define type-ids (make-immutable-free-id-table
                           (map cons
                                (attribute type-id/rev)
                                (attribute type-id/rev))))
         (define (fix-inferred-app-con-types stx)
           (syntax-parse stx
             #:literal-sets [u-type-literals]
             [{~#%type:app* (#%type:con inner-id:id) arg ...}
              #:with outer-id (free-id-table-ref type-ids #'inner-id #f)
              #:when (identifier? #'outer-id)
              #:with [arg* ...] (map fix-inferred-app-con-types (attribute arg))
              #'(#%apply-type outer-id arg* ...)]
             [({~literal sig:#%internal-decl} _) stx]
             [_
              (traverse-stx/recur stx fix-inferred-app-con-types)]))]
   #:with s:sig (fix-inferred-app-con-types #'(sig:sig sig-entry ...))
   #:with s-reintro
   (for/fold ([stx #'s.expansion])
             ([mod-id (in-list (attribute mod-id))])
     (define mod-internal-id
       (hash-ref (sig-internal-ids stx)
                 (namespaced:module (syntax-e mod-id))))
     (resugar mod-id
              mod-internal-id
              stx
              #f))

   #:with final-expression #'(let-values ([() s.residual])
                               (l:mod (hash val-sym/id ... ...)
                                      (hash ctor-sym/pat ... ...)
                                      (hash submod-sym/id ... ...)))

   (syntax-property #'final-expression
     mod/acc-sig-prop
     (syntax-local-introduce
      (attribute s-reintro)))]

  [(head [ent/rev ...] [v/rev ...] [t/rev ...] [m/rev ...] defn rest-defn ...)
   #:with defn- (local-expand/defer-elaborate #'defn 'module mod-stop-ids)
   (syntax-parse #'defn-
     #:literal-sets [mod-stop-literals]

     [(begin form ...)
      #'(mod/acc [ent/rev ...] [v/rev ...] [t/rev ...] [m/rev ...]
                 form ... rest-defn ...)]

     [d:hackett-module-component
      (syntax-track-origin
       #'(begin
           defn-
           (mod/acc [d.sig-entry ent/rev ...]
                    [d.val-id ... v/rev ...]
                    [d.type-id ... t/rev ...]
                    [d.mod-id ... m/rev ...]
                    rest-defn ...))
       #'d.residual
       #'head)]

     [:pass-through
      #'(begin
          defn-
          (mod/acc [ent/rev ...] [v/rev ...] [t/rev ...] [m/rev ...]
                   rest-defn ...))]

     [{~and (head . _) :disallowed}
      (raise-syntax-error #f
        (format "~a form not allowed inside of a module"
                #'head)
        #'defn)])])

(define-syntax-parser mod
  [(_ {~value defn} ...)
   ;; put the defns in a new scope
   #:with [defn* ...] ((make-syntax-introducer #true) #'[defn ...])

   ;; expand `mod/acc` in a (let () ....) to parse the definitions
   #:with expansion:mod/acc-sig
   (call-with-no-elaborate-pass
    (λ ()
      (local-expand+elaborate #'(block/defer-reconstruct
                                  (mod/acc [] [] [] [] defn* ...)))))

   (attach-sig #'expansion
               (attribute expansion.sig))])
