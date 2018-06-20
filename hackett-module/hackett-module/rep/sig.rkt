#lang racket/base

(provide sig
         decl
         expand-sig
         expand-decl
         signature-subst
         signature-substs
         sig-where/type)

(require racket/syntax
         racket/function
         racket/list
         racket/match
         syntax/parse
         syntax/parse/experimental/template
         syntax/parse/define
         (only-in syntax/parse [attribute @])
         syntax/intdef
         syntax/id-table
         threading
         hackett/private/util/stx
         hackett/private/typecheck
         "../util/stx.rkt"
         "../util/stx-traverse.rkt"
         "../util/stx-subst.rkt"
         "../util/hash.rkt"
         "../prop-dot-accessible.rkt"
         "../prop-reintroducible-dot-type.rkt"
         "../namespace/reqprov.rkt"
         "../rep/resugar.rkt"
         (for-syntax racket/base)
         (for-template racket/base
                       (only-in hackett/private/type-alias
                                make-alias-transformer)
                       (rename-in (unmangle-in "../dot/dot-e.rkt") [#%dot #%dot_e])
                       (rename-in (unmangle-in "../dot/dot-t.rkt") [#%dot #%dot_τ])
                       (rename-in (unmangle-in "../dot/dot-m.rkt") [#%dot #%dot_m])
                       "sig-literals.rkt"
                       "apply-type.rkt"))

; helper for expanding signatures; combines "residual" properties
(define (residual origs id)
  (for/fold ([acc #'(values)])
            ([orig (in-list (syntax->list origs))])
    (syntax-track-origin acc orig id)))

;; ---------------------------------------------

(define ((make-local-expand literal-ids) stx [intdef-ctx #f])
  (local-expand stx 'expression literal-ids intdef-ctx))

(define-simple-macro
  (define-expansion-class name:id desc:str expanded-name:id expand-fn:expr)
  (define-syntax-class (name [intdef-ctx #f])
    #:description desc
    #:attributes [expansion residual]
    [pattern stx
             #:with {~var || (expanded-name intdef-ctx)}
             (expand-fn #'stx intdef-ctx)]))

(define sig-local-expand
  (make-local-expand sig-literal-ids))

(define-expansion-class ref-id "id reference" expanded-id sig-local-expand)

(define-expansion-class partial-sig "signature" partial-expanded-sig sig-local-expand)
(define-expansion-class partial-decl "signature declaration" partial-expanded-decl sig-local-expand)

;; Stx [IntDefCtx] -> PartialSig
(define (partial-expand-sig stx [intdef-ctx #f])
  (syntax-parse stx
    [{~var S (partial-sig intdef-ctx)}
     #'S.expansion]))

;; Stx [IntDefCtx] -> PartialDecl
(define (partial-expand-decl stx [intdef-ctx #f])
  (syntax-parse stx
    [{~var D (partial-decl intdef-ctx)}
     #'D.expansion]))

(define-expansion-class sig "signature" expanded-sig partial-expand-sig)
(define-expansion-class decl "signature declaration" expanded-decl partial-expand-decl)

;; Stx [IntDefCtx] -> Signature
(define (expand-sig stx [intdef-ctx #f])
  (syntax-parse stx
    [{~var S (sig intdef-ctx)}
     #'S.expansion]))

;; Stx [IntDefCtx] -> Decl
(define (expand-decl stx [intdef-ctx #f])
  (syntax-parse stx
    [{~var D (decl intdef-ctx)}
     #'D.expansion]))

;; ---------------------------------------------

(define signature-subst stx-subst)
(define signature-substs stx-substs)

;; ---------------------------------------------

;; syntax-local-declare-decl :
;; Id PartialDecl ModulePath IntDefCtx -> [Signature -> SignatureStx]

;; declares that the decl exists without binding what its "equal to".
;; the root of `path-to-id` should already have the scope introduced
;; for the context its expanding to.
;; returns a function to call on an expanded signature after fully
;; expanding it.

;; The things this defines the decls to expand into are *uninterpreted*,
;; so sig-expansion must reinterpret the result after expanding using
;; this environment.
(define (syntax-local-declare-decl id decl path-to-id intdef-ctx)
  (syntax-parse decl
    #:context 'syntax-local-declare-decl
    #:literal-sets [sig-literals]
    [{~or (#%val-decl . _)
          (#%constructor-decl . _)}
     #:with path path-to-id
     (define rhs
       #'(make-variable-like-transformer (quote-syntax path)))
     (syntax-local-bind-syntaxes (list id) rhs intdef-ctx)
     identity]

    [(#%type-decl {~or (#%opaque [x ...])
                       (#%alias [x ...] _)
                       (#%data [x ...] . _)})
     #:with path (path->u-type-path path-to-id)
     #:with t (template (?#%apply-type path x ...))
     (define rhs
       #'(make-declared-type-transformer
          (list (quote-syntax x) ...)
          (quote-syntax t)))
     (syntax-local-bind-syntaxes (list id) rhs intdef-ctx)

     (λ (stx)
       (resugar (internal-definition-context-introduce intdef-ctx id)
                stx
                intdef-ctx))]

    [(#%module-decl (#%pi-sig . _))
     (syntax-local-bind-syntaxes (list id) #f intdef-ctx)
     identity]

    [(#%module-decl (#%sig internal-ids:hash-literal
                           decls:hash-literal))

     (define-values [keys tmp-ids reintros]
       (for/lists (keys tmp-ids reintros)
                  ([(local-key decl) (in-hash (@ decls.value))])
         (define sym (namespaced-symbol local-key))
         (define tmp-id
           (generate-temporary/1num sym))
         (define dot
           (decl-dot-form decl))
         (define local-path
           #`(#,dot #,path-to-id #,sym))
         (define nested-reintro
           (syntax-local-declare-decl tmp-id decl local-path intdef-ctx))
         (values local-key tmp-id nested-reintro)))

     (define reintro (apply compose reintros))

     (define/syntax-parse [key ...] keys)
     (define/syntax-parse [tmp-id ...] tmp-ids)
     (define/syntax-parse [[key/tmp-id ...] ...] #'[['key (quote-syntax tmp-id)] ...])

     (syntax-local-bind-syntaxes
      (list id)
      #`(declared-module-var
         (hash key/tmp-id ... ...))
      intdef-ctx)

     reintro]))


;; PartialDecl -> Identifier
(define (decl-dot-form decl)
  (syntax-parse decl
    #:literal-sets [sig-literals]
    [(#%type-decl . _) #'#%dot_τ]
    [(#%module-decl . _) #'#%dot_m]
    [{~or (#%val-decl . _) (#%constructor-decl . _)} #'#%dot_e]))

;; ----------
;; declared module var transformer

(struct declared-module-var [key->tmp-id]
  #:property prop:dot-accessible/type
  (λ (self)
    (define hsh (declared-module-var-key->tmp-id self))
    (dot-accessible/type
     (λ (key)
       (define id (hash-ref hsh key #f))
       id)))

  #:property prop:dot-accessible/module
  (λ (self)
    (define hsh (declared-module-var-key->tmp-id self))
    (dot-accessible/module
     (λ (key)
       (define id (hash-ref hsh key #f))
       id))))

;; ----------
;; declared type transformer

(struct declared-type-transformer [sym transformer]
  #:methods gen:non-currying-type-transformer []
  #:property prop:procedure (struct-field-index transformer)
  #:property prop:resugar-origin
  (λ (self)
    (match-define (declared-type-transformer stxprop _) self)
    (resugar-origin stxprop
                    u-type-app->type)))

;; [Listof Id] TypeStx -> [Stx -> Stx]
(define (make-declared-type-transformer args template)
  (define sym (gensym))
  (define template* (syntax-property template sym #t))
  (declared-type-transformer
   sym
   (make-alias-transformer args template* #f)))

;; -----------------

(define-syntax-class (expanded-id intdef-ctx)
  #:description #f
  #:attributes [expansion residual]
  [pattern expansion:id
           #:with residual (residual #'[] #'expansion)])

;; expects result from partial-sig.expansion as input, outputs
;; a fully expanded sig in .expansion.
(define-syntax-class (expanded-sig intdef-ctx)
  #:description #f
  #:attributes [expansion residual]
  #:commit
  #:literal-sets [sig-literals]

  ;; A Key is a (namespaced Namespace Symbol)
  ;; (#%sig
  ;;   #:hash([key . internal-id]
  ;;          ...)
  ;;   #:hash([key . decl]
  ;;          ...))
  [pattern (head:#%sig
            internal-ids:hash-literal
            decls:hash-literal)

           ;; create a context where all of the internal-ids are bound
           #:do [(define intdef-ctx* (syntax-local-make-definition-context intdef-ctx))
                 (define (intro stx)
                   (internal-definition-context-introduce intdef-ctx* stx))

                 ;; bind the ids and build up the function together
                 (define-values [new-internal-ids reintro-dotss]
                   (for/fold ([new-internal-ids (hash)]
                              [reintro-dotss '()])
                             ([(key id) (in-hash (@ internal-ids.value))])
                     (define id- (intro (generate-temporary/1num id)))
                     (define decl (hash-ref (@ decls.value) key))
                     (syntax-local-bind-syntaxes (list id-) #f intdef-ctx*)
                     (define nested-reintro
                       (syntax-local-declare-decl id decl id- intdef-ctx*))
                     (values (hash-set new-internal-ids key id-)
                             (cons nested-reintro reintro-dotss))))
                 (define reintro-dots (apply compose reintro-dotss))]

           #:with internal-ids- new-internal-ids
           #:with [{~var decl- (expanded-decl intdef-ctx*)} ...]
           (map intro (attribute decls.values))
           #:with decls-expansion-
           (reintro-dots
            (hash-zip (attribute decls.keys) (attribute decl-.expansion)))
           #:attr expansion (~>> (syntax/loc/props this-syntax
                                   (head internal-ids-
                                         decls-expansion-))
                                 (internal-definition-context-track intdef-ctx*))
           #:attr residual (residual #'[decl-.residual ... expansion]
                                     #'head)]

  ;; (#%pi-sig ([param-id sig]) sig)
  [pattern (head:#%pi-sig ([x:id {~var A (sig intdef-ctx)}]) B:expr)
           #:with x- (generate-temporary/1num #'x)

           ;; create a context where x is bound
           #:do [(define intdef-ctx* (syntax-local-make-definition-context intdef-ctx))
                 (define (intro stx)
                   (internal-definition-context-introduce intdef-ctx* stx))
                 (syntax-local-bind-syntaxes (list #'x-) #f intdef-ctx*)]
           #:with x-- (intro #'x-)

           #:do [(define reintro-dots
                   (syntax-local-declare-decl #'x #'(#%module-decl A.expansion) #'x-- intdef-ctx*))]

           #:with {~var B* (sig intdef-ctx*)} (intro #'B)
           #:with B** (reintro-dots #'B*.expansion)

           #:attr expansion (~>> (syntax/loc/props this-syntax
                                   (head ([x-- A.expansion])
                                         B**))
                                 (internal-definition-context-track intdef-ctx*))
           #:attr residual (residual #'[A.residual B*.residual expansion]
                                     #'head)])

;; expects result from partial-decl.expansion as input, outputs
;; a fully expanded decl in .expansion.
(define-syntax-class (expanded-decl intdef-ctx)
  #:description #f
  #:attributes [expansion residual]
  #:commit
  #:literal-sets [sig-literals]

  ;; (#%val-decl type)
  [pattern (val-decl:#%val-decl (~var val-type (type intdef-ctx)))
           #:attr expansion (syntax/loc/props this-syntax
                              (val-decl val-type.expansion))
           #:attr residual (residual #'[val-type.residual expansion]
                                     #'val-decl)]

  ;; (#%constructor-decl type)
  [pattern (constr-decl:#%constructor-decl (~var constr-type (type intdef-ctx)))
           #:attr expansion (syntax/loc/props this-syntax
                              (constr-decl constr-type.expansion))
           #:attr residual (residual #'[constr-type.residual expansion]
                                     #'constr-decl)]

  ;; (#%type-decl (#%alias [id ...] type))
  [pattern (type-decl:#%type-decl
            (alias:#%alias [x:id ...] alias-type:expr))

           ;; create a context where the xs are bound
           #:do [(define intdef-ctx* (syntax-local-make-definition-context intdef-ctx))
                 (define (intro stx)
                   (internal-definition-context-introduce intdef-ctx* stx))
                 (syntax-local-bind-syntaxes (attribute x) #f intdef-ctx*)]

           #:with [x- ...] (map intro (attribute x))
           #:with {~var alias-type- (type intdef-ctx*)}
           (intro #'alias-type)

           #:attr expansion (~>> (syntax/loc/props this-syntax
                                   (type-decl (alias [x- ...] alias-type-.expansion)))
                                 (internal-definition-context-track intdef-ctx*))
           #:attr residual (residual #'[alias-type-.residual expansion]
                                     #'type-decl)]

  ;; (#%type-decl (#%opaque [id ...]))
  [pattern (type-decl:#%type-decl (opaque:#%opaque [x:id ...]))
           #:attr expansion this-syntax
           #:attr residual (residual #'[expansion]
                                     #'type-decl)]

  ;; (#%type-decl (#%data [param-id ...] constructor-id ...))
  [pattern (type-decl:#%type-decl
            (data:#%data [x:id ...] ctor:id ...))

           ;; create a context where the xs are bound
           #:do [(define intdef-ctx* (syntax-local-make-definition-context intdef-ctx))
                 (define (intro stx)
                   (internal-definition-context-introduce intdef-ctx* stx))
                 (syntax-local-bind-syntaxes (attribute x) #f intdef-ctx*)]

           #:with [x- ...] (map intro (attribute x))
           #:with [{~var constructor-id (ref-id intdef-ctx*)} ...]
           (map intro (@ ctor))

           #:attr expansion (syntax/loc/props this-syntax
                              (type-decl (data [x- ...] constructor-id.expansion ...)))
           #:attr residual (residual #'[constructor-id.residual ... expansion]
                                     #'type-decl)]

  ;; (#%module-decl signature)
  [pattern (module-decl:#%module-decl
            {~var signature (sig intdef-ctx)})
           #:attr expansion (syntax/loc/props this-syntax
                              (module-decl signature.expansion))
           #:attr residual (residual #'[signature.residual expansion]
                                     #'module-decl)]

  )

;; ---------------------------------------------

;; expects once local-expanded syntax as input, outputs
;; partially expanded signature.
(define-syntax-class (partial-expanded-sig intdef-ctx)
  #:description #f
  #:attributes [expansion residual]
  #:commit
  #:literal-sets [sig-literals]

  [pattern (sig:#%sig
            internal-ids:hash-literal
            decls:hash-literal)

           #:with [{~var decl- (partial-decl intdef-ctx)} ...]
           (attribute decls.values)
           #:with decls-
           (hash-zip (attribute decls.keys) (attribute decl-.expansion))
           #:with expansion
           (syntax/loc/props this-syntax
             (sig internal-ids decls-))
           #:attr residual (residual #'[decl-.residual ... expansion] #'sig)]

  [pattern (pi:#%pi-sig . rst)
           #:with expansion this-syntax
           #:with residual (residual #'[expansion] #'pi)])

;; expects once local-expanded decl as input, outputs
;; partially expanded decl.
(define-syntax-class (partial-expanded-decl intdef-ctx)
  #:description #f
  #:attributes [expansion residual]
  #:commit
  #:literal-sets [sig-literals]

  [pattern (module-decl:#%module-decl
            {~var s (partial-sig intdef-ctx)})
           #:with expansion #'(module-decl s.expansion)
           #:with residual (residual #'[s.residual expansion] #'module-decl)]

  [pattern (head . rst)
           #:with expansion this-syntax
           #:with residual (residual #'[expansion] #'head)])

;; ---------------------------------------------

;; sig-where/type : Sig Symbol Type -> Sig
(define (sig-where/type base sym type)
  (syntax-parse base
    #:literal-sets [sig-literals]
    [(sig:#%sig internal-ids:hash-literal decls:hash-literal)
     #`(sig
        internal-ids
        #,(hash-update (attribute decls.value)
                       (namespaced:type sym)
            (λ (prev-decl)
              (syntax-parse prev-decl #:literal-sets [sig-literals]
                [(type-decl:#%type-decl (#%opaque ()))
                 #`(type-decl (#%alias () #,type))]
                [_
                 (raise-syntax-error #f
                   "can't `where` a non-opaque declaration"
                   base)]))
            (λ ()
              (raise-syntax-error #f
                "can't `where` a non-existent declaration"
                base))))]))
