#lang racket/base
(provide
 generate-module-var-bindings
 syntax-local-bind-module)

(require
 racket/list
 racket/match
 racket/syntax
 syntax/parse
 syntax/parse/experimental/template
 (only-in syntax/parse [attribute @])
 syntax/parse/class/local-value
 hackett/private/typeclass
 hackett/private/util/stx
 "expand-check-prop.rkt"
 "../rep/resugar.rkt"
 "../rep/sig.rkt"
 "../resugar-dot-sym.rkt"
 "../prop-dot-accessible.rkt"
 "../util/stx.rkt"
 "../util/stx-subst.rkt"
 "../util/hash.rkt"
 "../util/partition.rkt"
 "../namespace/reqprov.rkt"
 "../namespace/namespace.rkt"
 (for-template "../rep/sig-literals.rkt"
               "../rep/apply-type.rkt"
               (only-in (unmangle-in "../dot/dot-t.rkt") [#%dot #%dot_τ])
               (only-in (unmangle-in "../dot/dot-m.rkt") [#%dot #%dot_m])
               (only-in racket/base #%app quote)
               (prefix-in l: "../link/mod.rkt")
                "../util/register-local-class-instance.rkt")
 (for-template (except-in hackett/private/type-language
                          type-namespace-introduce
                          value-namespace-introduce
                          ~type)
               (only-in hackett/private/base
                        make-typed-var-transformer)
               (only-in hackett/private/type-alias
                        make-alias-transformer)
               (only-in hackett/private/adt
                        data-constructor
                        [type-constructor data-type-constructor])))

;; internal-id : Id
;; unique-symbol : Symbol
;; signature : Signature
;; value-ids : [Hash Key Id]
;; pattern-ids : [Hash Key Id]
;; type-ids : [Hash Key Id]
(struct module-var-transformer
  [internal-id
   unique-symbol
   signature
   value-ids
   pattern-ids
   type-ids
   submod-ids]
  #:property prop:procedure
  (λ (self stx)
    (define x- (module-var-transformer-internal-id self))
    (define sig (module-var-transformer-signature self))
    ((make-variable-like-transformer
      (λ (id)
        (attach-sig (replace-stx-loc x- id)
                    (expand-sig (strengthen-signature id sig)))))
     stx))
  #:property prop:dot-accessible
  (λ (self)
    (match-define (module-var-transformer _ _ _ hv hp ht hm) self)
    (dot-accessible
     (λ (val-key) (hash-ref hv val-key #f))
     (λ (pat-key) (hash-ref hp pat-key #f))
     (λ (type-key) (hash-ref ht type-key #f))
     (λ (mod-key) (hash-ref hm mod-key #f))))
  #:property prop:resugar-origin
  (λ (self)
    (match-define (module-var-transformer internal-id sym _ _ _ _ _) self)
    (define (how-to-resugar path stx ctx)
      (syntax-parse stx
        #:literal-sets [u-type-literals]
        [(~#%type:app* (#%type:con
                        {~var inner-id resugar-dot-sym-id})
                       arg
                       ...)
         #`(#%apply-type (#%dot_τ #,path inner-id.sym-stx)
                         arg
                         ...)]))
    (resugar-origin sym how-to-resugar)))

(struct opaque-type-constructor [])

(define (make-resugarable-alias-transformer prop args inner-id*)
  (define/syntax-parse inner-id inner-id*)
  (define/syntax-parse [arg ...] args)
  (define t
    (syntax-property
     (template (?#%type:app* (#%type:con inner-id) arg ...))
     prop
     #true))
  (make-alias-transformer args t #f))

;; PathStx Signature -> Signature
;; Strengthen the signature, recursively replacing opaque types with
;; self references to preserve the identity of the original module.
;; `self-path` should be bound with the given signature, so that
;; the strengthened signature is valid for aliases of the path.
(define (strengthen-signature self-path* signature)
  (syntax-parse signature
    #:literal-sets [sig-literals]
    [(sig:#%sig internal-ids:hash-literal
                decls:hash-literal)

     #:with self-path self-path*

     #:do [(define (strengthen-decl key d)
             (syntax-parse d
               #:literal-sets [sig-literals]
               #;[(type-decl:#%type-decl (#%opaque []))
                ;#:with sym (namespaced-symbol key)
                #:with d-id ((@ self-type.key->id) key)
                #'(type-decl (#%alias [] d-id))]
               [(type-decl:#%type-decl (#%opaque [x:id ...]))
                #:with sym (namespaced-symbol key)
                ; #:with d-id ((@ self-type.key->id) key)
                #'(type-decl (#%alias [x ...] (#%apply-type (#%dot_τ self-path sym) x ...)))]

               [(mod-decl:#%module-decl submod-signature)
                #:with sym (namespaced-symbol key)
                ; #:with d-id ((@ self-module.key->id) key)
                #:with strong-sig (strengthen-signature #'(#%dot_m self-path sym) #'submod-signature)
                #'(mod-decl strong-sig)]

               [_ d]))]

     #:with decls-
     (for/hash ([(key decl) (in-hash (@ decls.value))])
       (values key (strengthen-decl key decl)))

     #'(sig internal-ids decls-)]

    [(#%pi-sig . _) signature]))


;; generate-decl-bindings :
;; Key Id Decl
;; #:parent-link-id Id
;; #:ctx IntdefCtx
;; ->
;; [List [Listof [List Id TransformerStx]]
;;       [Listof [List Id Stx]]
;;       [Listof TransformerStx]]

;; Returns binding syntax for the given decl.
;; If `decl` is value/ctor decl, then `type-expand-ctx` must
;; have all of the necessary type bindings declared in it.
(define (generate-decl-bindings key id decl
                                #:parent-unique-symbol parent-sym*
                                #:parent-link-id parent-link-id*
                                #:ctx type-expansion-ctx)
  (define/syntax-parse key-sym
    (namespaced-symbol key))
  (define/syntax-parse parent-link-id
    parent-link-id*)
  (define/syntax-parse parent-sym
    parent-sym*)

  (define (expand-type/rem-ctx t)
    (internal-definition-context-introduce
     type-expansion-ctx
     (expand-type t type-expansion-ctx)
     'remove))

  (syntax-parse decl
    #:literal-sets [sig-literals]

    ;; ------
    ;; module decls

    [(#%module-decl signature)
     (define link-internal-id
       (generate-temporary (format-symbol "link:~a" id)))
     (define link-expr
       #`(l:mod-submod-ref parent-link-id 'key-sym))

     (match-define `{,submod-stx-bindings ,submod-val-bindings ,submod-effs}
       (generate-module-var-bindings id
                                     link-internal-id
                                     #'signature))

     `{,submod-stx-bindings
       ([,link-internal-id ,link-expr] ,@submod-val-bindings)
       ,submod-effs}]

    ;; ------
    ;; type decls

    [(#%type-decl (#%alias [x ...] t))
     (define rhs
       #'(make-alias-transformer
          (list (quote-syntax x) ...)
          (quote-syntax t)
          #f))
     `{([,id ,rhs]) () ()}]

    [(#%type-decl (#%opaque [x ...]))
     #:with inner-id (generate-temporary (format-symbol "inner-opaque:~a" id))

     ;; generate an alias that puts the `parent-sym` property
     ;; on as it expands, so that `resugar` can put whatever
     ;; it expands to "back" as (#%apply-type ...)

     ;; outer-rhs is a resugarable alias transformer that turns
     ;; (id arg ...) into (#%type:app* (#%type:con inner-id) arg ...)
     ;; with a property on it so that resugar can get it back
     (define outer-rhs
       #'(make-resugarable-alias-transformer
          'parent-sym
          (list (quote-syntax x) ...)
          (attach-resugar-dot-sym
           (quote-syntax inner-id)
           'key-sym)))

     ;; inner-rhs is the value on the inner-id within (#%type:con inner-id)
     (define inner-rhs
       #'(opaque-type-constructor))
     `{([,#'inner-id ,inner-rhs]
        [,id ,outer-rhs])
       ()
       ()}]

    [(#%type-decl (#%data [x ...] c-id ...))
     #:with inner-id (generate-temporary (format-symbol "inner-data:~a" id))

     ;; generate an alias that puts the `parent-sym` property
     ;; on as it expands, so that `resugar` can put whatever
     ;; it expands to "back" as (#%apply-type ...)
     #:with type-var-arity (length (@ x))

     ;; outer-rhs is a resugarable alias transformer that turns
     ;; (id arg ...) into (#%type:app* (#%type:con inner-id) arg ...)
     ;; with a property on it so that resugar can get it back
     (define outer-rhs
       #'(make-resugarable-alias-transformer
          'parent-sym
          (list (quote-syntax x) ...)
          (attach-resugar-dot-sym
           (quote-syntax inner-id)
           'key-sym)))

     ;; inner-rhs is the value on the inner-id within (#%type:con inner-id)
     ;; and it needs to interact with pattern matching
     (define inner-rhs
       #`(data-type-constructor
          (quote-syntax (#%type:con #,id))        ; type
          'type-var-arity                         ; arity
          (list (quote-syntax c-id) ...)          ; constructor ids
          #f))                                    ; fixity
     `{([,#'inner-id ,inner-rhs]
        [,id ,outer-rhs])
       ()
       ()}]

    ;; ------
    ;; constructor decls

    [(#%constructor-decl t)
     #:with t* (expand-type/rem-ctx #'t)
     #:with link-val-id (generate-temporary (format-symbol "link-val:~a" id))
     #:with link-pat-id (generate-temporary (format-symbol "link-pat:~a" id))
     (define link-val-expr #'(l:mod-value-ref parent-link-id 'key-sym))
     (define link-pat-expr #'(l:mod-pattern-ref parent-link-id 'key-sym))

     (define rhs
       #`(data-constructor
          (make-typed-var-transformer (quote-syntax link-val-id)
                                      (quote-syntax t*))
          (quote-syntax t*)
          (λ (sub-pats) #`(l:app/pat-info link-pat-id #,sub-pats))
          #f))

     `{([,id ,rhs])
       ([,#'link-val-id ,link-val-expr]
        [,#'link-pat-id ,link-pat-expr])
       ()}]

    ;; ------
    ;; value decls

    [(#%val-decl t)
     #:with t* (expand-type/rem-ctx #'t)
     #:with link-val-id (generate-temporary (format-symbol "link:~a" id))
     (define link-val-expr #'(l:mod-value-ref parent-link-id 'key-sym))

     (define rhs
       #'(make-typed-var-transformer (quote-syntax link-val-id)
                                     (quote-syntax t*)))

     `{([,id ,rhs])
       ([,#'link-val-id ,link-val-expr])
       ()}]

    ;; ------
    ;; instance decls

    [(#%instance-decl class [x ...] [constr ...] [bare-t ...])
     #:with (~#%type:forall* [x* ...]
              (~#%type:qual* [constr* ...]
                (~#%type:app* ({~literal #%type:con} class*) bare-t* ...)))
     (expand-type
      #'(?#%type:forall* [x ...]
          (?#%type:qual* [constr ...] (?#%type:app* class bare-t ...)))
      type-expansion-ctx)
     #:with link-dict-id (generate-temporary (format-symbol "link:~a" id))
     #:with link-dict-expr #''(l:mod-instance-lookup)
     #:with inst-expr
     #'(class:instance (quote-syntax class*)
                       (list (quote-syntax x*) ...)
                       (list (quote-syntax constr*) ...)
                       (list (quote-syntax bare-t*) ...)
                       (quote-syntax link-dict-id))

     `{()
       ([,#'link-dict-id ,#'link-dict-expr])
       (,#'inst-expr)}]
    ))

;; Generates bindings needed to introduce a module with the
;; given name and signature.
;; Id Id Signature ->
;;   [List [Listof [List Id TransformerStx]]     ; transformer bindings
;;         [Listof [List Id Stx]]                ; value bindings
;;         [Listof TransformerStx]]              ; compile-time effects
(define (generate-module-var-bindings name link-id s)

  (define/syntax-parse unique-symbol (gensym (syntax-e name)))

  (define-values [s-internal-ids s-decls]
    (syntax-parse s
      #:literal-sets [sig-literals]
      [(#%pi-sig . _) (values (hash) (hash))]
      [(#%sig . _) (values (sig-internal-ids s) (sig-decls s))]))

  (match-define
    (list opaque-type-keys data-type-keys alias-type-keys
          constructor-keys value-keys
          submod-keys
          instance-keys)
    (partition-decl-keys
     (list decl-type-opaque? decl-type-data? decl-type-alias?
           decl-constructor? decl-val?
           decl-module?
           decl-instance?)
     s-decls))

  ;; TODO: do something with the instances

  ;; -------------
  ;; generate new ids for each decl

  (define submod-ids
    (generate-prefixed-temporaries (format-symbol "module:~a." name)
                                   submod-keys))

  (define opaque-type-ids
    (generate-prefixed-temporaries (format-symbol "opaque:~a." name)
                                   opaque-type-keys))

  (define data-type-ids
    (generate-prefixed-temporaries (format-symbol "data:~a." name)
                                   data-type-keys))

  (define alias-type-ids
    (generate-temporaries alias-type-keys))

  (define constructor-ids
    (generate-prefixed-temporaries (format-symbol "ctor:~a." name)
                                   constructor-keys))

  (define typed-value-ids
    (generate-prefixed-temporaries (format-symbol "typed:~a." name)
                                   value-keys))

  (define instance-ids
    (generate-prefixed-temporaries (format-symbol "instance:~a." name)
                                   instance-keys))

  ;; -----
  ;; make mappings to the new ids

  (define key->introduced
    (for*/hash
        ([hsh (in-list (list (hash-zip submod-keys submod-ids)
                             (hash-zip opaque-type-keys opaque-type-ids)
                             (hash-zip data-type-keys data-type-ids)
                             (hash-zip alias-type-keys alias-type-ids)
                             (hash-zip constructor-keys constructor-ids)
                             (hash-zip value-keys typed-value-ids)
                             (hash-zip instance-keys instance-ids)))]
         [(key introduced) (in-hash hsh)])
      (values key introduced)))

  (define internal->introduced
    (for/free-id-table ([(key introduced) (in-hash key->introduced)])
      (values (hash-ref s-internal-ids key) introduced)))

  ;; ------
  ;; make intdef context for expanding types

  (define type-expansion-ctx
    (make-type-expansion-context
     s-internal-ids
     (hash-zip submod-keys submod-ids)
     (hash-zip alias-type-keys alias-type-ids)
     (hash-zip opaque-type-keys opaque-type-ids)
     (hash-zip data-type-keys data-type-ids)))

  (define (tec-bind-syntax-bindings bs)
    (syntax-local-bind-syntaxes
     (map first bs)
     #`(values #,@(map second bs))
     type-expansion-ctx))

  (define (generate-only-decls predicate)
    (define-values [stx-bss val-bss effss]
      (for*/lists (stx-bss val-bss effss)
                  ([(key new-id) (in-hash key->introduced)]
                   [decl (in-value (hash-ref s-decls key))]
                   #:when (predicate decl))
        (define decl* (stx-substs decl internal->introduced))
        (match-define `{,trs ,exprs ,effs}
          (generate-decl-bindings key new-id decl*
                                  #:parent-unique-symbol (syntax-e #'unique-symbol)
                                  #:parent-link-id link-id
                                  #:ctx type-expansion-ctx))
        (values trs exprs effs)))
    (list (append* stx-bss) (append* val-bss) (append* effss)))

  ;; ----------
  ;; generate non-value decls (types and modules)

  (match-define `{,type/mod-stx-bindings ,type/mod-val-bindings ,type/mod-effs}
    (generate-only-decls
     (λ (d) (or (decl-type? d)
                (decl-module? d)))))

  ;; ------------------
  ;; generate value decls (val-decl, constructor-decl)

  ;; bind types/mods first
  (tec-bind-syntax-bindings type/mod-stx-bindings)

  (match-define `{,val/ctor-stx-bindings ,val/ctor-val-bindings ,val/ctor-effs}
    (generate-only-decls
     (λ (d) (or (decl-val? d)
                (decl-constructor? d)))))

  ;; ------------------
  ;; generate instance decls

  (match-define `{,inst-stx-bindings ,inst-val-bindings ,inst-effs}
    (generate-only-decls
     (λ (d) (or (decl-instance? d)))))

  ;; generate module-var-transformer binding for module name

  (define/syntax-parse [submod-key ...] submod-keys)
  (define/syntax-parse [submod-id ...] submod-ids)
  (define/syntax-parse [[submod-key/id ...] ...] #'[['submod-key (quote-syntax submod-id)] ...])

  (define/syntax-parse [alias-key ...] alias-type-keys)
  (define/syntax-parse [alias-id ...] alias-type-ids)
  (define/syntax-parse [[alias-key/id ...] ...] #'[['alias-key (quote-syntax alias-id)] ...])

  (define/syntax-parse [op-key ...] opaque-type-keys)
  (define/syntax-parse [op-id ...] opaque-type-ids)
  (define/syntax-parse [[op-key/id ...] ...] #'[['op-key (quote-syntax op-id)] ...])

  (define/syntax-parse [data-key ...] data-type-keys)
  (define/syntax-parse [data-id ...] data-type-ids)
  (define/syntax-parse [[data-key/id ...] ...] #'[['data-key (quote-syntax data-id)] ...])

  (define/syntax-parse [ctor-key ...] constructor-keys)
  (define/syntax-parse [ctor-id ...] constructor-ids)
  (define/syntax-parse [[ctor-key/id ...] ...] #'[['ctor-key (quote-syntax ctor-id)] ...])

  (define/syntax-parse [val-key ...] value-keys)
  (define/syntax-parse [val-id ...] typed-value-ids)
  (define/syntax-parse [[val-key/id ...] ...] #'[['val-key (quote-syntax val-id)] ...])

  (define module-binding
    (list name
          #`(module-var-transformer
             (quote-syntax #,link-id)
             'unique-symbol
             (quote-syntax #,s)
             ; values
             (hash ctor-key/id ... ... val-key/id ... ...)
             ; patterns
             (hash ctor-key/id ... ...)
             ; types
             (hash alias-key/id ... ... op-key/id ... ... data-key/id ... ...)
             ; submods
             (hash submod-key/id ... ...))))

  ;; ------

  (define all-stx-bindings
    (cons module-binding
          (append type/mod-stx-bindings
                  val/ctor-stx-bindings
                  inst-stx-bindings)))

  (define all-val-bindings
    (append type/mod-val-bindings
            val/ctor-val-bindings
            inst-val-bindings))

  (define all-effs
    (append type/mod-effs
            val/ctor-effs
            inst-effs))

  (list all-stx-bindings
        all-val-bindings
        all-effs))

;; Id Id Signature IntDefCtx -> [Listof [List Id Expr-Stx]]
;; adds bindings introduced by the module to the given intdef-ctx. returns
;; an association list of the value bindings that should be introduced along
;; with the module.
;;
;; ASSUME: signature must be expanded. internal-id should be
;; already bound in the intdef-ctx.
(define (syntax-local-bind-module name internal-id signature intdef-ctx)
  ;; NOTE: we ignore the RHS of the value bindings,
  ;;   since this is used for local-expanding, not evalutating.
  (match-define (list ids/transformers ids/exprs effs)
    (generate-module-var-bindings name internal-id signature))
  (define/syntax-parse [([stx-id transformer] ...)
                        ([val-id expr] ...)]
    (list ids/transformers ids/exprs))
  (syntax-local-bind-syntaxes (@ val-id) #f intdef-ctx)
  (syntax-local-bind-syntaxes (@ stx-id) #`(values transformer ...) intdef-ctx)
  (for ([eff (in-list effs)])
    (define inst (syntax-local-eval eff))
    (register-local-class-instance! inst))
  (for/list ([id/expr (in-list ids/exprs)])
    (list (internal-definition-context-introduce intdef-ctx (first id/expr))
          (internal-definition-context-introduce intdef-ctx (second id/expr)))))

;; [Hash Key Id] [Hash Key Id] [Hash Key Id] [Hash Key Id] [Hash Key Id] -> IntDefCtx
(define (make-type-expansion-context internal-ids
                                     submod-key->id
                                     alias-key->id
                                     opaque-key->id
                                     data-key->id)
  (define intdef-ctx
    (syntax-local-make-definition-context))

  ;; assume:
  ;;   each rhs of each hash-table either is already or will soon be
  ;;   bound in the intdef-ctx created here
  ;; create:
  ;;   transformers expanding to those ids in the rhs

  (for ([(key submod-id) (in-hash submod-key->id)])
    (define internal-id (hash-ref internal-ids key))
    (syntax-local-bind-syntaxes
     (list internal-id)
     #`(make-rename-transformer (quote-syntax #,submod-id))
     intdef-ctx))

  (for ([(key alias-id) (in-hash alias-key->id)])
    (define internal-id (hash-ref internal-ids key))
    (syntax-local-bind-syntaxes
     (list internal-id)
     #`(make-rename-transformer (quote-syntax #,alias-id))
     intdef-ctx))

  (for ([(key opaque-id) (in-hash opaque-key->id)])
    (define internal-id (hash-ref internal-ids key))
    (syntax-local-bind-syntaxes
     (list internal-id)
     #`(make-variable-like-transformer (quote-syntax #,opaque-id))
     intdef-ctx))

  (for ([(key data-id) (in-hash data-key->id)])
    (define internal-id (hash-ref internal-ids key))
    (syntax-local-bind-syntaxes
     (list internal-id)
     #`(make-variable-like-transformer (quote-syntax #,data-id))
     intdef-ctx))

  intdef-ctx)

;; ---------------------------------------------------------

;; [Listof [Decl -> Bool]] [Hashof Key Decl] -> [Listof [Listof Key]]
(define (partition-decl-keys decl-predicates decls)
  (define key-decl-predicates
    (for/list ([decl-predicate (in-list decl-predicates)])
      (λ (k d) (decl-predicate d))))
  (map
   hash-keys
   (partition*/hash key-decl-predicates decls)))

;; SymStr [Listof Key] -> [Listof Id]
(define (generate-prefixed-temporaries prefix keys)
  (generate-temporaries/1num
   (for/list ([k (in-list keys)])
     (format-symbol "~a~a" prefix (namespaced-symbol k)))))
