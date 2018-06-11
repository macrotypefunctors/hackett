#lang racket/base
(provide
 generate-module-var-bindings
 syntax-local-bind-module)

(require
 racket/list
 racket/match
 racket/syntax
 syntax/parse
 (only-in syntax/parse [attribute @])
 syntax/parse/class/local-value
 hackett/private/util/stx
 "expand-check-prop.rkt"
 "../prop-reintroducible-dot-type.rkt"
 "../prop-dot-accessible.rkt"
 "../util/stx.rkt"
 "../util/stx-subst.rkt"
 "../util/stx-traverse.rkt"
 "../util/hash.rkt"
 "../util/partition.rkt"
 "../namespace/reqprov.rkt"
 "../namespace/namespace.rkt"
 (for-template "../rep/sig-literals.rkt"
               (only-in (unmangle-in "../dot/dot-t.rkt") [#%dot #%dot_τ])
               (only-in (unmangle-in "../dot/dot-m.rkt") [#%dot #%dot_m])
               (only-in racket/base #%app quote)
               (prefix-in l: "../link/mod.rkt"))
 (for-template (except-in hackett/private/type-language
                          type-namespace-introduce
                          value-namespace-introduce
                          ~type)
               (only-in hackett/private/base
                        make-typed-var-transformer)
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

    (define (look-for-bool stx)
      (define bx (box #f))
      (let trav ([stx stx])
        (if (and (identifier? stx)
                 (eq? (syntax-e stx) 'Bool))
            (begin (set-box! bx stx) stx)
            (traverse-stx/recur stx trav)))
      (unbox bx))

    (define bool (look-for-bool sig))
    (when bool
      (printf "found bool in ~a\n" (syntax-e stx))
      (printf "module var bool in type ns? ~a\n"
              (bound-identifier=? bool (type-namespace-introduce bool)))
      (printf "module var bool in sig ns? ~a\n"
              (bound-identifier=? bool (signature-namespace-introduce bool))))

    ((make-variable-like-transformer
      (λ (id)
        (attach-sig (replace-stx-loc x- id)
                    (strengthen-signature id sig))))
     stx))
  #:property prop:dot-accessible
  (λ (self)
    (match-define (module-var-transformer _ sym _ hv hp ht hm) self)
    (dot-accessible
     sym
     (λ (val-key) (hash-ref hv val-key #f))
     (λ (pat-key) (hash-ref hp pat-key #f))
     (λ (type-key) (hash-ref ht type-key #f))
     (λ (mod-key) (hash-ref hm mod-key #f)))))

(struct opaque-type-constructor
  [module-sym external-sym]
  #:property prop:procedure
  (λ (self stx)
    ((make-variable-like-transformer
      (λ (id) #`(#%type:con #,id)))
     stx))
  #:property prop:reintroducible-dot-type
  (λ (self)
    (reintroducible-dot-type
     (opaque-type-constructor-module-sym self)
     (opaque-type-constructor-external-sym self))))

(struct data-type-constructor/reintroducible
  data-type-constructor
  [module-sym external-sym]
  #:property prop:reintroducible-dot-type
  (λ (self)
    (reintroducible-dot-type
     (data-type-constructor/reintroducible-module-sym self)
     (data-type-constructor/reintroducible-external-sym self))))


;; Id Signature -> Signature
;; Strengthen the signature, recursively replacing opaque types with
;; self references to preserve the identity of the original module.
;; `self-path` should be bound with the given signature, so that
;; the strengthened signature is valid for aliases of the path.
(define (strengthen-signature self-id signature)
  (syntax-parse signature
    #:literal-sets [sig-literals]
    [(sig:#%sig internal-ids:hash-literal
                decls:hash-literal)

     ;#:with self-path self-path*
     #:with self-type:dot-accessible-id/type self-id
     #:with self-module:dot-accessible-id/module self-id

     #:do [(define (strengthen-decl key d)
             (syntax-parse d
               #:literal-sets [sig-literals]
               [(type-decl:#%type-decl (#%opaque))
                ;#:with sym (namespaced-symbol key)
                #:with d-id ((@ self-type.key->id) key)
                #'(type-decl (#%alias d-id))]

               [(mod-decl:#%module-decl submod-signature)
                ; #:with sym (namespaced-symbol key)
                #:with d-id ((@ self-module.key->id) key)
                #:with strong-sig (strengthen-signature #'d-id #'submod-signature)
                #'(mod-decl strong-sig)]

               [_ d]))]

     #:with decls-
     (for/hash ([(key decl) (in-hash (@ decls.value))])
       (values key (strengthen-decl key decl)))

     #'(sig internal-ids decls-)]

    [(#%pi-sig . _) signature]))


;; Generates bindings needed to introduce a module with the
;; given name and signature.
;; Id Id Signature ->
;;   [List [Listof [List Id TransformerStx]]     ; transformer bindings
;;         [Listof [List Id Stx]]]               ; value binding
(define (generate-module-var-bindings name internal-id s)

  (define/syntax-parse unique-symbol (gensym (syntax-e name)))

  (define-values [s-internal-ids s-decls]
    (syntax-parse s
      #:literal-sets [sig-literals]
      [(#%pi-sig . _) (values (hash) (hash))]
      [(#%sig . _) (values (sig-internal-ids s) (sig-decls s))]))

  (match-define
    (list opaque-type-keys data-type-keys alias-type-keys
          constructor-keys value-keys
          submod-keys)
    (partition-decl-keys
     (list decl-type-opaque? decl-type-data? decl-type-alias?
           decl-constructor? decl-val?
           decl-module?)
     s-decls))

  (define submod-ids
    (generate-prefixed-temporaries (format-symbol "module:~a." name)
                                   submod-keys))
  (define submod-val-ids (generate-temporaries submod-keys))

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

  (define constructor-val-ids (generate-temporaries constructor-keys))
  (define constructor-pat-ids (generate-temporaries constructor-keys))

  (define constructor-key->id
    (hash-zip constructor-keys constructor-ids))

  (define typed-value-ids
    (generate-prefixed-temporaries (format-symbol "typed:~a." name)
                                   value-keys))
  (define untyped-value-ids
    (generate-prefixed-temporaries (format-symbol "untyped:~a." name)
                                   value-keys))

  ;; make intdef context for expanding types

  (define type-expansion-ctx
    (make-type-expansion-context
     s-internal-ids
     (hash-zip submod-keys submod-ids)
     (hash-zip alias-type-keys alias-type-ids)
     (hash-zip opaque-type-keys opaque-type-ids)
     (hash-zip data-type-keys data-type-ids)))

  (define (expand-type/rem-ctx t)
    (internal-definition-context-introduce
     type-expansion-ctx
     (expand-type t type-expansion-ctx)
     'remove))

  (define (tec-bind-syntax-bindings bs)
    (syntax-local-bind-syntaxes
     (map first bs)
     #`(values #,@(map second bs))
     type-expansion-ctx))

  (define internal->introduced
    (for*/free-id-table
        ([hsh (in-list (list (hash-zip submod-keys submod-ids)
                             (hash-zip alias-type-keys alias-type-ids)
                             (hash-zip opaque-type-keys opaque-type-ids)
                             (hash-zip data-type-keys data-type-ids)))]
         [(key introduced) (in-hash hsh)])
      (values (hash-ref s-internal-ids key) introduced)))

  ;; generate bindings for submodules

  (define submod-internal-id-bindings/val
    (for/list ([key (in-list submod-keys)]
               [id (in-list submod-val-ids)])
      (list id #`(l:mod-submod-ref #,internal-id '#,(namespaced-symbol key)))))

  ;; generate the bindings for submodules using a recursive
  ;; call to generate-module-var-bindings
  (match-define (list (list submod-bindingss/stx submod-bindingss/val) ...)
    (for/list ([key (in-list submod-keys)]
               [id (in-list submod-ids)]
               [internal-id (in-list submod-val-ids)])
      (define/syntax-parse ({~literal #%module-decl} sub-s)
        (hash-ref s-decls key))
      (define sub-s*
        (stx-substs #'sub-s internal->introduced))
      (generate-module-var-bindings id internal-id sub-s*)))

  (define submod-bindings/stx (append* submod-bindingss/stx))
  (define submod-bindings/val
    (append* submod-internal-id-bindings/val submod-bindingss/val))

  (define/syntax-parse [submod-key ...] submod-keys)
  (define/syntax-parse [submod-id ...] submod-ids)
  (define/syntax-parse [[submod-key/id ...] ...]
    #'[['submod-key (quote-syntax submod-id)] ...])

  ;; generate aliases for alias types

  (define alias-type-bindings
    (for/list ([key (in-list alias-type-keys)]
               [id (in-list alias-type-ids)])
      (define/syntax-parse ({~literal #%type-decl} ({~literal #%alias} t))
        (hash-ref s-decls key))
      (define/syntax-parse t*
        (stx-substs #'t internal->introduced))
      (list id
            #'(make-variable-like-transformer (quote-syntax t*)))))

  (define/syntax-parse [alias-key ...] alias-type-keys)
  (define/syntax-parse [alias-id ...] alias-type-ids)
  (define/syntax-parse [[alias-key/id ...] ...] #'[['alias-key (quote-syntax alias-id)] ...])

  ;; generate #%type:con's for opaque types

  (define opaque-type-bindings
    (for/list ([key (in-list opaque-type-keys)]
               [id (in-list opaque-type-ids)])
      (list id
            #`(opaque-type-constructor
               'unique-symbol
               '#,(namespaced-symbol key)))))

  (define/syntax-parse [op-key ...] opaque-type-keys)
  (define/syntax-parse [op-id ...] opaque-type-ids)
  (define/syntax-parse [[op-key/id ...] ...] #'[['op-key (quote-syntax op-id)] ...])

  ;; generate #%type:con's for data types

  (define data-type-bindings
    (for/list ([key (in-list data-type-keys)]
               [id (in-list data-type-ids)])
      ;; get the constructors
      (define/syntax-parse
        ({~literal #%type-decl} ({~literal #%data} c ...))
        (hash-ref s-decls key))

      ;; find the "new" constructor ids for the module being introduced
      (define/syntax-parse
        [c-binding-id ...]
        (for/list ([c-id (in-list (@ c))])
          (or (for/first ([(key id) (in-hash s-internal-ids)]
                          #:when (free-identifier=? c-id id))
                (hash-ref constructor-key->id key))
              (raise-syntax-error c-id
                "constructor declaration not found in signature"))))

      ;; TODO: get the actual type-var arity
      (define type-var-arity 0)

      (list id
            #`(data-type-constructor/reintroducible
               (quote-syntax (#%type:con #,id))        ; type
               '#,type-var-arity                       ; arity
               (list (quote-syntax c-binding-id) ...)  ; constructor ids
               #f                                      ; fixity
               'unique-symbol                          ; module sym
               '#,(namespaced-symbol key)              ; external key
               ))))

  (define/syntax-parse [data-key ...] data-type-keys)
  (define/syntax-parse [data-id ...] data-type-ids)
  (define/syntax-parse [[data-key/id ...] ...] #'[['data-key (quote-syntax data-id)] ...])

  (tec-bind-syntax-bindings
   (append submod-bindings/stx
           alias-type-bindings opaque-type-bindings data-type-bindings))

  ;; generate data-constructor bindings for data types

  (define constructor-val-bindings
    (for/list ([key (in-list constructor-keys)]
               [id (in-list constructor-val-ids)])
      (list id #`(l:mod-value-ref #,internal-id '#,(namespaced-symbol key)))))

  (define constructor-pat-bindings
    (for/list ([key (in-list constructor-keys)]
               [id (in-list constructor-pat-ids)])
      (list id #`(l:mod-pattern-ref #,internal-id '#,(namespaced-symbol key)))))

  (define constructor-bindings
    (for/list ([key (in-list constructor-keys)]
               [id (in-list constructor-ids)]
               [pat-id (in-list constructor-pat-ids)]
               [val-id (in-list constructor-val-ids)])
      (define/syntax-parse [pat-id* val-id*]
        (list pat-id val-id))
      (define/syntax-parse
        ({~literal #%constructor-decl} t)
        (hash-ref s-decls key))
      (define/syntax-parse t*
        (expand-type/rem-ctx #'t))

      (list id
            #'(data-constructor
               (make-typed-var-transformer (quote-syntax val-id*)
                                           (quote-syntax t*))
               (quote-syntax t*)
               (λ (sub-pats) #`(l:app/pat-info pat-id* #,sub-pats))
               #f))))

  (define/syntax-parse [ctor-key ...] constructor-keys)
  (define/syntax-parse [ctor-id ...] constructor-ids)
  (define/syntax-parse [[ctor-key/id ...] ...] #'[['ctor-key (quote-syntax ctor-id)] ...])

  ;; generate mod-value-ref expressions for values

  (define typed-value-bindings
    (for/list ([key (in-list value-keys)]
               [id (in-list typed-value-ids)]
               [un (in-list untyped-value-ids)])
      (define/syntax-parse ({~literal #%val-decl} t)
        (hash-ref s-decls key))
      (define/syntax-parse t*
        (expand-type/rem-ctx #'t))

      (list id
            #`(make-typed-var-transformer (quote-syntax #,un)
                                          (quote-syntax t*)))))

  (define untyped-value-bindings
    (for/list ([key (in-list value-keys)]
               [id (in-list untyped-value-ids)])
      (list id #`(l:mod-value-ref #,internal-id '#,(namespaced-symbol key)))))

  (define/syntax-parse [val-key ...] value-keys)
  (define/syntax-parse [val-id ...] typed-value-ids)
  (define/syntax-parse [[val-key/id ...] ...] #'[['val-key (quote-syntax val-id)] ...])

  ;; generate module-var-transformer binding for module name

  (define module-binding
    (list name
          #`(module-var-transformer
             (quote-syntax #,internal-id)
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

  (define all-syntax-bindings
    (cons module-binding
          (append submod-bindings/stx
                  alias-type-bindings
                  opaque-type-bindings
                  data-type-bindings
                  constructor-bindings
                  typed-value-bindings)))

  (define all-value-bindings
    (append submod-bindings/val
            constructor-val-bindings
            constructor-pat-bindings
            untyped-value-bindings))

  (list all-syntax-bindings
        all-value-bindings))

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
  (match-define (list ids/transformers ids/exprs)
    (generate-module-var-bindings name internal-id signature))
  (define/syntax-parse [([stx-id transformer] ...)
                        ([val-id expr] ...)]
    (list ids/transformers ids/exprs))
  (syntax-local-bind-syntaxes (@ val-id) #f intdef-ctx)
  (syntax-local-bind-syntaxes (@ stx-id) #`(values transformer ...) intdef-ctx)
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
     #`(make-variable-like-transformer (quote-syntax #,alias-id))
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
