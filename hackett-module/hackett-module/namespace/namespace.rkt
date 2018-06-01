#lang racket/base

(provide module-namespace-introduce
         signature-namespace-introduce
         value-namespace-introduce
         type-namespace-introduce
         ~module ~signature ~value ~type)

(require syntax/parse
         (for-template (prefix-in hkt: hackett/private/type-language))
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse))

;; ---------------------------------------------------------

;; Syntax -> Syntax
(define (module-namespace-introduce stx)
  (module-scope-introducer
   (remove-signature-scope (remove-value-scope (remove-type-scope stx)))
   'add))

;; Syntax -> Syntax
(define (signature-namespace-introduce stx)
  (signature-scope-introducer
   (remove-module-scope (remove-value-scope (remove-type-scope stx)))
   'add))

;; Syntax -> Syntax
(define (value-namespace-introduce stx)
  (hkt:value-namespace-introduce
   (remove-signature-scope (remove-module-scope stx))))

;; Syntax -> Syntax
(define (type-namespace-introduce stx)
  (hkt:type-namespace-introduce
   (remove-signature-scope (remove-module-scope stx))))

;; ---------------------------------------------------------

(begin-for-syntax
  ;; Id-refering-to-Stx→Stx-at-phase-0 -> StxParsePatternExpander
  (define (make-namespace-pattern-expander intro-id)
    (define/syntax-parse intro:id intro-id)
    (pattern-expander
     (syntax-parser
       [(_ pat)
        #:with tmp (generate-temporary)
        #'{~and tmp {~parse pat (intro #'tmp)}}]))))

(define-syntax ~module
  (make-namespace-pattern-expander #'module-namespace-introduce))

(define-syntax ~signature
  (make-namespace-pattern-expander #'signature-namespace-introduce))

(define-syntax ~value
  (make-namespace-pattern-expander #'value-namespace-introduce))

(define-syntax ~type
  (make-namespace-pattern-expander #'type-namespace-introduce))

;; ---------------------------------------------------------

;; Syntax (U 'add 'remove) -> Syntax
(define module-scope-introducer
  (make-interned-syntax-introducer 'hackett-module))

;; Syntax (U 'add 'remove) -> Syntax
(define signature-scope-introducer
  (make-interned-syntax-introducer 'hackett-signature))

;; [Syntax -> Syntax] -> [Syntax -> Syntax]
(define (make-scope-remover introducer)
  (define id (datum->syntax #f '||))
  (define delta (make-syntax-delta-introducer (introducer id) id))
  (define (remove-scope stx)
    (delta stx 'remove))
  remove-scope)

(define remove-value-scope (make-scope-remover hkt:value-namespace-introduce))
(define remove-type-scope (make-scope-remover hkt:type-namespace-introduce))

(define (remove-module-scope stx) (module-scope-introducer stx 'remove))
(define (remove-signature-scope stx) (signature-scope-introducer stx 'remove))

