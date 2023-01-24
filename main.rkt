#lang racket/base

(module+ test (require rackunit))
(require racket/contract)
(provide (contract-out
          [rename make-dependency dependency (->* () ((-> any/c any/c)) dependency?)]
          [dependency-get (-> dependency? any)]
          [dependency-provided? (-> dependency? boolean?)]
          [assert-dependency-provided (->* (dependency?) (symbol?) void?)]
          [check-dependencies-provided (->* () #:rest (listof dependency?) (or/c #t string?))])
         (rename-out [make-provider provider])
         dependency?
         provider?
         with-providers)

(require racket/list
         racket/hash
         (for-syntax syntax/parse racket/base))

(struct dependency [guard] #:transparent)
; A Dependency is a
#;(dependency (any/c -> any))
; Represents an injected dependency whose provided values pass through 'guard'

(struct provider [dependency thunk] #:transparent)
; A Provider is a
#;(provider Dependency (-> any/c))
; Represents a provider for 'dependency'
; Provides the value of 'thnk'

#;(hash/c dependency? (-> any/c))
(define dependencies (make-parameter (hasheq)))

#;([predicate?] -> dependency?)
(define (make-dependency [guard (lambda (x) x)])
  (dependency guard))

#;(dependency? -> any/c)
; Get the value of a dependency according to its current provider.
; Applies the value to the guard procedure.
; Error if not provided.
(define (dependency-get dep)
  (define thnk (hash-ref (dependencies)
                         dep
                         (lambda () (error 'depdendency-get "dependency not provided: ~a" dep))))
  (define val (thnk))
  (define guard (dependency-guard dep))
  (guard val))

#;(dependency? -> boolean?)
; Is the dependenct currently provided?
; Does not evaluate the provider's value.
(define (dependency-provided? dep)
  (hash-has-key? (dependencies) dep))

#;(dependency? [symbol?] -> void?)
; Error if dep is not currently provided.
; Does not evaluate the provider's value.
(define (assert-dependency-provided dep [who 'assert-dependency-provided])
  (unless (dependency-provided? dep)
    (error who "dependency not provided: ~a" dep)))

#;(dependency? ... -> (or/c #t string?))
; #t is the dependencies are provided, an error message string otherwise.
; Useful for #:pre/desc in a ->* contract.
(define (check-dependencies-provided . deps)
  (let/cc k
    (for ([dep deps])
      (unless (dependency-provided? dep)
        (k (format "dependency not provided: ~a" dep))))
    #t))

(define-syntax make-provider
  (syntax-parser
    [(_ (~var dep (expr/c #'dependency?)) body)
     #'(provider dep.c (lambda () body))]))

(define-syntax with-providers
  (syntax-parser
    [(_ ((~var provider (expr/c #'provider?)) ...) body ...+)
     #'(with-providers/proc (list provider.c ...)
         (lambda () body ...))]))

#;((listof provider?) (-> any) -> any)
; Evaluate thnk with providers active.
; Error if there are multiple providers of the same dependency.
(define (with-providers/proc prvs thnk)
  (define deps (map provider-dependency prvs))
  (define dup (check-duplicates deps eq?))
  (when dup
    (error 'with-providers "multiple providers of the same dependency were supplied: ~a" dup))
  (parameterize ([dependencies (hash-union (dependencies)
                                           (providers->dependency-hash prvs)
                                           #:combine (lambda (a b) b))])
    (thnk)))

#;((sequence/c provider?) -> (hash/c dependency? (-> any/c)))
; convert the sequence of providers to a mapping from dependencies to a thunk of its provided value.
(define (providers->dependency-hash prvs)
  (for/hasheq ([prv prvs])
    (values (provider-dependency prv) (provider-thunk prv))))

(module+ test
  (test-case "simple banking"
    (define deposits-dep (make-dependency))
    (define withdrawals-dep (make-dependency))
    (define balance-dep (make-dependency))

    (define (get-balance)
      (dependency-get balance-dep))

    ; order doesn't matter
    (define balance-provider (make-provider balance-dep (- (dependency-get deposits-dep)
                                                           (dependency-get withdrawals-dep))))
    (define deposits-provider (make-provider deposits-dep 4))
    (define withdrawals-provider (make-provider withdrawals-dep 1))

    ; order doesn't matter
    (check-equal? (with-providers (balance-provider deposits-provider withdrawals-provider)
                    (get-balance))
                  3))
  (test-case "provide and get"
    (define dep (make-dependency))
    (define prv (make-provider dep 42))
    (check-exn #rx"not provided" (lambda () (dependency-get dep)))
    (check-equal? (with-providers (prv) (dependency-get dep))
                  42))
  ; currently, we over-evaluate to accomodate dependent providers
  #;(test-case "provider body runs at most once per with-providers"
    (define eval-count 0)
    (define dep (make-dependency))
    (define prv (make-provider dep (begin (set! eval-count (add1 eval-count))
                                          42)))
    (check-equal? eval-count 0)
    (with-providers (prv) (void))
    ; providing doesn't force the body
    (check-equal? eval-count 0)
    (check-equal? (with-providers (prv) (list (dependency-get dep) (dependency-get dep)))
                  '(42 42))
    ; getting forces the body, only runs once, even for multiple gets
    (check-equal? eval-count 1)
    ; provider re-evaluates for a new with-providers
    (check-equal? (with-providers (prv) (list (dependency-get dep) (dependency-get dep)))
                  '(42 42))
    (check-equal? eval-count 2))
  (test-case "provider with dependency"
    (define num-dep (make-dependency))
    (define bignum-dep (make-dependency))
    (define num-provider (make-provider num-dep 1))
    (define other-num-provider (make-provider num-dep 3))
    (define bignum-provider (make-provider bignum-dep (add1 (dependency-get num-dep))))
    (check-exn #rx"not provided"
               (lambda () (with-providers (bignum-provider) (dependency-get bignum-dep))))
    (check-exn #rx"not provided"
               (lambda () (with-providers (num-provider) (dependency-get bignum-dep))))
    ; order doesn't matter
    (check-equal? (with-providers (bignum-provider num-provider) (dependency-get bignum-dep))
                  2)
    ; swapping indirect provider changes dependent provider's value
    ; regression test: the provider's first resolution was used for all future with-provider expressions.
    ; A little too lazy.
    (check-equal? (with-providers (bignum-provider other-num-provider) (dependency-get bignum-dep))
                  4))
  (test-case "change provider"
    (define dep (make-dependency))
    (define prv (make-provider dep 1))
    (define other-prv (make-provider dep 2))
    (check-equal? (with-providers (prv)
                    (list (dependency-get dep)
                          (with-providers (other-prv) (dependency-get dep))
                          (dependency-get dep)))
                  '(1 2 1)))
  (test-case "change provider with dependent provider"
    (define num-dep (make-dependency))
    (define bignum-dep (make-dependency))
    (define num-provider (make-provider num-dep 1))
    (define other-num-provider (make-provider num-dep 3))
    (define bignum-provider (make-provider bignum-dep (add1 (dependency-get num-dep))))
    (check-equal? (with-providers (bignum-provider num-provider)
                    (list (dependency-get bignum-dep)
                          ; don't re-provide bignum
                          (with-providers (other-num-provider)
                            (dependency-get bignum-dep))
                          (dependency-get bignum-dep)))
                  '(2 4 2)))
  (test-case "error dependency guard"
    (define num-dep (make-dependency (lambda (x) (if (number? x) x (error "expected: number?")))))
    (define prv (make-provider num-dep "two"))
    ; error on get, not provide
    (check-pred void? (with-providers (prv) (void)))
    (check-exn #rx"expected: number?" (lambda () (with-providers (prv) (dependency-get num-dep)))))
  (test-case "transforming dependency guard"
    (define dep (make-dependency add1))
    (define prv (make-provider dep 1))
    (check-equal? (with-providers (prv) (list (dependency-get dep) (dependency-get dep)))
                  '(2 2)))
  (test-case "supply non-dependency to make-provider"
    (check-exn #rx"expected: dependency?" (lambda () (make-provider 'not-a-dependency 42))))
  (test-case "supply non-provider to with-providers"
    (check-exn #rx"" (lambda () (with-providers ('not-a-provider) (void)))))
  (test-case "duplicate provider"
    (define dep (make-dependency))
    (define prv (make-provider dep 1))
    (check-exn #rx"multiple providers of the same dependency were supplied"
               (lambda () (with-providers (prv prv) dep))))
  (test-case "two different providers of same dependency"
    (define dep (make-dependency))
    (define prv1 (make-provider dep 1))
    (define prv2 (make-provider dep 2))
    (check-exn #rx"multiple providers of the same dependency were supplied"
               (lambda () (with-providers (prv1 prv2) dep))))
  (test-case "dependency-provided?"
    (define dep (make-dependency))
    (check-false (dependency-provided? dep))
    (define prv (make-provider dep 1))
    (check-true (with-providers (prv) (dependency-provided? dep)))
    (check-false (dependency-provided? dep)))
  (test-case "assert-dependency-provided"
    (define dep (make-dependency))
    (check-exn #rx"not provided" (lambda () (assert-dependency-provided dep)))
    (define prv (make-provider dep 1))
    (with-providers (prv) (assert-dependency-provided dep))
    (check-exn #rx"not provided" (lambda () (assert-dependency-provided dep)))))
