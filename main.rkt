#lang racket/base

(module+ test (require rackunit))
(provide)

(require racket/function
         racket/promise
         racket/set
         racket/hash)

(struct dependency [guard] #:transparent)
; A Dependency is a
#;(dependency predicate?)
; Represents an injected dependency whose values must satisfy 'guard'

(struct provider [dependency thunk] #:transparent)
; A Provider is a
#;(provider Dependency (-> any/c))
; Represents a provider for 'dependency'
; Provides the value of 'promise'

#;(hash/c dependency? promise?)
(define dependencies (make-parameter (hasheq)))

#;([predicate?] -> dependency?)
(define (make-dependency [guard (const #t)])
  (dependency guard))

#;(dependency? -> any/c)
; Get the value of a dependency according to its current provider.
; Error if not provided.
(define (dependency-get dep)
  (define promise (hash-ref (dependencies)
                            dep
                            (lambda () (error 'depdendency-get "dependency not provided: ~a" dep))))
  (force promise))

(define-syntax-rule (make-provider dep body) (provider dep (lambda () body)))

(define-syntax-rule
  (with-providers (provider ...)
    body
    ...)
  (with-providers/proc (seteq provider ...)
    (lambda () body ...)))

#;((sequence/c provider?) (-> any) -> any)
; evaluate thnk with providers active
(define (with-providers/proc prvs thnk)
  (parameterize ([dependencies (hash-union (dependencies) (providers->dependency-hash prvs))])
    (thnk)))

#;((sequence/c provider?) -> (hash/c dependency? promise?))
; convert the sequence of providers to a mapping from dependencies to a promise holding its provided value.
(define (providers->dependency-hash prvs)
  (for/hasheq ([prv prvs])
    (values (provider-dependency prv) (delay ((provider-thunk prv))))))

(module+ test
  (test-case "simple banking"
    (define deposits-dep (make-dependency number?))
    (define withdrawals-dep (make-dependency number?))
    (define balance-dep (make-dependency number?))

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
  (test-case "provider body runs at most once per with-providers"
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
    (define num-dep (make-dependency number?))
    (define bignum-dep (make-dependency number?))
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
                  4)))
