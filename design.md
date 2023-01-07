design notes

```racket
;; sketch example of a TODO app with persistent storage
;; uses a data access object (dao) and a metric service, both of which require a security key

(define dao-dep (dep dao?))
(define metric-service-dep (dep metric-service?))
(define security-key-dep (dep security-key?))

(provide (contract-out [get-todo (and/c (-> integer? any/c) (dep/c dao-dep metric-service-dep))]))
; get item by id
(define (get-todo id)
  (define dao (get-dep dao-dep))
  (define metric-service (get-dep metric-service-dep))
  (record-get-todo metric-service id)
  (get-record dao id))

; create or update item
(define (put-todo todo)
  (define dao (get-dep dao-dep))
  (define metric-service (get-dep metric-service-dep))
  (define todo-validator (get-dep validator-dep))
  (validate-todo todo-validator todo)
  (record-put-todo metric-service todo)
  (put-record dao todo))

(define dao-provider (provider (make-dao dao-dep (get-dep security-key-dep))))
(define metric-service-provider (provider metric-service-dep (make-metric-service (get-dep security-key-dep))))
(define security-key-provider (provider security-key-dep (make-security-key "password123")))
; order doesn't matter
(define providers (provider-set dao-provider metric-service-provider security-key-provider))

(define (main)
  (with-providers (providers)
    (define todo (get-todo 123))
    (put-todo (mark-done todo))))
```

general constraints:
* user can have multiple providers for a dependency, but only one active at a time
* duplicate active providers is an error
* user can use dependencies without worrying about providers
* user just has to get dependencies, doesn't have to list them all
* user can create providers easily and have freedom over how to organize that
* user can easily create providers which have dependencies
* user can dynamically swap providers
* getting a dependency that hasn't been provided is an error
* distinction between dynamically replacing a provider and supplying duplicate providers
* provider body runs at most once, ever
* provider order doesn't matter, even if providers have dependencies

would be nice:
* user can group providers into a provider set or something.
* attach a contract for a guard, not just a predicate(?)
* can easily specify a function's dependencies in contracts
* can specify a provider's dependencies in a contract(?)

issues with this design:
* user needs to topo sort providers since everything is so dynamic
  * actually, if we invoke providers lazily, order doesn't matter, as long as providers are active
* user needs to make dependencies and providers and specify providers like parameters. repetetive.
  * not if we force a provider to be associated with a dependency

choices:
* should a provider be associated with a dependency? or should you match dependencies to providers when you configure your system? former reduces repetition but makes it harder to re-use providers for different dependencies.
  * I think it's worth it to make them associated with a dependency. they can abstract in other ways if they want to re-use a provider like that.

changes:
* provider bodies run at most once _per with-providers_. If a provider has a dependency, it must be re-evaluated
when the dependency changes. you'd need reactive programming to do this optimally.
