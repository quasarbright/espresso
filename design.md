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
* user can easily create providers which themselves have dependencies
* user can dynamically swap providers
* getting a dependency that hasn't been provided is an error
* distinction between dynamically replacing a provider and supplying duplicate providers
* ~~provider body runs at most once, ever.~~ Never mind, dependent providers require re-evaluation.
* provider order doesn't matter, even if providers have dependencies

would be nice:
* user can group providers into a provider set or something.
* ~~attach a contract for a guard, not just a predicate(?)~~ Guards aren't predicates anymore, they're transformers which can error. Like parameter guards.
* can easily specify a function's dependencies in contracts
* can specify a provider's dependencies in a contract(?)
* can ensure that all dependencies and their providers' dependencies are provided before running a function body. Not sure if possible.

issues with this design:
* user needs to topo sort providers since everything is so dynamic
  * actually, if we invoke providers lazily, order doesn't matter, as long as providers are active
* user needs to make dependencies and providers and specify providers like parameters. repetetive.
  * not if we force a provider to be associated with a dependency
* dependent providers in combination with dynamic re-providing means determining whether to re-evaluate a provider is tricky. For now, we re-evaluate every time you get. A reactive mechanism would be ideal, but then gathering dependencies would be necessary and you'd need to solve a bunch of other problems.

choices:
* should a provider be associated with a dependency? or should you match dependencies to providers when you configure your system? former reduces repetition but makes it harder to re-use providers for different dependencies.
  * I think it's worth it to make them associated with a dependency. they can abstract in other ways if they want to re-use a provider like that.

changes:
* ~~provider bodies run at most once _per with-providers_. If a provider has a dependency, it must be re-evaluated when the dependency changes.~~ They run every 'get' now. Once per 'with' would work, but I wanted to keep it simple bc that would require too much book-keeping.

thoughts:
* Regarding re-evaluation: Since the user explicitly gets, re-evaluation of dependent providers is actually simpler than general reactive programming. If you can build the dependency graph, then instead of going through the graph and _triggering_ re-evaluations, you can just go through and _mark_ providers as stale. You can just do a naive, simpler traversal for this. Next time their value is requested, if they are stale, re-evaluate and mark them as not stale. Otherwise use their stored result. The graph would cost memory, would require book keeping, and you'd have to be careful about garbage collection, but you'd minimize re-evaluations.
  * what if we had different kinds of providers? Like we could have a strict provider, a lazy provider, and a dependent provider, each with their own re-evaluation rules.
