;; The system consists of two talkers, each with a feature-bus coming
;; in, and a comm-line connecting.
;; The state of this system can be described as a list of five elements:
;; (feat1 talker1 comm talker2 feat2)
;; Starting pattern for a instance
;; (  ((f1 1) (f2 2)) imap1  () imap2 () )
;;         or
;; (  ((f1 1) (f2 2)) imap 1 () imap2 ((f1 1) (f2 2)) )
;; Run a test by
;;   update imap1, update imap2: check for quiescence

;; A feature set is defined as follows:
;; There are five roles which can be expressed:
;; subject, object, action, instrument, place
;; Actions come from category " verb" and anywhere from 0-2 can be asserted
;; All others are nouns and 2-4 can be asserted
;; (YES, IT'S PRETTY ARBITRARY: I JUST WANT A DECENT-SIZED SPACE)
;; How big is it? 28 nouns, 7 verbs. avg 4 words/sentence.

;; Normal Set
;; (define nouns '(bob jim mary icepick shovel table lab))
;; (define verbs '(move approach retreat touch eat fear))
;; (define noun-roles '(subject object instrument place))

;; Mega Set - 50 nouns, 20 verbs
(define nouns '(bob jim mary icepick shovel table lab fred bill classroom
                leg cup butterfly dog cat turtle door window car hammer
                keyboard coffee danish pencil pen eraser wall socket book ed
                wheel chainsaw gun kite bedroom shower beach shoe light dark
                hat office house apple banana flea vampire stapler kim joe))
(define verbs '(move approach retreat touch eat fear zap feel fly throw
                catch push hit stab tickle hurt love hate want ignite))
(define noun-roles '(subject object instrument place))

(define (generate-features)
  (let loop ((nnoun (+ 2 (random 3)))
             (nverb (random 3))
             (features '())
             (roles '()))
    (let ((vp (list-transform-negative verbs (lambda (x) (memq x features))))
          (np (list-transform-negative nouns (lambda (x) (memq x features))))
          (nr (list-transform-negative noun-roles (lambda (x) (memq x roles)))))
      (if (<= nnoun 0)
          (if (<= nverb 0)
              (map (lambda (x y) (if (yes? y)
                                (list x y 1)
                                (list x 'verb 1)))
                   (proper-null features)
                   (proper-null roles))
              (loop 0 (- nverb 1)
                    (cons (list-ref vp (random (length vp))) features)
                    (cons #f roles)))
          (loop (- nnoun 1) nverb
                (cons (list-ref np (random (length np))) features)
                (cons (list-ref nr (random (length nr))) roles))))))


(define get-feat1 first)
(define get-imap1 second)
(define get-comm third)
(define get-imap2 fourth)
(define get-feat2 fifth)

;; talk returns: (imap comm)
;; prune-conflits returns: imap
;; listen returns: (imap feat)
;; iterate operates in the following manner:
;; 1. comm lines start blank.
;; 2. have everything speak
;; 3. have everything resolve speaking conflicts
;; 4. have everything listen
;; Unfortunately, if we do this in perfect order, then the synchrony
;; makes it difficult to separate the outgoing and incoming transmissions,
;; most particularly, to determine which bits of a feature are correct.
;; We solve this by ordering the transmissions such that one of the
;; two hears the others transmissions before its tramissions clutters
;; the wires. (If, of course, only one is transmitting, then it cannot
;; be pre-empted.)

(define (iterate state)
  (let* ((tres1 (talk '() (get-feat1 state) (get-imap1 state)))
         (tres2 (talk '() (get-feat2 state) (get-imap2 state)))
         (comms (combine-comms (second tres1) (second tres2)))
         (whofirst (if (and (yes? (second tres1)) (yes? (second tres2)))
                       (if (eq? (random 2) 0) #t #f)
                       (if (yes? (second tres1)) #t #f)))
         (lres1 (listen whofirst (if whofirst comms (second tres2))
                        (get-feat1 state) (first tres1)
                        (get-imap1 state) (second tres1)))
         (lres2 (listen (not whofirst) (if whofirst (second tres1) comms)
                        (get-feat2 state) (first tres2)
                        (get-imap2 state) (second tres2)))
         (newstate (list (second lres1) (first lres1) comms
                         (first lres2) (second lres2))))
    newstate)) ; for now just doing a single learning pass

;; this merge a set of comm lines, turning transmissions into noise
(define (combine-comms . inputs)
  (fold-left
   (lambda (x y)
     (let* ((res1 (split x
                         (lambda (a)
                           (list-search-positive y
                             (lambda (b) (equal? (first b) (first a)))))))
            (res2 (split y
                         (lambda (a)
                           (list-search-positive x
                               (lambda (b) (equal? (first b) (first a)))))))
            (merged (map (lambda (a)
                           (let* ((v2 (second
                                       (list-search-positive (first res2)
                                         (lambda (b)
                                           (equal? (first b) (first a)))))))
                             (list (first a) (if (eq? v2 (second a))
                                                 (second a)
                                                 'x))))
                         (proper-null (first res1)))))
       (append (second res1) (second res2) merged)))
   '()
   (proper-null inputs)))

(define cycle-log (make-log "cycle.log"))

(define (run-cycles initial-state i terminate)
  (let* ((features (generate-features))
         (istest (< 0.8 (random 1.0)))
         (testwhich (< 0.5 (random 1.0)))
         (state (list (if istest (if testwhich features #f) features)
                      (second initial-state)
                      #f
                      (fourth initial-state)
                      (if istest (if testwhich #f features) features))))
    (let ((newstate (iterate state)))
      (if istest
          (if testwhich
              (pp (list i 'test2 (first state) 'returns (fifth newstate)))
              (pp (list i 'test1 (fifth state) 'returns (first newstate))))
          (pp (list i 'train (first state))))
      (cycle-log (list 'step i features state newstate))
      (cycle-log 'flush-buffer)
      (if (or (eq? terminate -1) (>= i terminate))
          newstate
          (run-cycles newstate (+ i 1) terminate)))))

;; test a percent-correct measure
(define (test)
  (define cyclestate (run-cycles '(#f #f #f #f #f) 1 1000)) ;; 300
  (let loop ((i 0) (successes 0) (state cyclestate))
    (let* ((features (generate-features))
           (state (list (if (< i 100) features #f)
                        (second state)
                        #f
                        (fourth state)
                        (if (< i 100) #f features))))
      (if (>= i 200)
          (pp `(Final results: ,successes successes out of ,i trials))
          (let ((newstate (iterate state)))
            (let ((fout (if (< i 100) (fifth newstate) (first newstate))))
              (if (and (equal-set? features (fifth newstate) equal?)
                       (equal-set? features (first newstate) equal?))
                  (begin
                    (cycle-log (list 'test i features 'succeed))
                    (cycle-log 'flush-buffer)
                    (loop (+ i 1) (+ successes 1) newstate))
                  (begin
                    (cycle-log (list 'test i features fout 'fail))
                    (cycle-log 'flush-buffer)
                    (loop (+ i 1) successes newstate)))))))))
