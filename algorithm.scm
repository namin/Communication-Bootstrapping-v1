;; Talker
;; Wires: only driven values are listed:
;; (  (line 1/-1/X) )
;; NOTE: Right now, all asserted feature lines are assumed to be positive.

(define num-wires 1000) ;; 10000
(define num-wires-per-symbol 20) ;; 100
(define min-wires-per-symbol 5) ;; 20
(define percent-match 0.8)
(define unary-percent-match 0.05)

;; internal-map bundles as follows:
;; (symbol (certaincommlines) (uncertaincommlines) uncertainfactor)
;; When listening, both certain and uncertain comm lines are considered
;; When talking, only certain comm lines are used
;; An uncertain comm line becomes certain after surviving n transmissions
(define certainty-threshold 4)
(define certain-pruning-threshold 6)

;; line abstractions
;; returns a line-bundle with the new value
;; On a conflict between values, the line become unasserted
;; This is represented by an "X" on the line
(define (assert-line line-bundle line value)
  (let* ((result (split line-bundle
                        (lambda (x) (eq? (first x) line))))
         (pos (first result)) ;; should have 0 or 1 entries
         (neg (second result)))
    (if (yes? pos)
        (if (equal? (second (car pos)) value)
            line-bundle
            (consp (list line 'x) neg))
        (consp (list line value) line-bundle))))

;; this can put inflections on lines as well as simple values
(define (assert-feature-line line-bundle line inflection value)
  (let* ((result (split line-bundle
                        (lambda (x) (eq? (first x) line))))
         (pos (first result)) ;; should have 0 or 1 entries
         (neg (second result)))
    (if (and (yes? pos) (or (no? inflection)
                            (equal? (second (car pos)) inflection)))
        (if (or (no? inflection) (equal? (third (car pos)) value))
            line-bundle
            (consp (list line inflection 'x) neg))
        (consp (list line inflection value) line-bundle))))

;; returns only the non-conflicted bits
(define (clean-bits line-bundle)
  (list-transform-negative line-bundle (lambda (x) (eq? (second x) 'x))))

;; returns the value found on the line, or zero if it's not asserted
;; Conflicted bits *are* returned as an "X"
;; Thus, there are four possible results: 1,-1,0,x
(define (test-line line-bundle line)
  (let ((result (list-search-positive line-bundle
                  (lambda (x) (eq? (first x) line)))))
    (if (yes? result) (second result) 0)))

;; can check the inflection on feature-lines
(define (test-inflection line-bundle line)
  (let ((result (list-search-positive line-bundle
                  (lambda (x) (eq? (second x) line)))))
    (if (yes? result) (second result) 0)))

;; listen & talk return (cons internal-map foo-line)
;; Two cases to handle:
;; 1. Features in internal-map
;;    If asserted on feature-lines: modify internal-map for conflicts
;;    else: if enough matches, assert on feature-lines
;; 2. Features asserted, but not in internal-map
;;    add new features to internal-map
;; Returns (cons internal-map feature-lines)
;; added spokefirst
;; changed "new features" to be features *heard* for the first time,
;;   regardless of whether they've been spoken before.
;; NOTE as it's set up, there's some overkill. Really, the critical
;;   factor is whether c-a is empty set of not a symbol creation.
;;   However, I'm keeping it this way since it's a bit more "pure"
(define (listen spokefirst comm-lines feature-lines internal-map pre-map comm-spoke)
  (let* ((smap (if (yes? internal-map) (second internal-map) #f))
         (nmap (if (yes? internal-map) (first internal-map) #f))
         (res (split smap
                     (lambda (x)
                       (and (equal? 0 (test-line feature-lines (first x)))
                            (equal? 0 (test-inflection feature-lines (first x)))))))
         (stimfeat (second res))
         (smap-check (first res))
         (res2 (split stimfeat
                      (lambda (x)
                        (or spokefirst
                            (< 0 (find-symbol-certainty internal-map (first x)))))))
         (newfeat (second res2))
         (c-a (list-transform-negative comm-lines
                (lambda (x) (member x comm-spoke))))
         (newfeat
          (map (lambda (x)
                 (list (car x) (find-symbol-codes internal-map (car x))
                       (map first c-a) 1))
               (proper-null (second res2))))
         (smap-assert (if (no? spokefirst)
                          (listen-resolve-conflicts spokefirst comm-lines
                                                    (first res2))
                          (first res2))))
    (list (list (if (no? spokefirst)
                    (nsquelch (update-nmap nmap comm-lines
                                           feature-lines stimfeat))
                    nmap)
                (appendp smap-check smap-assert newfeat))
          (listen-assert-features comm-lines feature-lines nmap smap-check))))

;; returns a new internal-map with the code changed/added
;; If a code-mapping drops below a minimum size, it is considered
;; worthless and discarded. This can happen to either new codes or
;; being changed. Handles certainty.
(define (set-symbol-codes imap symbol codes)
  (let* ((smap (if (yes? imap) (second imap) #f))
         (nmap (if (yes? imap) (first imap) #f)))
    (list nmap (set-smap-codes smap symbol codes))))

(define (find-smap-codes smap symbol)
  (let* ((elt (list-search-positive smap (lambda (x) (eq? (first x) symbol)))))
    (if (yes? elt) (second elt) #f)))

(define (find-smap-uncertains smap symbol)
  (let* ((elt (list-search-positive smap (lambda (x) (eq? (first x) symbol)))))
    (if (yes? elt) (third elt) #f)))

(define (find-smap-certainty smap symbol)
  (let* ((elt (list-search-positive smap (lambda (x) (eq? (first x) symbol)))))
    (if (yes? elt) (fourth elt) #f)))

(define (set-smap-codes smap symbol codes)
  (let* ((result (split smap
                        (lambda (x) (eq? (first x) symbol))))
         (pos (first result)) ;; should have 0 or 1 entries
         (neg (second result)))
    (if (< (+ (length (first codes)) (length (second codes)))
           min-wires-per-symbol)
        neg
        (if (yes? pos)
            (consp (consp symbol codes) neg)
            (consp (consp symbol codes) smap)))))

;; sets the code for an inflection
(define (set-inflection-code imap inflection ucode)
  (let* ((smap (if (yes? imap) (second imap) #f))
         (nmap (if (yes? imap) (first imap) #f))
         (result (split nmap
                        (lambda (x) (eq? (first x) inflection))))
         (pos (first result)) ;; should have 0 or 1 entries
         (neg (second result)))
    (list (consp (list inflection ucode) neg) smap)))

;; The purpose of this function is to shrink the symbols in the
;; association DB in order to have more precise interptations
;; of which lines represent a given symbol.
;; Certain lines are not filtered until the symbol has become certain
(define (listen-resolve-conflicts spokefirst comm-lines mappings)
  (let loop ((map-list mappings) (new-imap mappings))
    (if (yes? map-list)
        (let* ((elt (first map-list))
               (ccodes (find-smap-codes new-imap (first elt)))
               (cnewcodes (list-transform-negative ccodes
                            (lambda (x)
                              (eq? 0 (test-line comm-lines x)))))
               (ucodes (find-smap-uncertains new-imap (first elt)))
               (unewcodes (list-transform-negative ucodes
                            (lambda (x)
                              (eq? 0 (test-line comm-lines x)))))
               (newcert (+ (if (yes? spokefirst) 0 1)
                           (find-smap-certainty new-imap (first elt))))
               (newcodes
                (if (>= newcert certainty-threshold)
                    (if (>= newcert certain-pruning-threshold)
                        (list (->uniset (appendp cnewcodes unewcodes))
                              '() newcert)
                        (list (->uniset (appendp ccodes unewcodes))
                              '() newcert))
                    (list ccodes unewcodes newcert))))
          (loop (cdr map-list)
                (set-smap-codes new-imap (first elt) newcodes)))
        new-imap)))

;; This function asserts a feature line if enough of its associated
;; comm-lines are lit up.
;; Only certain com lines are considered.
(define (listen-assert-features comm-lines feature-lines nmap mappings)
  (fold-left (lambda (features imap-elt)
               (let* ((ll (second imap-elt)) ;; line-list
                      (fll (list-transform-negative ll
                             (lambda (x) (eq? 0 (test-line comm-lines x)))))
                      (num-1s (length (list-transform-positive ll
                                        (lambda (x)
                                          (eq? -1 (test-line comm-lines x))))))
                      (num1s (length (list-transform-positive ll
                                       (lambda (x)
                                         (eq? 1 (test-line comm-lines x)))))))
                 (if (and (yes? ll)
                          (>= (/ (length fll) (length ll)) percent-match)
                          (< 0 (+ num1s num-1s)))
                     (let loop ((inflections
                                 (unary-match (/ num1s (+ num1s num-1s))
                                              nmap))
                                (f features))
                       (if (yes? inflections)
                           (loop (cdr inflections)
                                 (assert-feature-line f (first imap-elt)
                                                      (car inflections) 1))
                           (assert-feature-line f (first imap-elt) #f 1)))
                     features)))
             (proper-null feature-lines)
             (proper-null mappings)))

(define (unary-match ucode mappings)
  (map first (proper-null
              (list-transform-positive mappings
                (lambda (x) (< (abs (- (second x) ucode))
                          unary-percent-match))))))

;; given the nmap, adjust the interpretations
;; for features in the nmap
;;   if inflection on featurelines
;;     get uvalue of comm-lines
;;       if more than .%xu-%match away, jump to value
;;     else
;;       test if simulated by any comm-lines
;;         if so, randomly reselect value
(define (update-nmap nmap comm-lines feature-lines stimfeat)
  (let loop ((tnmap nmap))
    (if
     (no? tnmap)
     #f
     (let* ((infl (car tnmap))
            (res (map (lambda (x)
                        (let* ((lines (map (lambda (y)
                                             (test-line comm-lines y))
                                           (second x)))
                               (numbits (length lines))
                               (num1s (- numbits (length (delq 1 lines))))
                               (num-1s (- numbits (length (delq -1 lines))))
                               (numxs (- numbits (length (delq 'x lines)))))
                          (if (= numxs 0) ;; for now, only on clean ones
                              (if (>= (/ (+ num1s num-1s) numbits)
                                      percent-match)
                                  (list (first x) (/ num1s (+ num1s num-1s)))
                                  #f)
                              'invalid)))
                      (proper-null stimfeat)))
            (tmap (delq #f res))
            (ifeat (list-search-positive feature-lines
                     (lambda (x) (eq? (first infl) (second x))))))
       (if (member 'invalid (proper-null tmap))
           (consp infl (loop (cdr tnmap)))
       (if (yes? ifeat)
           (let* ((res (list-search-positive tmap
                         (lambda (x) (eq? (first ifeat) (first x)))))
                  ;; if no value in tmap, don't change the inflection value
                  (uvalue (if res (second res) (second infl)))
                  (dif (- uvalue (second infl))))
             (if (> dif (/ unary-percent-match 2))
                 (consp (list (first infl) uvalue) (loop (cdr tnmap)))
                 (consp infl (loop (cdr tnmap)))))
           (let ((res (list-search-positive tmap
                        (lambda (x) (< (abs (- (second x) (second infl)))
                                  unary-percent-match)))))
             (if (yes? res)
                 (consp (list (first infl) (random 1.0))
                       (loop (cdr tnmap)))
                 (consp infl (loop (cdr tnmap)))))))))))

;; nmap = (list (list infl val))
;; nsquelch kills mappings which are too close together
(define (nsquelch nmap)
  (let loop ((m (random-permutation (proper-null nmap))))
    (if (yes? m)
        (let ((res (list-search-positive (cdr m)
                     (lambda (x) (< (abs (- (second x) (second (first m))))
                               (* 2 unary-percent-match))))))
          (if res
              (loop (cdr m))
              (consp (car m) (loop (cdr m)))))
        #f)))

;; returns the comm-codes of a symbol in an internal representation
;; only return the *certain* codes
(define (find-symbol-codes imap symbol)
  (let* ((smap (if imap (second imap) #f))
         (elt (list-search-positive smap (lambda (x) (eq? (first x) symbol)))))
    (if elt (second elt) #f)))

;; returns list of *uncertain* codes
(define (find-symbol-uncertains imap symbol)
  (let* ((smap (if imap (second imap) #f))
         (elt (list-search-positive smap (lambda (x) (eq? (first x) symbol)))))
    (if elt (third elt) #f)))

;; returns the certainty value
(define (find-symbol-certainty imap symbol)
  (let* ((smap (if imap (second imap) #f))
         (elt (list-search-positive smap (lambda (x) (eq? (first x) symbol)))))
    (if elt (fourth elt) #f)))

;; returns the unary code for a given inflection
(define (find-inflection-code imap inflection)
  (let* ((nmap (if (yes? imap) (first imap) #f))
         (elt (list-search-positive nmap (lambda (x) (eq? (first x) inflection)))))
    (if (yes? elt) (second elt) #f)))

;; Assert-line on many different lines ((line value) (line value) ...)
(define (assert-line-multiple line-list linebundle)
  (if (no? line-list)
      linebundle
      (assert-line-multiple
       (cdr line-list)
       (assert-line linebundle (caar line-list) (cadar line-list)))))

;; Generate Random Comm Codes
(define (random-comm-codes wps wires)
  (if (> wps 0)
      (consp (+ (random wires) 1) (random-comm-codes (- wps 1) wires))
      #f))

;; given a set of wires and a unary code, returns a list
;; of wires with appropriate bits on them. Signal is the %age of 1s.
(define (unary-signal wires ucode)
  (let* ((num1s (round->exact (* ucode (length wires))))
         (num-1s (- (length wires) num1s))
         (bits (random-permutation (appendp (make-list num1s 1)
                                            (make-list num-1s -1)))))
    (map list (proper-null wires) (proper-null bits))))

;; Main Talk Procedure
;; Only output certain symbols; all newly generate randoms are certain
;; Internal-Map is a list of two elements. The first is inflection maps,
;; the second is symbol maps
(define (talk comm-lines feature-lines internal-maps)
  (define (loop feature-lines)
    (if (no? feature-lines)
        #f
        (let* ((cur-elmt (car feature-lines))
               (symbol (first cur-elmt))
               (inflection (second cur-elmt))
               (comm-codes (find-symbol-codes internal-maps symbol))
               (infl-code (find-inflection-code internal-maps inflection)))
          (cond ((no? comm-codes)
                 (set! internal-maps
                       (set-symbol-codes
                        internal-maps symbol
                        (list (random-comm-codes num-wires-per-symbol
                                                 num-wires) '() 0)))
                 (loop feature-lines))
                ((no? infl-code)
                 (set! internal-maps
                       (set-inflection-code internal-maps inflection
                                            (random 1.0)))
                 (loop feature-lines))
                (else
                 (appendp (unary-signal comm-codes infl-code)
                          (loop (cdr feature-lines))))))))
  (if (no? feature-lines)
      (list internal-maps comm-lines)
      (let* ((newlines (assert-line-multiple (loop feature-lines) comm-lines))
             (newmaps internal-maps))
        (list newmaps newlines))))

