(define-module (gutman)
  :use-module (tuile pr)
  :use-module (tuile utils)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-9)
  :use-module (srfi srfi-11)
  :use-module (srfi srfi-43)
  :use-module (ice-9 string-fun)

  #:export (
            gutman-read
            gutman-edit
            gutman-catch
            gutman-raise
            gutman-use

            read-file
            read-file-content
            write-file
            copy
            line
            step
            firstline
            lastline
            blockline
            lines
            get
            ref
            set
            has
            sub
            update
            insert
            insert-step
            remove
            insertfile
            insertfile-step
            clear
            find
            search
            linecount
            filename
            edit
            edited?
            within?
            excursion
            mark
            unmark
            do-all
            do-range
            do-for
            get-range
            get-for
            peek
            peek-ln
            view
            view-ln
            ))


;; ------------------------------------------------------------
;; Gutman State.

;; Internal Gutman State per edited/accessed file.
(define-mu-record state
  filename                              ; Gutman file name.
  lines                                 ; File content as vector of lines.
  line                                  ; Current line number.
  mark                                  ; Default mark.
  marks                                 ; Named marks (hash).
  blockline                             ; Current block end-line.
  edited                                ; Edited (dirty) flag.
  )

;; Current Gutman State.
(define guts (make-parameter #f))

;; Access routines to current Gutman State.
(define (gs-set-filename!  val) (guts (let ((st (guts))) (set-state-filename!  st val) st)))
(define (gs-set-lines!     val) (guts (let ((st (guts))) (set-state-lines!     st val) st)))
(define (gs-set-line!      val) (guts (let ((st (guts))) (set-state-line!      st val) st)))
(define (gs-set-mark!      val) (guts (let ((st (guts))) (set-state-mark!      st val) st)))
(define (gs-set-marks!     val) (guts (let ((st (guts))) (set-state-marks!     st val) st)))
(define (gs-set-blockline! val) (guts (let ((st (guts))) (set-state-blockline! st val) st)))
(define (gs-set-edited!    val) (guts (let ((st (guts))) (set-state-edited!    st val) st)))



;; ------------------------------------------------------------
;; User entry functions:


;; Catch/handle Gutman Exception.
;;
;; By default catch all exception, but if type or types are given,
;; then the listed exceptions are only handled.
;;
;; Example:
;;
;;      (gutman-catch #f
;;                    (search "diiduu"))
;;
(define-syntax gutman-catch
  (lambda (x)
    (syntax-case x ()
      ((_ exn body ...)
       #'(begin
           (gutman-handle-exception exn
                                    (lambda ()
                                      body ...)))))))


;; Raise Gutman Exception.
;;
;; Exceptions:
;;     gutman-file-error     - file can't be opened
;;     gutman-search-error   - search failed
(define (gutman-raise exn msg)
  (with-output-to-port (current-error-port)
    (lambda ()
      (display "gutman: ")
      (display msg)
      (newline)))
  (raise-exception exn))


;; Open existing file for viewing or editing.
(define (gutman-read filename)
  (gutman-handle-exception
   'gutman-fatal
   (lambda ()
     (gutman-create-state filename)
     (read-file)
     (guts))))


;; Edit file and also create it if it does not exist. Editing will be
;; performed in dedicated Gutman State.
;;
;; Example:
;;
;;     (gutman-edit "my-file.txt"
;;                  (set "Line1"))
;;
(define-syntax gutman-edit
  (lambda (x)
    (syntax-case x ()
      ((_ filename body ...)
       #'(begin
           (parameterize ((guts (gutman-create-state filename)))
             (gutman-catch #f
                           (when (file-exists? filename)
                             (read-file))
                           body ...
                           (write-file))))))))


;; Create Gutman State for filename.
(define (gutman-create-state filename)
  (guts (make-state filename
                  (vector)
                  0
                  #f
                  (make-hash-table)
                  #f
                  #f))
  (guts))


;; Use the given Gutman State.
;;
;; Example:
;;
;;     (gutman-use gutman-state
;;                 (set "Line1"))
;;
(define-syntax gutman-use
  (lambda (x)
    (syntax-case x ()
      ((_ use-guts body ...)
       #'(parameterize ((guts use-guts))
           body ...)))))



;; ------------------------------------------------------------
;; Access routines for user.

;; Read file in.
(define (read-file . args)
  (let ((filename (apply-arg-or-default args
                                        (filename))))
    (if (file-exists? filename)
        (gs-set-lines! (read-file-content filename))
        (gutman-raise 'gutman-file-error
                      (ss "File not found: " filename)))))


;; Write Gutman content to disk.
(define (write-file . args)
  (let ((filename (apply-arg-or-default args
                                        (filename))))
    (when (edited?)
      (let ((file-dir (dirname filename)))
        (when (and file-dir
                   (not (file-exists? file-dir)))
          (mkdir file-dir)))
      (call-with-output-file filename
        (lambda (port)
          (for ((line (vector->list (->lines))))
            (if line
                (display (ss line "\n") port)
                (display "\n" port)))))
      (gs-set-edited! #f))))


;; Copy Gutman content to file.
(define (copy filename)
  (write-file filename))


;; Return or set line.
(define (line . args)
  (if (pair? args)
      (gs-set-line! (abs-index (car args)))
      (+ (->line) 1)))


;; Step forward or backward current position.
(define (step . args)
  (let ((dir (if (pair? args) (car args) 1)))
    (gs-set-line! (abs-index (+ (+ (->line) 1) dir)))))


;; Jump to first line.
(define (firstline)
  (gs-set-line! 0))


;; Jump to last line.
(define (lastline)
  (gs-set-line! (- (linecount) 1)))


;; Jump to line after block.
(define (blockline)
  (when (state-blockline (guts))
    (gs-set-line! (state-blockline (guts)))))


;; Get or set all Gutman content.
(define (lines . args)
  (if (pair? args)
      (begin
        (gs-set-edited! #t)
        (gs-set-lines! (args-to-vector (car args))))
      (->lines)))

;; Get current line or lines by count.
(define (get . args)
  (let ((count (default-unsigned-count args 1)))
    (if (= count 1)
        (vector-ref (->lines) (->line))
        (take-range (->lines) (->line) (+ (->line) (- count 1))))))


;; Get current line or any line.
(define (ref . args)
  (if (pair? args)
      (let ((idx (raw-abs-index (car args))))
        (cond
         ((< idx 0) #f)
         ((> idx (- (linecount) 1)) #f)
         (else (vector-ref (->lines) idx))))
      (vector-ref (->lines) (->line))))


;; Set current line.
(define (set text)
  (gs-set-edited! #t)
  (vector-set! (->lines) (->line) text))


;; Return non-false if line has the string or regexp.
;;
;; args:
;;   from     String or regexp.
;;   to       Target string.
;;   [regexp] From is regexp if #t.
;;
(define* (has str-or-re #:key (regexp #f))
  (cond
   (regexp
    (re-match str-or-re (vector-ref (->lines) (->line))))
   (else
    (string-contains (vector-ref (->lines) (->line))
                     str-or-re))))


;; Substitute part of current line content.
;;
;; args:
;;   from     String or regexp.
;;   to       Target string.
;;   [regexp] From is regexp if #t.
;;
(define* (sub from to #:key (regexp #f))
  (gs-set-edited! #t)
  (vector-set! (->lines)
               (->line)
               (cond
                (regexp
                 (re-sub from
                         (vector-ref (->lines) (->line))
                         to))
                (else
                 (string-replace-substring (vector-ref (->lines) (->line))
                                           from
                                           to)))))


;; Update current line content (i.e. get&set) with the return value
;; of the given block. Hence last stmt should include the new line
;; content.
;;
;; Example:
;;
;;     (update (lambda (c)
;;               (regexp-replace "foo" c "bar")))
(define (update fn)
  (gs-set-edited! #t)
  (vector-set! (->lines)
               (->line)
               (apply fn (list (vector-ref (->lines) (->line))))))


;; Insert lines and move to insertion position.
;;
;; Position: <num>, 'first, 'after, 'last, 'end.
;;
;; args:
;;   <none>     Insert empty line at current position.
;;   text       Insert text at current position.
;;   text pos   Insert text at given position and move to it.
(define (insert . args)
  (gs-set-edited! #t)
  (let-values (((index count) (insert-lines args)))
    (gs-set-line! index)))


;; Insert lines and move to last inserted line.
;;
;; args:
;;   <none>     Insert empty line at current position.
;;   text       Insert text at current position.
;;   text pos   Insert text at given position (position: <num>, 'first, 'end).
(define (insert-step . args)
  (gs-set-edited! #t)
  (let-values (((index count) (insert-lines args)))
    (line (+ index count))))


;; Remove current line or number of lines.
(define (remove . args)
  (gs-set-edited! #t)
  (let ((count (default-unsigned-count args 1)))
    (gs-set-lines! (vector-delete (->lines) (->line) count))))


;; Insert file to current position.
(define (insertfile filename . pos)
  (insert (read-file-content filename)
          (if (pair? pos) (car pos) (->line))))


;; Insert file to current position and step.
(define (insertfile-step filename . pos)
  (insert-step (read-file-content filename)
               (if (pair? pos) (car pos) (->line))))


;; Clear Gutman content and reset current line.
(define (clear)
  (gs-set-edited! #t)
  (gs-set-lines! #())
  (gs-set-line! 0))


;; Find Regexp or literal string forwards or backwards. Return true
;; on success.
(define (find re-or-str . forward)
  (let ((fwd (if (pair? forward) (car forward) #t)))
    (let ((res (find-or-fail re-or-str fwd)))
      (if res
          (begin
            (gs-set-line! res)
            #t)
          #f))))


;; Search Regexp or literal string forwards or backwards. Fail with
;; expection (gutman-search-error) if not found.
(define (search re-or-str . forward)
  (let ((fwd (if (pair? forward) (car forward) #t)))
    (let ((res (find-or-fail re-or-str fwd)))
      (if res
          (gs-set-line! res)
          (gutman-raise 'gutman-search-error
                        (ss "Pattern not found: " re-or-str))))))


;; Return line count of Gutman content.
(define (linecount)
  (vector-length (->lines)))


;; Return Gutman file name.
(define (filename)
  (state-filename (guts)))


;; Mark content modified (explicit).
(define (edit)
  (gs-set-edited! #t))


;; Return true if content is modified.
(define (edited?)
  (state-edited (guts)))


;; Return true if within the lines region.
(define (within?)
  (<= (line) (linecount)))


;; Execute block, retain current position, and return block value.
(define (excursion fn)
  (let* ((orgline (->line))
         (ret (fn)))
    (gs-set-line! orgline)
    ret))


;; Mark (store) current position to default or to named mark.
(define (mark . args)
  (if (pair? args)
      (hash-set! (state-marks (guts))
                 (car args)
                 (+ (->line) 1))
      (gs-set-mark! (+ (->line) 1))))


;; Unmark (restore) current position from default or from named
;; mark.
(define (unmark . args)
  (if (and (pair? args)
           (hash-ref (state-marks (guts)) (car args)))
      (gs-set-line! (hash-ref (state-marks (guts)) (car args)))
      (when (state-mark (guts))
        (gs-set-line! (- (state-mark (guts)) 1))
        (gs-set-mark! #f))))


;; Execute given block for all lines, i.e. all positions. Block
;; parameter is Gutman.
(define (do-all fn)
  (do-range-safe 0 (- (linecount) 1) fn))


;; Execute given block between start and stop positions, and update
;; position.
(define (do-range start stop fn)
  (let-values (((a b) (normalize-user-indeces start stop)))
    (do-range-safe a b fn)))


;; Execute given block starting from start by count, and update
;; position.
(define (do-for start count fn)
  (let-values (((a b) (normalize-user-indeces start (+ start (- count 1)))))
    (do-range-safe a b fn)))


;; Get lines between start and stop positions inclusive.
(define (get-range au bu)
  (let-values (((a b) (normalize-user-indeces au bu)))
    (take-range (->lines) a b)))


;; Get lines starting from start by count.
(define (get-for au bu)
  (let ((atmp (abs-index au)))
    (let-values (((a b) (normalize-user-indeces atmp (+ atmp bu))))
      (take-range (->lines) a b))))


;; View line content around current position (by count).
(define (peek . args)
  (let ((count (default-unsigned-count args 0))
        (line (+ (->line) 1)))
    (view-range (- line count) (+ line count) #f)))


;; View line content with line numbers around current position (by
;; count).
(define (peek-ln . args)
  (let ((count (default-unsigned-count args 0))
        (line (+ (->line) 1)))
    (view-range (- line count) (+ line count) #t)))


;; View line content.
;;
;; * no args:  view all
;; * one arg:  view from current onwards by count
;; * two args: view given range
(define (view . args)
  (let-values (((a b) (view-args-to-range args)))
    (view-range a b #f)))


;; View line content with line numbers.
;;
;; * no args:  view all
;; * one arg:  view from current onwards by count
;; * two args: view given range
(define (view-ln . args)
  (let-values (((a b) (view-args-to-range args)))
    (view-range a b #t)))




;; ------------------------------------------------------------
;; Private:

;; Handle given exception.
;;
;; If exception is #f, the exception will be handled withtout exiting
;; the program.
(define (gutman-handle-exception exn thunk)
  (if exn
      ;; Exit with exception.
      (with-exception-handler
          (lambda (exn)
            (exit 1))
        thunk
        #:unwind? #t)
      ;; Continue with exception.
      (with-exception-handler
          (lambda (exn)
            #t)
        thunk
        #:unwind? #t)))


;; Return current line number.
(define (->line)
  (state-line (guts)))

;; Return all lines.
(define (->lines)
  (state-lines (guts)))


;; Read file content.
;;
;; Empty lines are mapped to false.
;;
(define (read-file-content filename)
  (list->vector
   (map (lambda (line)
          (if (string-null? line)
              #f
              line))
        (vector->list (file->lines filename)))))


;; Take a range of lines.
(define (take-range lines a b)
  (vector-range lines a (1+ b)))


;; Convert arguments to vector.
;;
;; Do nothing if already a vector.
(define (args-to-vector args)
  (cond
   ((vector? args) args)
   ((list? args) (list->vector args))
   (else (vector args))))


;; Find re-or-str (or fail) to given direction.
(define (find-or-fail re-or-str forward)
  (let ((line (->line))
        (len (linecount)))
    (let-values (((off limcmp lim) (if forward
                                       (values + < len)
                                       (values - >= 0))))
      (let ((patcmp (if (string? re-or-str)
                        (lambda (line pat) (string-contains line pat))
                        (lambda (line pat) (re-match pat line)))))

        (call/cc
         (lambda (cc)
           (let loop ((line line))
             (when (limcmp line lim)
               (when (and (vector-ref (->lines) line)
                          (patcmp (vector-ref (->lines) line) re-or-str))
                 (cc line))
               (loop (off line 1))))
           #f))))))


;; Non-normalized, but absolute, index.
(define (raw-abs-index a)
  (if (< a 0)
      (+ (linecount) a)
      (- a 1)))


;; Normalize index.
(define (normalize-abs-index a)
  (cond
   ((< a 0) 0)
   ((> a (linecount)) (- (linecount) 1))
   (else a)))


;; Return absolute index.
(define (abs-index a)
  (normalize-abs-index (raw-abs-index a)))


;; Return multiple-values of 2.
(define (normalize-user-indeces au bu)
  (let ((a (abs-index au))
        (b (abs-index bu)))
    (if (> a b)
        (values b a)
        (values a b))))


;; Safe execution of range.
(define (do-range-safe a b fn)
  (let ((orgline (->line)))
    (gs-set-line! a)
    (let loop ((i a))
      (fn)
      (when (< i b)
        (gs-set-line! (+ (->line) 1))
        (loop (+ i 1))))
    (gs-set-blockline! (->line))
    (gs-set-line! orgline)))


;; View range of lines.
(define (view-range first last show-lines)
  (let-values (((a b) (normalize-user-indeces first last)))
    (view-range-safe a b show-lines)))


;; View range of lines safely.
(define (view-range-safe a b show-lines)
  (let loop ((line (take-range (->lines) a b))
             (lineno (+ a 1)))
    (if show-lines
        (begin
          (pr (:rj 3 lineno) ": " line)
          (loop (cdr line) (1+ lineno)))
        (pr line))))


;; Apply first from args or default.
(define (apply-arg-or-default args default)
  (if (pair? args)
      (car args)
      default))


;; Return given count of default.
(define (default-unsigned-count args default)
  (if (pair? args)
      (if (> (car args) default)
          (car args)
          default)
      default))


;; View line content.
;;
;; * no args:  view all
;; * one arg:  view from current onwards by count
;; * two args: view given range
(define (view-args-to-range args)
  (cond
   ((>= (length args) 2)
    (normalize-user-indeces (first args)
                            (second args)))
   ((= (length args) 1)
    (normalize-user-indeces (+ (->line) 1)
                            (+ (->line) (first args))))
   (else (values 1 (linecount)))))


;; Convert position to index.
(define (pos-to-index pos)
  (cond
   ((number? pos)     (abs-index pos))
   ((eq? pos 'first)  0)
   ((eq? pos 'after)  (+ (->line) 1))
   ((eq? pos 'last)   (- (linecount) 1))
   ((eq? pos 'end)    (linecount))))


;; Insert lines.
(define (insert-lines args)
  (let-values (((text index)
                (cond
                 ((>= (length args) 2)
                  (values (args-to-vector (first args))
                          (pos-to-index (second args))))
                 ((>= (length args) 1)
                  (values (args-to-vector (first args)) (->line)))
                 (else
                  (values (args-to-vector #f)
                          (->line))))))
    (if (= index (linecount))
        (gs-set-lines! (vector-append (->lines) text))
        (gs-set-lines! (vector-insert (->lines) index text)))
    (values index (vector-length text))))
