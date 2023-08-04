; #lang racket
; (require racket/mpair
;          compatibility/defmacro
;          "queue.rkt")  ;; dispatch based queue 

; (provide add-action!
;          after-delay
;          and-gate
;          and-gate-delay
;          get-signal
;          inverter
;          inverter-delay
;          logical-not
;          logical-and
;          logical-or
;          make-wire
;          or-gate-delay
;          set-signal!
;          probe
;          propagate
;          with-wires)

; (define (car x)
;   (cond ((pair? x) (car x))
;         ((mpair? x) (mcar x))))
; (define (cdr x) 
;   (cond ((pair? x) (cdr x)) 
;         ((mpair? x) (mcdr x))))
; (define (set-car! x v) 
;   (cond ((pair? x) (set-car! x v))
;         ((mpair? x) (set-mcar! x v))))
; (define (set-cdr! x v) 
;   (cond ((pair? x) (set-cdr! x v)) 
;         ((mpair? x) (set-mcdr! x v))))

; (define (make-wire)
;   (let ((signal-value 0) (action-procedures '()))
;       (define (set-my-signal! new-value)
;         (if (not (= signal-value new-value))
;             (begin (set! signal-value new-value)
;                    (call-each action-procedures))
;             'done))
;       (define (accept-action-procedure! proc)
;         (set! action-procedures (cons proc action-procedures))
;         (proc))
;       (define (dispatch m)
;         (cond [(eq? m 'get-signal) signal-value]
;               [(eq? m 'set-signal!) set-my-signal!]
;               [(eq? m 'add-action!) accept-action-procedure!]
;               [else (error "Unknown operation -- WIRE" m)]))
;       dispatch))

; (define (call-each procedures)
;   (cond [(null? procedures) 'done]
;         [else ((car procedures)) 
;               (call-each (cdr procedures))]))

; (define (get-signal wire) (wire 'get-signal))
; (define (set-signal! wire new-value) ((wire 'set-signal!) new-value))
; (define (add-action! wire action-procedure) ((wire 'add-action!) action-procedure))

; (define-macro with-wires
;   (lambda (wires . body)
;     `(let ,(map (lambda (wire-sym) `(,wire-sym (make-wire))) 
;                 wires)
;        ,@body)))

; (define (logical-not s)
;   (cond ((= s 0) 1)
;         ((= s 1) 0)
;         (else (error "Invalid signal" s))))
; (define (logical-and x y)
;   (cond ((and (= x 1) (= x 1)) 1)
;         (else 0)))
; (define (logical-or x y)
;   (cond ((and (= x 0) (= x 0))  0)
;         (else 1)))

; (define (inverter input output)
;   (define (invert-input)
;     (let ((new-value (logical-not (get-signal input))))
;       (after-delay inverter-delay 
;                   (lambda ()
;                           (set-signal! output new-value)))))
;   (add-action! input invert-input)
;   'ok)

; (define (and-gate a1 a2 output)
;   (define (and-action-procedure)
;     (let ((new-value
;            (logical-and (get-signal a1) (get-signal a2))))
;       (after-delay and-gate-delay
;                    (lambda ()
;                      (set-signal! output new-value)))))
;   (add-action! a1 and-action-procedure)
;   (add-action! a2 and-action-procedure)
;   'ok)

; (define (after-delay delay action)
;   (add-to-agenda! (+ delay (current-time the-agenda))
;                   action
;                   the-agenda))

; (define (propagate)
;   (if (empty-agenda? the-agenda)
;       'done
;       (let ((first-item (first-agenda-item the-agenda)))
;         (first-item)
;         (remove-first-agenda-item! the-agenda)
;         (propagate))))

; (define (probe name wire)
;   (add-action! wire
;                (lambda ()        
;                  (newline)
;                  (display name)
;                  (display " ")
;                  (display (current-time the-agenda))
;                  (display "  New-value = ")
;                  (display (get-signal wire)))))

; (define (make-time-segment time queue)
;   (mcons time queue))
; (define (segment-time s) (mcar s))
; (define (segment-queue s) (mcdr s))

; (define (make-agenda) (list 0))
; (define (current-time agenda) (car agenda))
; (define (set-current-time! agenda time) 
;   (set-mcar! agenda time))
; (define (segments agenda) (mcdr agenda))
; (define (set-segments! agenda segments)
;   (set-mcdr! agenda segments))
; (define (first-segment agenda) (mcar (segments agenda)))
; (define (rest-segments agenda) (mcdr (segments agenda)))

; (define (empty-agenda? agenda)
;   (null? (segments agenda)))

; (define (add-to-agenda! time action agenda)
;   (define (belongs-before? segments)
;     (or (null? segments)
;         (< time (segment-time (car segments)))))

;   (define (make-new-time-segment time action)
;     (let ((q (make-queue)))
;       ((q 'insert!) action)
;       (make-time-segment time q)))
;   (define (add-to-segments! segments)
;     (if (= (segment-time (car segments)) time)
;         (((segment-queue (car segments)) 'insert!) action)
;         (let ((rest (cdr segments)))
;           (if (belongs-before? rest)
;               (set-mcdr! 
;               segments
;               (cons (make-new-time-segment time action)
;                     (cdr segments)))
;               (add-to-segments! rest)))))

;     (let ((segments (segments agenda)))
;       (if (belongs-before? segments)
;           (set-segments! 
;             agenda
;             (cons (make-new-time-segment time action)
;                   segments))
;             (add-to-segments! segments))))

; (define (remove-first-agenda-item! agenda)
;   (let ((q (segment-queue (first-segment agenda))))
;     (q 'delete-queue!)
;     (when (q 'empty-queue?)
;           (set-segments! agenda (rest-segments agenda)))))

; (define (first-agenda-item agenda)
;   (if (empty-agenda? agenda)
;       (error "Agenda is empty -- FIRST-AGENDA-ITEM")
;       (let ((first-seg (first-segment agenda)))
;         (set-current-time! agenda (segment-time first-seg))
;         ((segment-queue first-seg) 'front-queue))))

; (define the-agenda (make-agenda))
; (define inverter-delay 2)
; (define and-gate-delay 3)
; (define or-gate-delay 5)





#lang racket
(require racket/mpair
         compatibility/defmacro
         "queue.rkt")  ;; dispatch based queue 

(provide add-action!
         after-delay
         and-gate
         and-gate-delay
         get-signal
         inverter
         inverter-delay
         logical-not
         logical-and
         logical-or
         make-wire
         or-gate-delay
         set-signal!
         probe
         propagate
         with-wires)

(define (car x) 
  (cond ((pair? x) (car x))
        ((mpair? x) (mcar x))))
(define (cdr x) 
  (cond ((pair? x) (cdr x)) 
        ((mpair? x) (mcdr x))))
(define (set-car! x v) 
  (cond ((pair? x) (set-car! x v))
        ((mpair? x) (set-mcar! x v))))
(define (set-cdr! x v) 
  (cond ((pair? x) (set-cdr! x v)) 
        ((mpair? x) (set-mcdr! x v))))

(define (make-wire)
  (let ((signal-value 0) 
        (action-procedures null))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation -- WIRE" m))))
    dispatch))

(define (call-each procedures)
  (cond ((null? procedures) 'done)
        (else ((car procedures))
              (call-each (cdr procedures)))))

(define (get-signal wire) (wire 'get-signal))
(define (set-signal! wire new-value) ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure) ((wire 'add-action!) action-procedure))

(define-macro with-wires
  (lambda (wires . body)
    `(let ,(map (lambda (wire-sym) `(,wire-sym (make-wire))) 
                wires)
       ,@body)))


(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))
(define (logical-and x y)
  (cond ((and (= x 1) (= x 1)) 1)
        (else 0)))
(define (logical-or x y)
  (cond ((and (= x 0) (= x 0))  0)
        (else 1)))

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

(define (probe name wire)
  (add-action! wire
               (lambda ()        
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display "  New-value = ")
                 (display (get-signal wire)))))

(define (make-time-segment time queue)
  (mcons time queue))
(define (segment-time s) (mcar s))
(define (segment-queue s) (mcdr s))

(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-mcar! agenda time))
(define (segments agenda) (mcdr agenda))
(define (set-segments! agenda segments)
  (set-mcdr! agenda segments))
(define (first-segment agenda) (mcar (segments agenda)))
(define (rest-segments agenda) (mcdr (segments agenda)))

(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      ((q 'insert!) action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (((segment-queue (car segments)) 'insert!) 
         action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-mcdr!
               segments
               (cons (make-new-time-segment time action)
                     (cdr segments)))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
         agenda
         (cons (make-new-time-segment time action)
               segments))
        (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (q 'delete-queue!)
    (when (q 'empty-queue?)
      (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty -- FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        ((segment-queue first-seg) 'front-queue))))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

