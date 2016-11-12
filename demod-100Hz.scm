;;;
;;;  Demodurate WWV's 100Hz sub-carrier
;;;
(use math.const)
(use gauche.uvector)

;;
;; 8ksps
;;
(define-constant T100Hz 80)
(define-constant T0.1s 800)

;;
;; 44.1ksps
;;
;(define-constant T100Hz 441)
;(define-constant T0.1s 4410)

;;
;; 48ksps
;;
;(define-constant T100Hz 480)
;(define-constant T0.1s 4800)

(define-constant BUFSIZ 20)

;; Time index
(define T  0)

;; Accumulators
(define aI 0) 
(define aQ 0)

;; Output
(define I  0)
(define Q  0)
(define A  0)  ; Analog out = sqrt(I^2 + Q^2)
(define Av 0)  ; Average Analog out
(define D  0)  ; Digital data

;;;
;;;
;;;
(define (Ibase T)  (cos (/ (* 2 pi T) T100Hz)))
(define (Qbase T)  (sin (/ (* 2 pi T) T100Hz)))

(define (IbaseP T)
  (if (< T (* T100Hz 2/4)) +1 -1))

(define (QbaseP T)
  (cond ((< T (* T100Hz 1/4)) -1)
        ((< T (* T100Hz 3/4)) +1)
        (else     -1)))

(define (process-buf inbuf n)
  (let lp ((i 0))
    (cond ((= i n) #t)
          ((< T T0.1s)
           (let ((d (s16vector-ref inbuf i)))
             (set! aI (+ aI (* (Ibase T) d)))
             (set! aQ (+ aQ (* (Qbase T) d))))
           (inc! T)
           (lp (+ i 1)))
          (else
           (set! T  0)
           (set! I  aI)
           (set! Q  aQ)
           (set! A  (sqrt (+ (* I I) (* Q Q))))
           (let ((a 0.998))
             (set! Av (+ (* a Av) (* (- 1.0 a) A))))
           (set! D (if (> A Av) 1 0))
           (set! aI 0)
           (set! aQ 0)
           (print #`",|I| ,|Q| ,|A| ,|Av| ,|D|")
           (lp i)))))

;;
;;
(define (read-wav-data inbuf)
  (let ((m (read-uvector! inbuf)))
    (if (eof-object? m)  0 m)))

(define-constant hsize 44)
(define (read-wav-header)
  (let* ((h (make-u8vector hsize 0))
         (n (read-uvector! h)))
    (if (not (= n hsize))
      (error "Something went wrong"))))

;;
;;
(define (run)
  (let ((inbuf (make-s16vector BUFSIZ 0)))
    (read-wav-header)
    (print "# |I| |Q| |A| |Av| |D|")
    (let lp ((n (read-wav-data inbuf)))
      (process-buf inbuf n)
      (if (= n BUFSIZ)
        (lp (read-wav-data inbuf))
        0))))

(define (main args)  (run))
;;; EOF
