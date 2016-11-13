; -*- mode: Scheme; compile-command: "sh ./run.sh"; -*-
;;;
;;;  Demodurate WWV's 100Hz sub-carrier
;;;
(use math.const)
(use gauche.uvector)

(define-constant BUFSIZ 20)

;; Sample period
(define T100Hz #f)
(define T100ms #f)

(define (set-T SampleRate)
  (case SampleRate
    ((8000)
     (set! T100Hz   80)
     (set! T100ms  800))
    ((44100)
     (set! T100Hz  441)
     (set! T100ms 4410))
    ((48000)
     (set! T100Hz  480)
     (set! T100ms 4800))
    (else
     (error "Unsupported sample rate"))))

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

(define (comparator)
  (set! A  (sqrt (+ (* I I) (* Q Q))))
  (let ((a 0.995))
    (set! Av (+ (* a Av) (* (- 1.0 a) A))))
  (set! D (updateD (if (> A Av) 1 0))))

(define Aq (make-vector 10 0))

(define-constant MMM (list -1 -1 -1 -1 -1 -1 -1 -1 -1 -1))
(define-constant MM  (list +1 +1 +1 +1 +1 +1 +1 +1 -1 -1))
(define-constant M1  (list +1 +1 +1 +1 +1 -1 -1 -1 -1 -1))
(define-constant M0  (list +1 +1 -1 -1 -1 -1 -1 -1 -1 -1))

(define (push-A A)
  (vector-copy! Aq 0 Aq 1)
  (vector-set! Aq 9 A))
  
(define (correlate A)
  (define (corr lis)
    (let lp ((i 0)
             (P 0)
             (lis lis))
      (if (null? lis)
        P
        (lp (+ i 1)
            (+ P (* (car lis) (vector-ref Aq i)))
            (cdr lis)))))
  (push-A A)
  (let ((cMM (corr MMM))
        (cM  (corr MM))
        (c1  (corr M1))
        (c0  (corr M0)))
    (format #t "## CORR MM: ~5d \
                         M: ~5d \
                         1: ~5d \
                         0: ~5d ~%"
            cMM cM c1 c0)))

(define (process-buf inbuf n)
  (let lp ((i 0))
    (cond ((= i n) #t)
          ((< T T100ms)
           (let ((d (s16vector-ref inbuf i))
                 (a 0.998))
             (set! aI (+ (* a aI) (* (- 1 a) (Ibase T) d)))
             (set! aQ (+ (* a aQ) (* (- 1 a) (Qbase T) d))))
           (inc! T)
           (lp (+ i 1)))
          (else
           (set! T  0)
           (set! I  aI)
           (set! Q  aQ)
           (comparator)
           (correlate (if (> A Av) 1 -1))
           (set! aI 0)
           (set! aQ 0)
           (print #`",|I| ,|Q| ,|A| ,|Av| ,|D|")
           (lp i)))))

;;
;;
(define codes (make-vector 60))
(define cp #f)
(define cc 0)

(define prev-code #f)

(define (set-next-code code)
  (define (set-code)
    (print #`"## set-next cp: ,|cp| code: ,|code|")
    (if cp
      (begin
        (vector-set! codes cp code)
        (inc! cp)
        (if (= cp 60) (set! cp 0)))))

  (set! prev-code code)
  (cond ((eq? code 'E)
         (set! cp #f)
         (set-code))
        ((eq? code 'MM)
         (set! cp 0)
         (set-code))
        ((and (eq? 'M code) cp (= cp 59))
         (set-code)
         (decode-wwv))
        (else
         (set-code))))

(define (updateD Dn)
  (cond ((and (= D 0) (= Dn 1))
         (print #`"## Rising  edge cc: ,|cc|")
         (case cc
           ((19 20 21)
            (if (eq? prev-code 'M)
              (set-next-code 'MM)
              (set-next-code 'MME)))
           ((8 9 10 11) #t)
           (else
            (cond ((< cc 8)  (set-next-code 'too-short-period))
                  ((> cc 21) (set-next-code 'too-long-period))
                  (else      (set-next-code 'not-quite)))))
         (set! cc 0))
        ((and (= D 1) (= Dn 0))
         (print #`"## Falling edge cc: ,|cc|")
         (case cc
           ((1 2 3) (set-next-code 0))
           ((4 5 6) (set-next-code 1))
           ((7 8 9) (set-next-code 'M))
           (else    (set-next-code 'too-long-high)))))
  (inc! cc)
  Dn)

(define (decode-wwv)
  ;; for now....
  (vector-for-each-with-index
   (lambda (i m)
     (print #`"## codes[,|i|]: ,|m|"))
   codes)
  (set! cp 0))

;;
;;
(define (run)
  (let ((inbuf (make-s16vector BUFSIZ 0)))
    (read-wav-header)
    (print "# |I| |Q| |A| |Av| |D|")
    (let lp ((n (read-data inbuf)))
      (process-buf inbuf n)
      (if (= n BUFSIZ)
        (lp (read-data inbuf))
        0))))

(define (main args)  (run))

;;
;;
(define (read-data inbuf)
  ;; We know its 16bit monoral
  (let ((m (read-uvector! inbuf)))
    (if (eof-object? m)  0 m)))

(define-constant hsize 44)

(define (read-wav-header)

  (define (skip n)
    (if (<= n 0)
      #t
      (if (eof-object? (read-byte))
        (error "Unexpected EOF")
        (skip (- n 1)))))

  (define (rstr n r)
    (if (<= n 0)
      (list->string (map integer->char (reverse r)))
      (let ((c (read-byte)))
        (if (eof-object? c)
          (error "Unexpected EOF"))
        (rstr (- n 1) (cons c r)))))

  (define (ru16)
    (let* ((d0 (read-byte))
           (d1 (read-byte)))
      (+ (ash d1 8) d0)))

  (define (ru32)
    (let* ((d0 (read-byte))
           (d1 (read-byte))
           (d2 (read-byte))
           (d3 (read-byte)))
        (+ (ash d3 24)
           (ash d2 16)
           (ash d1  8)
           d0)))

  (define (read-fmt)
    (let* ((size          (ru32))
           (fmt           (ru16))
           (NumChannels   (ru16))
           (SampleRate    (ru32))
           (ByteRate      (ru32))
           (BlockAlign    (ru16))
           (BitsPerSample (ru16)))
      (skip (- size 16))
      (print #`"## ID fmt: ,|fmt| NumChannels: ,|NumChannels| \
                                        SampeRate: ,|SampleRate| \
                                        ByteRate: ,|ByteRate| \
                                        BlockAlign: ,|BlockAlign| \
                                        BitsPerSample: ,|BitsPerSample|")
      (if (and (= fmt 1)
               (= NumChannels 1)
               (= BitsPerSample 16))
        (set-T SampleRate)
        (error "Unsupported data format"))))

  ;;
  (let*  ((ChunkID    (rstr 4 '()))
          (ChunkSize  (ru32))
          (Format     (rstr 4 '())))

    (if (not (and (string=? ChunkID "RIFF")
                  (string=? Format  "WAVE")))
      (error "Not a wav file")))

      (let lp ((id (rstr 4 '())))
        (cond ((string=? "data" id) (ru32))
              ((string=? "fmt " id) (read-fmt) (lp (rstr 4 '())))
              (else (let ((siz (ru32)))
                      (print #`"## skipping unknown ID: \",|id|\" ,|siz| bytes")
                      (skip siz)
                      (lp (rstr 4 '())))))))
;;; EOF
