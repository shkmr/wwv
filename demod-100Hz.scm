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
           (set! A  (sqrt (+ (* I I) (* Q Q))))
           (let ((a 0.998))
             (set! Av (+ (* a Av) (* (- 1.0 a) A))))
           (set! D (if (> A Av) 1 0))
           (set! aI 0)
           (set! aQ 0)
           (print #`",|I| ,|Q| ,|A| ,|Av| ,|D|")
           (lp i)))))

(define codes (make-vector 60))
(define cp #f)
(define cc 0)

(define prev-code #f)

(define (set-next-code code)
  (cond ((eq? code 'MM)
         (set! cp 0))
        ((and (eq? 'M code) cp (= cp 59))
         (decode-wwv)))
  (if cp (vector-set! codes cp code))
  (print #`"BBB ,|cp| ,|code|")
  (set! prev-code code)
  (if cp
    (begin
      (inc! cp)
      (if (>= cp 60)
        (set! cp 0)))))


(define (decode-wwv)
  ;; for now....
  (vector-for-each-with-index
   (lambda (i m)
     (print #`"AAA ,|i| ,|m|"))
   codes)
  (set! cp 0))

(define (updateD Dn)
  (cond ((and (= D 0) (= Dn 1))
         (if (and (eq? 'M prev-code)
                  (<= 9 cc 11))
           (set-next-code 'MM))
         (set! cc 0))
        ((and (= D 1) (= Dn 0))
         (set-next-code (case cc
                          ((1 2 3) 0)
                          ((4 5 6) 1)
                          ((7 8 9) 'M)
                          (else
                           (set! cp #f)
                           'E)))))
  (inc! cc)
  Dn)

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
