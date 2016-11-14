; -*- mode: Scheme; compile-command: "./run.sh all"; -*-
;;;
;;;  Demodurate WWV's 100Hz sub-carrier
;;;
(use math.const)
(use gauche.uvector)
(use gauche.vport)

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

;;
(define Ts  0)  ; Time index between samples.
(define Tm  #f) ; TIme index between markers.  #f means no marker found yet.

;; Output. Updated each samples.
(define I   0)  ; Filtered I
(define Q   0)  ; Filtered Q
(define A   0)  ; Analog out = sqrt(I^2 + Q^2)
(define Av  0)  ; Average Analog out
(define D   0)  ; Digitized A
(define cMM 0)  ; Minute Mark correlation
(define cM  0)  ; Mark correlation
(define c1  0)  ; 1 correlatoin
(define c0  0)  ; 2 correlation

;;
;;
(define (Ibase T)  (cos (/ (* 2 pi T) T100Hz)))
(define (Qbase T)  (sin (/ (* 2 pi T) T100Hz)))

(define (IbaseP T)
  (if (< T (* T100Hz 2/4)) +1 -1))

(define (QbaseP T)
  (cond ((< T (* T100Hz 1/4)) -1)
        ((< T (* T100Hz 3/4)) +1)
        (else     -1)))

;;
(define Aq (make-vector 10 1))  ; init with 1 to avoid false MM.

(define (push-A A)
  (vector-copy! Aq 0 Aq 1)
  (vector-set! Aq 9 A))

(define-constant MMM (list -1 -1 -1 -1 -1 -1 -1 -1 -1 -1))
(define-constant MM  (list +1 +1 +1 +1 +1 +1 +1 +1 -1 -1))
(define-constant M1  (list +1 +1 +1 +1 +1 -1 -1 -1 -1 -1))
(define-constant M0  (list +1 +1 -1 -1 -1 -1 -1 -1 -1 -1))

(define (corr-A M)
  (let lp ((i 0) (P 0) (M M))
    (cond ((null? M) P)
          (else
           (lp (+ i 1)
               (+ P (* (car M) (vector-ref Aq i)))
               (cdr M))))))
;;
(define aI 0)
(define aQ 0)

(define (process-buf inbuf n)
  (let lp ((i 0))
    (cond ((= i n) #t)
          ((< Ts T100ms)
           (let ((d (s16vector-ref inbuf i))
                 (a 0.998))
             (set! aI (+ (* a aI) (* (- 1 a) (Ibase Ts) d)))
             (set! aQ (+ (* a aQ) (* (- 1 a) (Qbase Ts) d))))
           (inc! Ts)
           (lp (+ i 1)))
          ((= Ts T100ms)
           (set! Ts  0)
           (set! I  aI)
           (set! Q  aQ)
           (set! aI  0)
           (set! aQ  0)
           (set! A  (sqrt (+ (* I I) (* Q Q))))
           (let ((a 0.995))
             (set! Av (+ (* a Av) (* (- 1.0 a) A))))
           (set! D (if (> A Av) 1 -1))
           (push-A D)
           (set! cMM (corr-A MMM))
           (set! cM  (corr-A MM))
           (set! c1  (corr-A M1))
           (set! c0  (corr-A M0))
           (detect-code)
           (print #`",|I| ,|Q| ,|A| ,|Av| ,|D| ,|cMM| ,|cM| ,|c1| ,|c0|")
           (lp i))
          (else
           (error "Something went wrong")))))

;;
;;
(define S   0)   ;  Second running index
(define S0  #f)  ;  Points zero second.
(define codes (make-vector 60 #f))

(define (inc!-S)
  (inc! S)
  (if (= S 60) (set! S 0)))

(define (get-codes)
  (if (not S0)
    (make-list 60 #f)
    (let lp ((s S0)
             (n 60)
             (r '()))
      (cond ((= n 0)   (reverse r))
            ((= s 60)  (lp 0 n r))
            (else
             (lp (+ s 1)
                 (- n 1)
                 (cons (vector-ref codes s) r)))))))

(define (disp-codes)
  (display "## codes: ")
  (let ((l (get-codes)))
    (print l)
    (if (or (eq? (car l) 'MM1)
            (eq? (car l) 'MM2))
      (decode-wwv l))))

(define (detect-code)
  (let ((M (cond ((and (> cM 7) (not Tm))
                  (print #`"## detect: Marker found, setting Tm=0, cM: ,|cM|")
                  (set! Tm 0)  'Mi)
                 ((and (> cM 7) (or (= Tm 100)))
                  (print #`"## detect: Sync'ed marker, setting Tm=0, cM: ,|cM|")
                  (set! Tm 0)  'M1)
                 ((and (> cM 7) (or (= Tm 200)))
                  (print #`"## detect: Sync'ed marker, setting Tm=0, cM: ,|cM|")
                  (set! Tm 0)  'M2)
                 ((and (> cM 7) (> Tm 205))
                  (print #`"## detect: Out of sync marker, setting Tm=0, cM: ,|cM|")
                  (set! Tm 0)  'M0)
                 ((> cM 7)    'MF) ; false marker
                 ((> cMM 7)   'MM) ; minute marker
                 ((> c1  7)   'B1)
                 ((> c0  7)   'B0)
                 (else #f))))
    (if (and Tm (= (modulo Tm 10) 0))
      (begin (print #`"## detect: Tm: ,|Tm| code: ,|M|")
             (every-second M)))
    (if Tm (inc! Tm))))

(define prev-code #f)

(define (every-second m)
  (define (MM? x) (or (eq? x 'MM0) (eq? x 'MM1) (eq? x 'MM2)))
  (cond ((not m) #f)
        ((and (eq? m 'MM) (eq? prev-code 'M1))
         (if (MM? (vector-ref codes S))
           (vector-set! codes S 'MM1)
           (vector-set! codes S 'MM0))
         (set! S0 S)
         (disp-codes))
        ((and (eq? m 'MM) (eq? prev-code 'M2))
         (if (MM? (vector-ref codes S))
           (vector-set! codes S 'MM2)
           (vector-set! codes S 'MM0))
         (set! S0 S)
         (disp-codes))
        ((eq? m 'MM) #f)
        ((eq? m 'MF) #f)
        (else
         (vector-set! codes S m)))
  (set! prev-code m)
  (inc!-S))

;;
;;
(define (decode-wwv l)

  (define (rbcd)
    (let* ((d0 (read-byte))
           (d1 (read-byte))
           (d2 (read-byte))
           (d3 (read-byte)))
      (if (or (= d0 255)
              (= d1 255)
              (= d2 255)
              (= d3 255))
        'x
        (+ (* 8 d3)
           (* 4 d2)
           (* 2 d1)
           (* 1 d0)))))

  (define (decode)
    (let* ((DST1  (begin (read-byte)   ; 0
                         (read-byte)   ; 1
                         (read-byte))) ; 2
           (LSW   (begin (read-byte))) ; 3
           (Y0    (begin (rbcd)))      ; 4 5 6 7
           (M0    (begin (read-byte)   ; 8
                         (read-byte)   ; 9
                         (rbcd)))      ; 10 11 12 13
           (M1    (begin (read-byte)   ; 14
                         (rbcd)))      ; 15 16 17 18
           (H0    (begin (read-byte)   ; 19
                         (rbcd)))      ; 20 21 22 23
           (H1    (begin (read-byte)   ; 24
                         (rbcd)))      ; 25 26 27 28
           (D0    (begin (read-byte)   ; 29
                         (rbcd)))      ; 30 31 32 33
           (D1    (begin (read-byte)   ; 34
                         (rbcd)))      ; 35 36 37 38
           (D2    (begin (read-byte)   ; 39
                         (rbcd)))      ; 40 41 42 43
           (DUT1  (begin (read-byte)   ; 44
                         (read-byte)   ; 45
                         (read-byte)   ; 46
                         (read-byte)   ; 47
                         (read-byte)   ; 48
                         (read-byte)   ; 49
                         (read-byte))) ; 50
           (Y1    (begin (rbcd)))      ; 51 52 53 54
           (DST2  (begin (read-byte)))); 55
      (print #`"## decode: ,|H1|,|H0|:,|M1|,|M0| \
                ,|D2|,|D1|,|D0|-th day of year 20,|Y1|,|Y0|.")))

  (let ((p (open-input-uvector
            (list->u8vector
             (map (lambda (m)
                    (case m
                      ((B0) 0)
                      ((B1) 1)
                      (else 255)))
                  l)))))
    (with-input-from-port p decode)))

;;
;;
(define-constant BUFSIZ 20)

(define (run)
  (let ((inbuf (make-s16vector BUFSIZ 0)))
    (read-wav-header)
    (print #`"# 1:|I| 2:|Q| 3:|A| 4:|Av| 5:|D| 6:|cMM| 7:|cM| 8:|c1| 9:|c0|")
    (let lp ((n (read-data inbuf)))
      (cond ((eof-object? n) 0)
            (else
             (process-buf inbuf n)
             (lp (read-data inbuf)))))))

(define (main args)  (run))

;;
;;   RIFF/WAV
;;   http://soundfile.sapp.org/doc/WaveFormat/
;;
;;
(define (read-data inbuf)
  ;; TODO: support multi channel...
  (let ((m (read-uvector! inbuf)))
    m))

;;
(define (read-wav-header)

  (define (skip n)
    (if (<= n 0)
      #t
      (if (eof-object? (read-byte))
        (error "Unexpected EOF")
        (skip (- n 1)))))

  #;(define (rstr n) (read-string n))
  (define (rstr n)
    (let lp  ((n n)
              (r '()))
      (if (= n 0)
        (list->string (map integer->char (reverse r)))
        (let ((c (read-byte)))
          (cond ((eof-object? c) c)
                ((zero? c) (lp (- n 1) (cons (char->integer #\space) r)))
                (else (lp (- n 1) (cons c r))))))))

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
      (print #`"## wav: ID fmt: ,|fmt| NumChannels: ,|NumChannels| \
                                       SampeRate: ,|SampleRate| \
                                       ByteRate: ,|ByteRate| \
                                       BlockAlign: ,|BlockAlign| \
                                       BitsPerSample: ,|BitsPerSample|")
      (if (and (= fmt 1)
               (= NumChannels 1)
               (= BitsPerSample 16))
        (set-T SampleRate)
        (error "Unsupported data format"))))

  (define (read-LIST size)
    (define (read-subchunk)
      (let ((id (rstr 4)))
        (cond ((string=? id "INFO")
               (print #`"## wav: LIST INFO:")
               (let lp ((id (rstr 4)))
                 (if (eof-object? id)
                   #t
                   (let ((str (rstr (ru32))))
                     (print #`"## wav:    ,|id|:\",|str|\"")
                     (lp (rstr 4))))))
              (else
               (print #`"## wav: LIST ,|id|: ,|size| bytes")))))
    (let* ((buf (read-uvector <u8vector> size))
           (p   (open-input-uvector buf)))
      (with-input-from-port p read-subchunk)))

  (let*  ((ChunkID    (rstr 4))
          (ChunkSize  (ru32))
          (Format     (rstr 4)))

    (if (not (and (string=? ChunkID "RIFF")
                  (string=? Format  "WAVE")))
      (error #`"Not a wav file \",|ChunkID|\"  \",|Format|\"")))

      (let lp ((id (rstr 4)))
        (cond ((string=? "data" id) (ru32))
              ((string=? "fmt " id) (read-fmt)  (lp (rstr 4)))
              ((string=? "LIST" id) (read-LIST (ru32)) (lp (rstr 4)))
              (else (let ((siz (ru32)))
                      (print #`"## wav: skipping unknown ID: \",|id|\" ,|siz| bytes")
                      (skip siz)
                      (lp (rstr 4)))))))
;;; EOF
