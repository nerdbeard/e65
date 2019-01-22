(setq typical '(seq c=0 binary enable v=0))

(setq typ1 '(alt (seq <0\? (1+ m))
                 (1- b))
      m 100
      b 101)

(setq typ2 '(not (loop (seq <0\? (1+ m)))))

(setq typ3 '(star (seq (li s) ~=0\? pop)))

(setq typ3 '(not (loop (seq (li s) ~=0\? pop))))

(setq incr
      '(seq (li \# 0)
            c=1
            (while c=1\?
              (seq (l i n) (+ \# 0) (st i n))))
      n 32)

(setq sum
      '(seq (l \# 0)
            li
            (while (seq (ci n) ~=\?)
              (seq (+ i A) i+1)))
      A 29)

(setq mult
      '(seq (l x) (st xp)
            (l y) (st y0p)
            (l \# 0) (st y1p) (st z0) (st z1)
            (while (seq (lsr xp) ~=\?)
              (seq c=0 (l z0) (+ y0p) (st z0)
                   (l z1) (+ y1p) (st z1)
                   (asl y0p) (rl y1p)))))

(setq x 1 xp 2
      y 3 y0p 4
      y1p 5 z1 6
      z0 7)

(setq masktable
      '(compile '(seq 128 64 32 16 8 4 2 1) 0 0)
      p 8)

(setq testb
      '(seq push
            (land \# 7)
            (li a)
            pop
            (3 lsr)
            (lj a)
            (l @j p)
            (land i masktable)
            return))

(setq setb
      '(seq push
            (land \# 7)
            (li a)
            pop
            (3 lsr)
            (lj a)
            (l @j p)
            (lor i masktable)
            (st @j p)
            return))

;;; Routines for the Universal Product Code Wand.  Requires 88 bytes+table.
(setq upctable
      (compile '(seq 13 25 19 61 35 49 47 59 55 11) 0 0))

(setq
 code 11
 digit 12
 mq 13
 upcwand
 '(alt
   (seq (fori (\# 6) (\# 12)            ; complement patterns of right 6 upc digits.
              (l i code)
              (lxor \# 127)
              (st i code))
        (fori (\# 0) (\# 12)
              (l i code)
              (not
               (forj (\# 0) (\# 10)
                     (c j upctable)
                     ~=\?))             ; fail if equal.
              (stj i digit))            ; store index of upctable.
        decimal                         ; set decimal arithmetic mode.
        (l \# 0)                        ; clear ac.
        (fori (\# 0) (\# 12)            ; add up the even digits.
              (+ i digit)               ; loop control clears carry!
              i+1)                      ; only every other one.
        (st mq)                         ; save partial sum.
        c=0                             ; clear the carry.
        (2 (+ mq))                      ; multiply by 3.
        (fori (\# 1) (\# 12)            ; add up the odd digits.
              (+ i digit)               ; loop cotrol clears carry.
              i+1)                      ; only every other one.
        (lxor \# 15)                    ; select low decimal digit.
        =0\?                            ; fails if non-zero.
        return)
   (seq break                           ; signal failure.
        return)))
