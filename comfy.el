(defun comfy-sublis (a e)
  (cond ((atom e)
         (let* ((binding (assq e a)))
           (cond (binding (cdr binding))
                 (t e))))
        (t (cons (comfy-sublis a (car e))
                 (comfy-sublis a (cdr e))))))

(defun comfy-put (symbol propname value)
  (let ((symbol (intern (concat "comfy-" (symbol-name symbol)))))
    (put symbol
	 propname ;; (format "comfy-%s" propname)
	 value)))

(defun comfy-get (symbol propname)
  (let ((symbol (intern (concat "comfy-" (symbol-name symbol)))))
    (get symbol propname)))

;;; Basic test instructions.
(comfy-put 'c=1\? 'test 176)       ;;; test carry=1.
(comfy-put 'c=0\? 'test 144)       ;;; test carry=0.
(comfy-put 'llt 'test 144)         ;;; logically less than.
(comfy-put 'lge 'test 176)         ;;; logically greater than or equal.
(comfy-put '=\? 'test 240)         ;;; equal.
(comfy-put '~=\? 'test 208)        ;;; not equal.
(comfy-put '=0\? 'test 240)        ;;; equals zero.
(comfy-put '~=0\? 'test 208)       ;;; not equal to zero.
(comfy-put 'v=1\? 'test 112)       ;;; test overflow=1.
(comfy-put 'v=0\? 'test 80)        ;;; test overflow=0.
(comfy-put '<\? 'test 48)          ;;; test arithmetic less than.
(comfy-put '>=\? 'test 16)         ;;; test arithmetic greater than or equal.
(comfy-put '<0\? 'test 48)         ;;; test arithmetic less than zero.
(comfy-put '>=0\? 'test 16)        ;;; test airthmetic greater than or equal to zero.

;;; Group 0.
(comfy-put '\? 'skeleton 32)       ;;; test.
(comfy-put 'stj 'skeleton 152)     ;;; store j.
(comfy-put 'lj 'skeleton 168)      ;;; load j.
(comfy-put 'cj 'skeleton 200)      ;;; compare j.
(comfy-put 'ci 'skeleton 232)      ;;; compare i.
;;; Group 1.
(comfy-put 'lor 'skeleton 17)      ;;; logical or.
(comfy-put 'land 'skeleton 49)     ;;; logical and.
(comfy-put 'lxor 'skeleton 81)     ;;; logical xor.
(comfy-put '+ 'skeleton 113)       ;;; add with carry.
(comfy-put 'st 'skeleton 145)      ;;; store accumulator.
(comfy-put 'l 'skeleton 177)       ;;; load accumulator.
(comfy-put 'c 'skeleton 209)       ;;; compare accumulator.
(comfy-put '- 'skeleton 241)       ;;; subtract with borrow.
;;; Group 2.
(comfy-put 'asl 'skeleton 10)      ;;; arithmetic shift left.
(comfy-put 'rl 'skeleton 42)       ;;; rotate left.
(comfy-put 'lsr 'skeleton 74)      ;;; logical shift right.
(comfy-put 'rr 'skeleton 106)      ;;; rotate right.
(comfy-put 'sti 'skeleton 138)     ;;; store i.
(comfy-put 'li 'skeleton 170)      ;;; load i.
(comfy-put '1- 'skeleton 194)      ;;; decrement.
(comfy-put '1+ 'skeleton 226)      ;;; increment.
;;; random instructions.
(comfy-put 'trap 'skeleton 0)      ;;; programmed break.
(comfy-put 'save 'skeleton 8)      ;;; push processor state onto stack.
(comfy-put 'restore 'skeleton 40)  ;;; restore processor state from stack.
(comfy-put 'push 'skeleton 72)     ;;; push accumulator onto stack.
(comfy-put 'pop 'skeleton 104)     ;;; pop accumulator from stack.
(comfy-put 'c=0 'skeleton 24)      ;;; clear carry.
(comfy-put 'c=1 'skeleton 56)      ;;; set carry.
(comfy-put 'seb 'skeleton 24)      ;;; set borrow.
(comfy-put 'clb 'skeleton 56)      ;;; clear borrow.
(comfy-put 'v=0 'skeleton 184)     ;;; clear overflow.
(comfy-put 'enable 'skeleton 88)   ;;; enable interrupts.
(comfy-put 'disable 'skeleton 120) ;;; disable interrupts.
(comfy-put 'binary 'skeleton 216)  ;;; set binary mode.
(comfy-put 'decimal 'skeleton 248) ;;; set decimal mode.
(comfy-put 'i+1 'skeleton 232)     ;;; increment i.
(comfy-put 'j+1 'skeleton 200)     ;;; increment j.
(comfy-put 'i-1 'skeleton 202)     ;;; decrement i.
(comfy-put 'j-1 'skeleton 136)     ;;; decrement j.
(comfy-put 'nop 'skeleton 234)     ;;; no operation.

(defvar comfy-jmp 76)
(setq comfy-jmp 76)
(defvar comfy-jsr 32)
(setq comfy-jsr 32)
(comfy-put 'return 'jump 96)
(comfy-put 'resume 'jump 64)

(defvar comfy-mem (make-vector 10 0)
  "Vector where the compiled code is placed.")
(setq comfy-mem (make-vector 100 0))

(defvar comfy-f (length comfy-mem)
  "Compiled code array pointer; it works its way down from the top.")
(setq comfy-f (length comfy-mem))

(defun comfy-init ()
  ;; XXX NOT CALLED -- but useful
  (fillarray comfy-mem 0)
  (setq comfy-f (length comfy-mem)))

(defun comfy-subseq (v i)
  ;; XXX NOT CALLED -- is this any different than seq-subseq?
  (let* ((nv (make-vector (- (length v) i) nil))
         (j 0))
    (while (< j (length nv))
      (aset nv j (elt v (+ i j)))
      (setq j (1+ j)))
    nv))

(defun comfy-gen (obj)
  ;;; place one character "obj" into the stream.
  (setq comfy-f (1- comfy-f))
  (aset comfy-mem comfy-f obj)
  comfy-f)

(defun comfy-testp (e)
  ;;; predicate to tell whether "e" is a test.
  (and (symbolp e) (comfy-get e 'test)))

(defun comfy-actionp (e)
  ;;; predicate to tell whether "e" is an action.
  (and (symbolp e) (not (comfy-get e 'test))))

(defun comfy-jumpp (e)
  ;;; predicate to tell whether "e" is a jump-type action.
  (and (symbolp e) (comfy-get e 'jump)))

(defun comfy-macrop (x)
  (and (symbolp x) (comfy-get x 'cmacro)))

(defun comfy-ra (b a)
  ;;; replace the absolute address at the instruction "b"
  ;;; by the address "a".
  (let* ((ha (lsh a -8))
         (la (logand a 255)))
    (aset comfy-mem (1+ b) la)
    (aset comfy-mem (+ b 2) ha))
  b)

(defun comfy-inv (c)
  ;;; invert the condition for a branch.
  ;;; invert bit 5 (counting from the right).
  (logxor c 32))

(defun comfy-genbr (win)
  ;;; generate an unconditional jump to "win".
  (comfy-gen 0) (comfy-gen 0) (comfy-gen comfy-jmp) (comfy-ra comfy-f win))

(defun comfy-8bitp (n)
  (let* ((m (logand n -128)))
    (or (= 0 m) (= -128 m))))

(defun comfy-genbrc (c win lose)
  ;;; generate an optimized conditional branch
  ;;; on condition c to "win" with failure to "lose".
  (let* ((w (- win comfy-f)) (l (- lose comfy-f)))   ;;; Normalize to current point.
    (cond ((= w l) win)
          ((and (= l 0) (comfy-8bitp w)) (comfy-gen w) (comfy-gen c))
          ((and (= w 0) (comfy-8bitp l)) (comfy-gen l) (comfy-gen (comfy-inv c)))
          ((and (comfy-8bitp l) (comfy-8bitp (- w 2)))
           (comfy-gen l) (comfy-gen (comfy-inv c)) (comfy-gen (- w 2)) (comfy-gen c))
          ((and (comfy-8bitp w) (comfy-8bitp (- l 2)))
           (comfy-gen w) (comfy-gen c) (comfy-gen (- l 2)) (comfy-gen (comfy-inv c)))
          ((comfy-8bitp (- l 3)) (comfy-genbrc c (comfy-genbr win) lose))
          (t (comfy-genbrc c win (comfy-genbr lose))))))

(defun comfy-ogen (op a)
  ;;; put out address and op code into stream.
  ;;; put out only one byte address, if possible.
  (let* ((ha (lsh a -8))
         (la (logand a 255)))
    (cond ((= ha 0) (comfy-gen la) (comfy-gen op))
          (t (comfy-gen ha) (comfy-gen la) (comfy-gen (+ op 8))))))

(defun comfy-skeleton (op)
  ;;; return the skeleton of the op code "op".
  ;;; the "skeleton" property of op contains either
  ;;; the code for "accumulator" (groups 0,2) or "immediate" (1) addressing.
  (logand (comfy-get op 'skeleton) 227))

(defun comfy-emit (i win)
  ;;; place the unconditional instruction "i" into the stream with
  ;;; success continuation "win".
  (cond ((not (= win comfy-f)) (comfy-emit i (comfy-genbr win)))
        ;;; atom is a single character instruction.
        ((symbolp i) (comfy-gen (comfy-get i 'skeleton)))
        ;;; no op code indicates a subroutine call.
        ((null (cdr i))
         (comfy-gen 0) (comfy-gen 0) (comfy-gen comfy-jsr) (comfy-ra comfy-f (eval (car i))))
        ;;; "a" indicates the accumulator.
        ((eq (cadr i) 'a) (comfy-emit (car i) win))
        ;;; "s" indicates the stack.
        ((eq (cadr i) 's)
         (comfy-gen (+ (comfy-skeleton (car i)) 24)))
        ;;; length=2 indicates absolute addressing.
        ((= (length i) 2)
         (comfy-ogen (+ (comfy-skeleton (car i)) 4)
               (eval (cadr i))))
        ;;; "i" indicates absolute indexed by i.
        ((eq (cadr i) 'i)
         (comfy-ogen (+ (comfy-skeleton (car i)) 20) (eval (caddr i))))
        ;;; "j" indicates absolute indexed by j.
        ;;; this cannot be optimized for page zero addresses.
        ((eq (cadr i) 'j)
         (comfy-gen 0) (comfy-gen 0) (comfy-gen (+ (comfy-skeleton (car i)) 24))
         (comfy-ra comfy-f (eval (caddr i))))
        ;;; "\#" indicates immediate operand.
        ((eq (cadr i) '\#)
         (comfy-ogen (- (comfy-get (car i) 'skeleton) 8)
               (logand (eval (caddr i)) 255)))
        ;;; "i@" indicates index by i, the indirect.
        ((eq (cadr i) 'i@)
         (comfy-ogen (comfy-skeleton (car i))
               (logand (eval (caddr i)) 255)))
        ;;; "@j" indicates indirect, then index by j.
        ((eq (cadr i) '@j)
         (comfy-ogen (+ (comfy-skeleton (car i)) 16)
               (logand (eval (caddr i)) 255)))))

(defun comfy-compile (e win lose)
  ;;; comfy-compile expression e with success continuation "win" and
  ;;; failure continuation "lose".
  ;;; "win" an "lose" are both addresses of stuff higher in memory.
  (cond ((numberp e) (comfy-gen e))           ; allow constants.
        ((comfy-macrop e)
         (comfy-compile (apply (comfy-get e 'cmacro) (list e)) win lose))
        ((comfy-jumpp e) (comfy-gen (comfy-get e 'jump))) ; must be return or resume.
        ((comfy-actionp e) (comfy-emit e win))      ; single byte instruction.
        ((comfy-testp e) (comfy-genbrc (comfy-get e 'test) win lose)) ; test instruction
        ((eq (car e) 'not) (comfy-compile (cadr e) lose win))
        ((eq (car e) 'seq)
         (cond ((null (cdr e)) win)
               (t (comfy-compile (cadr e)
                           (comfy-compile (cons 'seq (cddr e)) win lose)
                           lose))))
        ((eq (car e) 'loop)
         (let* ((l (comfy-genbr 0))
                (r (comfy-compile (cadr e) l lose)))
           (comfy-ra l r)
           r))
        ((numberp (car e))              ; duplicate n times.
         (cond ((zerop (car e)) win)
               (t (comfy-compile (cons (1- (car e)) (cdr e))
                           (comfy-compile (cadr e) win lose)
                           lose))))
        ((eq (car e) 'if)               ; if-then-else.
         (comfy-compile (cadr e)
                  (comfy-compile (caddr e) win lose)
                  (comfy-compile (cadddr e) win lose)))
        ((eq (car e) 'while)            ; do-while.
         (let* ((l (comfy-genbr 0))
                (r (comfy-compile (cadr e)
                            (comfy-compile (caddr e) l lose)
                            win)))
           (comfy-ra l r)
           r))
        ;;; allow for COMFY macros !
        ((comfy-macrop (car e))
         (comfy-compile (apply (comfy-get (car e) 'cmacro) (list e)) win lose))
        (t (comfy-emit e win))))

(comfy-put
 'alt
 'cmacro
 '(lambda (e)
    ;;; define the dual of "seq" using DeMorgan's law.
    (list 'not
          (cons 'seq
                (mapcar '(lambda (e) (list 'not e))
                        (cdr e))))))

(comfy-put
 'call
 'cmacro
 '(lambda (e)
    (let* ((p (cadr e)) (pl (cddr e)))
      (comfy-sublis (list (cons 'pushes (genpush pl))
                    (cons 'p p)
                    (cons 'n (length pl)))
              '(seq (seq . pushes)
                    (p)
                    (li s)
                    (land ii)
                    (sti s))))))

(comfy-put
 'lambda
 'cmacro
 '(lambda (e)
    (let* ((pl (cadr e))
           (body (cddr e)))
      (comfy-sublis (list (cons 'body body)
                    (cons 'xchs (genxchs pl))
                    (cons 'moves (genmoves pl)))
              '(seq (li s)
                    (seq . xchs)
                    (seq . body)
                    (li s)
                    (seq . moves)
                    (return))))))

(defun comfy-genxchs (pl)		; XXX NOT CALLED
  (cond ((null pl) pl)
        (t (cons (list 'xch (list 'i (+ 258 (length pl))) (list (car pl)))
                 (comfy-genxchs (cdr pl))))))

(defun comfy-genmoves (pl)		; XXX NOT CALLED
  (cond ((null pl) nil)
        (t (cons (list 'move (list 'i (+ 258 (length pl))) (list (car pl)))
                 (comfy-genmoves (cdr pl))))))

(defun comfy-genpush (pl)		; XXX NOT CALLED
  (cond ((null pl) pl)
        (t (let* ((p (car pl)))
             (append (` ((l (, p)) push)) (comfy-genpush (cdr pl)))))))

(defun comfy-match (p e comfy-f alist)
  ;;; comfy-f is a function which is executed if the comfy-match fails.
  ;;; comfy-f had better not return.
  (cond ((constantp p)
         (cond ((eq p e) alist)
               (t (funcall comfy-f))))
        ((variablep p) (cons (cons (cadr p) e) alist))
        ((eq (car p) 'quote) (cond ((eq (cadr p) e) alist)
                                   (t (funcall comfy-f))))
        ((predicate p) (cond ((funcall (cadr p) e) alist)
                             (t (funcall comfy-f))))
        ((atom e) (funcall comfy-f))
        (t (comfy-match (car p)
                  (car e)
                  comfy-f
                  (comfy-match (cdr p)
                         (cdr e)
                         comfy-f
                         alist)))))

(defun comfy-predicate (x)		; XXX NOT CALLED
  (and (consp x) (eq (car x) 'in)))

(defun comfy-constantp (x) (atom x))	; XXX NOT CALLED

(defun comfy-variablep (x)		; XXX NOT CALLED
  (and (consp x) (eq (car x) '\,)))

(defmacro comfy-cases (&rest a)
  (` (quote
      (, (catch 'comfy-cases
           (fapplyl (cdr a)
                    (eval (car a))
                    '(lambda () (throw 'comfy-cases nil))))))))

(defun comfy-fapplyl (fl a fail)	; XXX NOT CALLED
  ;;; "fail" is a function which is executed if comfy-fapplyl fails.
  ;;; "fail" had better not return.
  (cond ((null fl) (funcall fail))
        (t (catch 'comfy-fapplyl
             (fapply (car fl) a
                     '(lambda ()
                        (throw 'comfy-fapplyl
                               (comfy-fapplyl (cdr fl) a fail))))))))

(defun comfy-fapply (comfy-f a fail)	; XXX NOT CALLED
  (let* ((alist (comfy-match (cadr comfy-f) a fail nil)))
    (apply (cons 'lambda
                 (cons (mapcar 'car alist)
                       (cddr comfy-f)))
           (mapcar 'cdr alist))))

(defmacro comfy-define (&rest a)
  (let* ((ind (car a))
         (patt (cadr a))
         (body (cddr a))
         (where (cond ((atom patt) patt)
                      ((atom (car patt)) (car patt)))))
    (or (comfy-get where ind) (comfy-put where ind '(lambda (e) (comfy-cases e))))
    (comfy-put
     where
     ind
     (` (lambda (e)
          (, (append (` (comfy-cases e (, (append (` (lambda (, patt))) body))))
                     (cddr (caddr (comfy-get where ind))))))))
    nil))

(comfy-put 'star 'cmacro nil)

(comfy-define cmacro (star . (, body))
  (` (not (loop (, (append '(seq) body))))))

(comfy-put 'i2 'cmacro nil)

(comfy-define cmacro (i2 (, p))
  (` (seq (1+ (, p))
          (if =0\? (1+ (1+ (, p)))
            (seq)))))

(comfy-put 'move 'cmacro nil)

(comfy-define cmacro (move (, x) (, y))
  (` (seq (, (append '(l) x))
          (, (append '(st) y)))))

(comfy-put 'prog 'cmacro nil)

(comfy-define cmacro (prog ((, v)) . (, body))
  (` (seq push
          (li s)
          (move ((, v)) (i 257))
          (, (append '(seq) body))
          (li s)
          (move (i 257) ((, v)))
          i-1
          (sti s))))

(comfy-put 'fori 'cmacro nil)

(comfy-define cmacro (fori (, from) (, to) . (, body))
  (` (seq (, (append '(li) from))
          (while (seq (, (append '(ci) to)) llt)
            (seq (, (append '(seq) body)) i+1)))))

(comfy-put 'forj 'cmacro nil)

(comfy-define cmacro (forj (, from) (, to) . (, body))
  (` (seq (, (append '(lj) from))
          (while (seq (, (append '(cj) to)) llt)
            (seq (, (append '(seq) body)) j+1)))))

(comfy-put 'for 'cmacro nil)

(comfy-define cmacro (for (, v) (, from) (, to) . (, body))
  (` (seq (, (append '(l) from)) (, (append '(st) v))
          (while (seq (, (append '(c) to)) llt)
            (seq (, (append '(seq) body))
                 (, (append '(1+) v))
                 (, (append '(l) v)))))))
