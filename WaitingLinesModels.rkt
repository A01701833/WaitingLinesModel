#lang racket

(define (add-to-end lst e) 
   (if (null? lst) 
       (list e)
       (cons (car lst)
             (add-to-end (cdr lst) e))))

(define (pow-tr2 a b)
  (define (pow-tr2-h b result)
    (if (= b 0)
        result
        (pow-tr2-h (- b 1) (* result a))))
  (pow-tr2-h b 1))

(define (factorial n)
 (cond ((< n 0) #f)
         ((<= n 1) 1)
         (else (* n (factorial (- n 1))))))

(define my_sqrt (lambda (a)
(exp (* 0.5 (log a)))))

;; End of Aux functions

;; BEG OF MG1 OR MD1

(define (mg1L k p o)
  (+ p (/ (+ (* (pow-tr2 k 2) (pow-tr2 o 2) ) (pow-tr2 p 2) ) (* 2 (- 1 p)) )))

(define (mg1P k s u)
  (/ k (* s u)))

;; END OF M G 1 OR MD1
;; BEG OF MM1

(define  mm1L
  (lambda (p)
    (/ p (- 1 p))))

(define (mmsL ps p s po)
  (+ ps (*(* (/ (pow-tr2 ps 2)(factorial s) ) (/ 1 (pow-tr2 (- 1 p) 2 )) ) po)))

;; M/M/S P(0)
( define (mmsPO p ps s)
   (/ 1 (+ (getsumPO p ps s 0 0) (/ (* (pow-tr2 ps s) p ) (* (factorial s) (- 1 p) ))))
    )

(define (getsumPO p ps s k acc)
  ( if (= k s)
       acc
       (getsumPO p ps s (+ k 1)(+ acc (/ (pow-tr2 ps k) (factorial k))))))

;; M/M/1 MTC

(define (mmsMTC k u da ca)
  (/ (+ k (my_sqrt (/ (* (* k u) da) ca ))) u ))


(define (mg1TC cs cd)
  (+ cd cs))


;; DEFINING M G 1

(define (getCS s ca)
  (* s ca))

(define(getCD l da)
  (* l da))

;; DEF MIN S

(define (minS k u n)
  (define res (* n u))
  (if (> res k)
      n
      (minS k u (+ 1 n))
      ))

;; FIRST CASE

(define k 34)
(define u 7)
(define o .14)
(define ca 2500)
(define da 4200)
(define low (minS k u 0))
(define p (mg1P k low u ))
(define l (mg1L k p o))
(define cs (getCS low ca))
(define cd (getCD l da))
(define ct (mg1TC cs cd))
;; FUNCTION FOR DISPLAYING TABLE

(define (prepare s p l cs cd ct)
   (string-append
   (string-append (string-append(number->string s) " | ")(string-append(number->string p) " | "))
   (string-append
   (string-append (string-append(number->string l) " | $")(string-append(number->string cs) " | $"))
   (string-append (string-append(number->string cd) " | $")(string-append(number->string ct) " | ")))))


 ;; CASE TEST 
(define title " S | P | L | CS | CD | CT | \n \n ")


;;  IF STANDARD DEVIATIONI IS 0
(define clow (minS k u 0))
(define cp (mg1P k low u ))
(define cl (mg1L k cp 0))
(define ccs (getCS clow ca))
(define ccd (getCD cl da))
(define cct (mg1TC ccs ccd))


;; SECOND CASE

(define (mm1Paux k s u)
  (/ (/ k s) u))


;; M/M/1

(define k2 45)
(define u2 12)
(define low2 (minS k2 u2 0))
(define ca2 1100)
(define da2 6000)

; First Level

(define paux2 (mm1Paux k2 low2 u2))
(define l3 (mm1L paux2))
(define p3 (mg1P k2 low2 u2 ))
(define cs3 (getCS low2 ca2))
(define cd3 (getCD l3 da2))
(define ct3 (mg1TC cd3 cs3))


; Optimal demonstration
(define opt (mmsMTC k2 u2 da2 ca2))
(define paux (mm1Paux k2 opt u2))
(define l2 (mm1L paux))
(define p2 (mg1P k2 opt u2 ))
(define cs2 (getCS opt ca2))
(define cd2 (getCD l2 da2))
(define ct2 (mg1TC cd2 cs2))



;; NOW PROCEDING TO M/M/S

(define low3 (minS k2 u2 0))
(define ps 3.75)
(define po (mmsPO p3 ps low3 ))
(define mL(mmsL ps p3 low3 po))
(define p4 (mg1P k2 low3 u2 ))
(define cs4 (getCS low3 ca2))
(define cd4 (getCD mL da2))
(define ct4 (mg1TC cd4 cs4))



;; AND NOW WITH OPTIMAL M/M/S
(define po2 (mmsPO p3 ps 8 ))
(define mL2(mmsL ps p3 8 po2))
(define p41 (mg1P k2 8 u2 ))
(define cs41 (getCS 8 ca2))
(define cd41 (getCD mL2 da2))
(define ct41 (mg1TC cd41 cs41))





;; DISPLAY
(display "Records are represented in the following order \n")
(display title)
(newline)
(display "First Case: Minimun amount is 5 Servers \n")
(prepare low  p l cs cd ct)
(display "When Standard Deviation is 0 it is optimal \n")
(prepare clow  cp cl ccs ccd cct)

(display "Second Case: M/M/1 \n")
(prepare low2  p3 l3 cs3 cd3 ct3)
(display "Optimal: \n")
(prepare 8  p2 l2 cs2 cd2 ct2)
(display "Case M/M/S \n")
(prepare low3 p4 mL cs4 cd4 ct4)
(display "Optimal: \n")
(prepare 8 p41 mL2 cs41 cd41 ct41)



;; TESTING
(newline)
;;(display " TESTING \n")
;; M/G/1
;;(define kT 34)
;;(define uT 7)
;;(define oT 0)
;;(define caT 2500)
;;(define daT 4200)
;;(define lowT 8 )
;;(define pT (mg1P kT lowT uT ))
;;(define lT (mg1L kT pT oT))
;;(define csT (getCS lowT caT))
;;(define cdT (getCD lT daT))
;;(define ctT (mg1TC csT cdT))

;;(prepare lowT pT lT csT cdT ctT)


;; M/M/1

;;(define kK 45)
;;(define uK 12)
;;(define lowK 8)
;;(define caK 1100)
;;(define daK 6000)

; First Level

;;(define pauxK (mm1Paux kK lowK uK))
;;(define lK (mm1L pauxK))
;;(define pK (mg1P kK lowK uK ))
;;(define csK (getCS lowK caK))
;;(define cdK (getCD lK daK))
;;(define ctK (mg1TC cdK csK))
;;(prepare lowK  pK lK csK cdK ctK)

;; M/M/S
;;(define kM 45)
;;(define uM 12)
;;(define lowM 8)
;;(define caM 1100)
;;(define daM 6000)
;;(define psM 3.75)

;;(define pM (mg1P k2 lowM uM ))
;;(define poM (mmsPO pM psM lowM ))
;;(define mLM(mmsL psM pM lowM poM))
;;(define csM (getCS lowM caM))
;;(define cdM (getCD mLM daM))
;;(define ctM (mg1TC cdM csM))
;;(prepare lowM  pM mLM csM cdM ctM)

;; THIS SECTION IS FOR PDF DOCUMENTATION
