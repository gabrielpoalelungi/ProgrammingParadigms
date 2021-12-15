#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)


; TODO
; Aveți libertatea să vă structurați programul cum doriți (dar cu restricțiile
; de mai jos), astfel încât funcția serve să funcționeze conform specificației.
; 
; Restricții (impuse de checker):
; - trebuie să existe în continuare funcția (empty-counter index)
; - cozile de la case trebuie implementate folosind noul TDA queue

(define-struct counter (index open tt et queue) #:transparent)

(define (empty-counter index)
  (make-counter index #t 0 0 empty-queue)
)
  
  
; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 3, apare un nou tip de cerere, așadar
; requests conține 5 tipuri de cereri (cele moștenite din etapa 3 plus una nouă):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă              (ca înainte)
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute         (ca înainte)
;   - (ensure <average>) - cât timp tt-ul mediu al caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)           (ca înainte)
;   - <x> - trec <x> minute de la ultima cerere, iar starea caselor se actualizează
;           corespunzător (cu efect asupra câmpurilor tt, et, queue)                       (ca înainte)
;   - (close <index>) - casa index este închisă                                            (   NOU!   )
; Sistemul trebuie să proceseze cele 5 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele DESCHISE cu tt minim; nu se va întâmpla
;   niciodată ca o persoană să nu poată fi distribuită la nicio casă                       (mică modificare)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți);
;   nu aplicați vreun tratament special caselor închise                                    (ca înainte)
; - tt-ul mediu (ttmed) se calculează pentru toate casele DESCHISE, 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>         (mică modificare)
; - când timpul prin sistem avansează cu <x> minute, tt-ul, et-ul și queue-ul tuturor 
;   caselor se actualizează pentru a reflecta trecerea timpului; dacă unul sau mai mulți 
;   clienți termină de stat la coadă, ieșirile lor sunt contorizate în ordine cronologică. (ca înainte)
; - când o casă se închide, ea nu mai primește clienți noi; clienții care erau deja acolo
;   avansează normal, până la ieșirea din supermarket                                    
; Rezultatul funcției serve va fi o pereche cu punct între:
; - lista sortată cronologic a clienților care au părăsit supermarketul:
;   - fiecare element din listă va avea forma (index_casă . nume)
;   - dacă mai mulți clienți ies simultan, sortați-i crescător după indexul casei
; - lista cozilor (de la case DESCHISE sau ÎNCHISE) care încă au clienți:
;   - fiecare element va avea forma (index_casă . coadă) (coada este de tip queue)
;   - lista este sortată după indexul casei


;AFISAREA QUEUE-URILOR
(define (list-of-queues counters acc)
  (if (null? counters)
      acc
      (if (queue-empty? (counter-queue (car counters)))
          (list-of-queues (cdr counters) acc)
          (list-of-queues (cdr counters) (append acc (list (cons (counter-index (car counters)) (counter-queue (car counters))))))
      )
  )
)

;UPDATE
(define (update-acc f counters index acc)
  (if (null? counters)
      acc
      (if (= index (counter-index (car counters)))
             (update-acc f (cdr counters) index (append acc (list(f (car counters)))))
             (update-acc f (cdr counters) index (append acc (list (car counters))))
      )
  )
)

(define (update f counters index)
  (update-acc f counters index null)
)

;MIN-TT & MIN-ET
(define (get-tt C)
	(counter-tt C)
)
(define (get-et C)
	(counter-et C) 
)

(define (general-min f counters acc)
	(if (null? counters)
		acc
		(if (< (f (car counters)) (cdr acc))
			(general-min f (cdr counters) (cons (counter-index (car counters)) (f (car counters))))
			(general-min f (cdr counters) acc)
		)
	)
)

(define (min-tt counters)
	(general-min get-tt counters (cons (counter-index (car counters)) (get-tt (car counters))))
)

(define (min-et counters)
  (general-min get-et counters (cons (counter-index (car counters)) (get-et (car counters))))
)

;ADD TO COUNTER

(define (add-to-counter name items)
  (λ (C)                             
    (struct-copy counter C
               [tt (+ (counter-tt C) items)]
               [queue (enqueue (cons name items) (counter-queue C))]
              [et (if (queue-empty? (counter-queue C))
                       (+ (counter-et C) items)
                       (counter-et C)
                   )
               ]
        )
  ))

;PASS TIME FOR <X> REQUEST

(define (pass-time-through-counter minutes)
  (λ (C)
    (if (> minutes (counter-tt C))
        (struct-copy counter C
               [tt 0]
               [et 0])
        (if (> minutes (counter-et C))
            (struct-copy counter C
               [tt (- (counter-tt C) minutes)]
               [et 0])
            (struct-copy counter C
               [tt (- (counter-tt C) minutes)]
               [et (- (counter-et C) minutes)])
        )
    )
))


; GET COUNTER BY INDEX
(define (get-counter-by-index counters index)
  (if (= index (counter-index (car counters)))
      (car counters)
      (get-counter-by-index (cdr counters) index))
)

; REMOVE FROM COUNTER
(define (remove-from-counter)
  (λ(C)
    (if (queue-empty? (counter-queue C))
        C
        (if (zero? (counter-et C))
            (struct-copy counter C
                         [queue (dequeue (counter-queue C))]
                         [et (if (queue-empty? (dequeue (counter-queue C)))
                                 0
                                 (cdr (top (dequeue(counter-queue C)))))]
            )
            C
        )
    )
  )
)

; PASS TIME FOR ALL COUNTERS
(define (pass-time-for-all-counters minutes counters acc)
  (if (zero? minutes)
      (cons (map append acc) counters)
      (pass-time-for-all-counters (- minutes 1)
                                  (map (remove-from-counter) (map (pass-time-through-counter 1) counters))
                                  (append acc (foldl (lambda(counter rez)
                                                            (if (queue-empty? (counter-queue counter))
                                                                rez
                                                                (if (zero? (- (counter-et counter) 1))
                                                                    (append rez (list (cons  (counter-index counter) (car (top (counter-queue counter))))))
                                                                    rez
                                                                )
                                                             )
                                                      )
                                                    null counters
                                               )
                                   )
       )
                                  
 )
)

; CLOSE
(define (close-method)
  (λ(C)
    (struct-copy counter C
               [open #f])
  )
)

;AVERAGE TT
(define (average-tt-acc counters sum number)
  (if (null? counters)
      (/ sum number)
      (if (counter-open (car counters))
          (average-tt-acc (cdr counters) (+ sum (counter-tt (car counters))) (+ number 1))
          (average-tt-acc (cdr counters) sum number)
      )
  )
)

(define (average-tt counters)
  (average-tt-acc counters 0 0)
)

;GET-LAST-INDEX
(define (get-last-index counters)
  (if (null? (cdr counters))
      (counter-index (car counters))
      (get-last-index (cdr counters))
  )
)

;ENSURE
(define (ensure-acc fast-counters average acc)
  (if (<= (average-tt (append fast-counters acc)) average)
      acc
      (ensure-acc fast-counters average (append acc (list(empty-counter (+ (get-last-index acc) 1)))))
  )
)

(define (ensure-method fast-counters slow-counters average)
  (ensure-acc fast-counters average slow-counters)
)

;DELAY-MINUTES
(define delay-minutes
  (λ (minutes)
    (λ (C)
      (struct-copy counter C
                   [et (+ (counter-et C) minutes)]
                   [tt (+ (counter-tt C) minutes)]
      )
    )
  )
)

;GET-OPEN-COUNTERS
(define (get-open-counters counters acc)
  (if (null? counters)
      acc
      (if (counter-open (car counters))
          (get-open-counters (cdr counters) (append acc (list (car counters))))
          (get-open-counters (cdr counters) acc)
      )
  )
)

;SERVE

(define (serve requests fast-counters slow-counters)
  (serve-aux requests fast-counters slow-counters '())
)

(define (serve-aux requests fast-counters slow-counters acc)
  (if (null? requests)
      (append (list acc) (append (list-of-queues fast-counters null) (list-of-queues slow-counters null)))
      (match (car requests)
        [(list 'close index)
         (serve-aux (cdr requests)
                    (update (close-method) fast-counters index)
                    (update (close-method) slow-counters index)
                    acc
             )
        ]
        [(list 'ensure average)
         (serve-aux (cdr requests)
                fast-counters
                (ensure-method fast-counters slow-counters average)
                acc
         )    
        ]
        
        [(list 'delay index minutes)
         (serve-aux (cdr requests)
                (update (delay-minutes minutes) fast-counters index)
                (update (delay-minutes minutes) slow-counters index)
                acc
         )
        ]
        
        [(list name items)
         (if (<= items ITEMS)
             (serve-aux (cdr requests)
                    (update (add-to-counter name items) fast-counters (car (min-tt (get-open-counters (append fast-counters slow-counters) null))))
                    (update (add-to-counter name items) slow-counters (car (min-tt (get-open-counters (append fast-counters slow-counters) null))))
                    acc
             )
             (serve-aux (cdr requests)
                    fast-counters
                    (update (add-to-counter name items) slow-counters (car (min-tt (get-open-counters slow-counters null))))
                    acc
             )
         )
        ]
        
        [x
         (serve-aux (cdr requests)
                (cdr (pass-time-for-all-counters x fast-counters acc))
                (cdr (pass-time-for-all-counters x slow-counters acc))
                (car (pass-time-for-all-counters x (append fast-counters slow-counters) acc))
         )
        ]
      )
  )
)