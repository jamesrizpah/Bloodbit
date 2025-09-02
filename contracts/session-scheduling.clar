;; SessionScheduling.clar - Time-based session scheduling for tutoring marketplace
;; Allows tutors to set availability and students to book specific time slots

(define-constant contract-owner tx-sender)
(define-constant err-slot-not-found (err u201))
(define-constant err-slot-unavailable (err u202))
(define-constant err-slot-booked (err u203))
(define-constant err-unauthorized (err u204))
(define-constant err-invalid-time (err u205))
(define-constant err-booking-not-found (err u206))
(define-constant err-cannot-cancel (err u207))
(define-constant err-invalid-duration (err u208))

;; Data variables
(define-data-var booking-counter uint u0)

;; Tutor availability slots
(define-map availability-slots {tutor: principal, start-time: uint}
  {
    duration-blocks: uint,
    hourly-rate: uint,
    available: bool,
    created-at: uint
  }
)

;; Student bookings for specific time slots
(define-map slot-bookings uint
  {
    student: principal,
    tutor: principal,
    start-time: uint,
    duration-blocks: uint,
    amount-paid: uint,
    status: (string-ascii 20),
    booked-at: uint
  }
)

;; Quick lookup for slot booking status
(define-map slot-booking-lookup {tutor: principal, start-time: uint} uint)

;; Read-only functions
(define-read-only (get-availability-slot (tutor principal) (start-time uint))
  (map-get? availability-slots {tutor: tutor, start-time: start-time})
)

(define-read-only (get-slot-booking (booking-id uint))
  (map-get? slot-bookings booking-id)
)

(define-read-only (is-slot-available (tutor principal) (start-time uint))
  (match (map-get? availability-slots {tutor: tutor, start-time: start-time})
    slot (and (get available slot) (is-none (map-get? slot-booking-lookup {tutor: tutor, start-time: start-time})))
    false
  )
)

;; Tutor sets availability for specific time slot
(define-public (set-availability 
    (start-time uint) 
    (duration-blocks uint) 
    (hourly-rate uint))
  (begin
    ;; Basic validations
    (asserts! (> start-time stacks-block-height) err-invalid-time)
    (asserts! (and (>= duration-blocks u10) (<= duration-blocks u144)) err-invalid-duration)
    (asserts! (> hourly-rate u0) err-invalid-time)
    
    ;; Create availability slot
    (ok (map-set availability-slots {tutor: tx-sender, start-time: start-time}
      {
        duration-blocks: duration-blocks,
        hourly-rate: hourly-rate,
        available: true,
        created-at: stacks-block-height
      }
    ))
  )
)

;; Student books a specific time slot
(define-public (book-time-slot (tutor principal) (start-time uint))
  (let 
    (
      (slot (unwrap! (map-get? availability-slots {tutor: tutor, start-time: start-time}) err-slot-not-found))
      (booking-id (+ (var-get booking-counter) u1))
    )
    ;; Validate booking conditions
    (asserts! (get available slot) err-slot-unavailable)
    (asserts! (is-none (map-get? slot-booking-lookup {tutor: tutor, start-time: start-time})) err-slot-booked)
    (asserts! (>= (stx-get-balance tx-sender) (get hourly-rate slot)) err-unauthorized)
    
    ;; Transfer payment to contract escrow
    (try! (stx-transfer? (get hourly-rate slot) tx-sender (as-contract tx-sender)))
    
    ;; Create booking record
    (map-set slot-bookings booking-id
      {
        student: tx-sender,
        tutor: tutor,
        start-time: start-time,
        duration-blocks: (get duration-blocks slot),
        amount-paid: (get hourly-rate slot),
        status: "booked",
        booked-at: stacks-block-height
      }
    )
    
    ;; Update booking lookup
    (map-set slot-booking-lookup {tutor: tutor, start-time: start-time} booking-id)
    
    ;; Update counter
    (var-set booking-counter booking-id)
    (ok booking-id)
  )
)

;; Cancel booking (student or tutor can cancel)
(define-public (cancel-booking (booking-id uint))
  (let ((booking (unwrap! (map-get? slot-bookings booking-id) err-booking-not-found)))
    (asserts! 
      (or 
        (is-eq (get student booking) tx-sender)
        (is-eq (get tutor booking) tx-sender)
      ) 
      err-unauthorized
    )
    (asserts! (not (is-eq (get status booking) "completed")) err-cannot-cancel)
    
    ;; Refund student
    (try! (as-contract (stx-transfer? (get amount-paid booking) tx-sender (get student booking))))
    
    ;; Remove booking lookup
    (map-delete slot-booking-lookup {tutor: (get tutor booking), start-time: (get start-time booking)})
    
    ;; Update booking status
    (ok (map-set slot-bookings booking-id (merge booking {status: "cancelled"})))
  )
)

;; Mark session as completed and release payment
(define-public (complete-scheduled-session (booking-id uint))
  (let ((booking (unwrap! (map-get? slot-bookings booking-id) err-booking-not-found)))
    (asserts! (is-eq (get tutor booking) tx-sender) err-unauthorized)
    (asserts! (is-eq (get status booking) "booked") err-cannot-cancel)
    
    ;; Calculate platform fee (5%)
    (let ((fee (/ (* (get amount-paid booking) u50) u1000)))
      ;; Pay tutor minus fee
      (try! (as-contract (stx-transfer? (- (get amount-paid booking) fee) tx-sender (get tutor booking))))
      ;; Pay platform fee
      (try! (as-contract (stx-transfer? fee tx-sender contract-owner)))
      
      ;; Update booking status
      (ok (map-set slot-bookings booking-id (merge booking {status: "completed"})))
    )
  )
)

;; Remove availability slot
(define-public (remove-availability (start-time uint))
  (let ((slot-key {tutor: tx-sender, start-time: start-time}))
    (asserts! (is-some (map-get? availability-slots slot-key)) err-slot-not-found)
    (asserts! (is-none (map-get? slot-booking-lookup {tutor: tx-sender, start-time: start-time})) err-slot-booked)
    
    (ok (map-delete availability-slots slot-key))
  )
)

;; Toggle availability status
(define-public (toggle-slot-availability (start-time uint))
  (let 
    (
      (slot-key {tutor: tx-sender, start-time: start-time})
      (slot (unwrap! (map-get? availability-slots slot-key) err-slot-not-found))
    )
    (ok (map-set availability-slots slot-key (merge slot {available: (not (get available slot))})))
  )
)