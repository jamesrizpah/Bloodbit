;; SubscriptionGifting.clar - Gift subscription functionality
;; Allows users to purchase subscriptions as gifts for others

(define-constant contract-owner tx-sender)
(define-constant err-not-found (err u600))
(define-constant err-already-redeemed (err u601))
(define-constant err-gift-expired (err u602))
(define-constant err-invalid-recipient (err u603))
(define-constant err-self-gift (err u604))

(define-data-var next-gift-id uint u0)

(define-map subscription-gifts
  uint
  {
    gifter: principal,
    recipient: (optional principal),
    plan-id: uint,
    amount-paid: uint,
    message: (string-ascii 200),
    created-at: uint,
    expires-at: uint,
    redeemed: bool,
    redeemed-at: (optional uint)
  }
)

(define-read-only (get-gift (gift-id uint))
  (map-get? subscription-gifts gift-id)
)

(define-read-only (get-next-gift-id)
  (var-get next-gift-id)
)

;; Create a gift subscription for a specific recipient or as an open gift
(define-public (create-gift-subscription 
    (plan-id uint) 
    (recipient (optional principal)) 
    (message (string-ascii 200)) 
    (valid-days uint))
  (let (
    (gifter tx-sender)
    (gift-id (var-get next-gift-id))
    (plan-opt (contract-call? .SS-protocol get-subscription-plan plan-id))
    (current-time stacks-block-height)
    (expires-at (+ current-time (* valid-days u144))) ;; Convert days to blocks
  )
    (match plan-opt
      plan
        (let (
          (plan-price (get price plan))
          (fee-amount (/ (* plan-price (contract-call? .SS-protocol get-protocol-fee)) u10000))
          (total-cost (+ plan-price fee-amount))
        )
          ;; Prevent self-gifting when recipient is specified
          (match recipient
            recipient-addr (asserts! (not (is-eq gifter recipient-addr)) err-self-gift)
            true ;; Allow open gifts
          )
          
          (asserts! (get active plan) err-not-found)
          (asserts! (> valid-days u0) err-gift-expired)
          
          ;; Transfer payment to contract
          (try! (stx-transfer? total-cost gifter (as-contract tx-sender)))
          
          ;; Create gift record
          (map-set subscription-gifts gift-id
            {
              gifter: gifter,
              recipient: recipient,
              plan-id: plan-id,
              amount-paid: total-cost,
              message: message,
              created-at: current-time,
              expires-at: expires-at,
              redeemed: false,
              redeemed-at: none
            }
          )
          
          (var-set next-gift-id (+ gift-id u1))
          (ok gift-id)
        )
      (err err-not-found)
    )
  )
)

;; Redeem a gift subscription
(define-public (redeem-gift-subscription (gift-id uint))
  (let (
    (redeemer tx-sender)
    (gift (unwrap! (map-get? subscription-gifts gift-id) err-not-found))
    (current-time stacks-block-height)
  )
    ;; Check if gift is still valid
    (asserts! (not (get redeemed gift)) err-already-redeemed)
    (asserts! (< current-time (get expires-at gift)) err-gift-expired)
    
    ;; Check if recipient matches (if specified)
    (match (get recipient gift)
      intended-recipient 
        (asserts! (is-eq redeemer intended-recipient) err-invalid-recipient)
      true ;; Open gift - anyone can redeem
    )
    
    ;; Create subscription via SS-protocol
    (let (
      (subscription-result (try! (as-contract (contract-call? .SS-protocol subscribe (get plan-id gift)))))
    )
      ;; Mark gift as redeemed
      (map-set subscription-gifts gift-id
        (merge gift {
          redeemed: true,
          redeemed-at: (some current-time),
          recipient: (some redeemer) ;; Set recipient if it was an open gift
        })
      )
      
      ;; Transfer subscription ownership to redeemer
      (ok subscription-result)
    )
  )
)

;; Cancel an unredeemed gift and refund the gifter
(define-public (cancel-gift-subscription (gift-id uint))
  (let (
    (gift (unwrap! (map-get? subscription-gifts gift-id) err-not-found))
    (caller tx-sender)
  )
    (asserts! (is-eq caller (get gifter gift)) err-invalid-recipient)
    (asserts! (not (get redeemed gift)) err-already-redeemed)
    
    ;; Refund the gifter
    (try! (as-contract (stx-transfer? (get amount-paid gift) tx-sender (get gifter gift))))
    
    ;; Mark as redeemed to prevent double-spending
    (map-set subscription-gifts gift-id
      (merge gift { redeemed: true, redeemed-at: (some stacks-block-height) })
    )
    
    (ok true)
  )
)