(define-constant contract-name "Bloodbit")
(define-constant contract-version "1.0.0")
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-token-owner (err u101))
(define-constant err-insufficient-balance (err u102))
(define-constant err-donor-not-verified (err u103))
(define-constant err-already-donated-today (err u104))
(define-constant err-invalid-amount (err u105))
(define-constant err-donor-already-verified (err u106))
(define-constant err-invalid-donation-type (err u107))
(define-constant err-invalid-blood-type (err u108))
(define-constant err-request-not-found (err u109))
(define-constant err-incompatible-blood-type (err u110))
(define-constant err-request-already-fulfilled (err u111))
(define-constant err-request-expired (err u112))
(define-constant err-self-request (err u113))
(define-constant err-insufficient-donation-history (err u114))

(define-fungible-token bloodbit)

(define-data-var token-name (string-ascii 32) "Bloodbit")
(define-data-var token-symbol (string-ascii 10) "BLOOD")
(define-data-var token-uri (optional (string-utf8 256)) none)
(define-data-var token-decimals uint u6)
(define-data-var total-donations uint u0)
(define-data-var donation-reward uint u1000000)

(define-map verified-donors principal bool)
(define-map donor-info principal {
    total-donations: uint,
    last-donation-block: uint,
    blood-type: (string-ascii 3),
    verification-date: uint
})
(define-map donation-records uint {
    donor: principal,
    donation-type: (string-ascii 10),
    block-height: uint,
    reward-amount: uint
})
(define-map daily-donations principal uint)

(define-map blood-requests uint {
    requester: principal,
    blood-type-needed: (string-ascii 3),
    quantity-needed: uint,
    urgency-level: (string-ascii 10),
    expiry-block: uint,
    reward-offered: uint,
    is-fulfilled: bool,
    created-block: uint
})

(define-map request-responses uint {
    request-id: uint,
    responder: principal,
    response-block: uint,
    donation-amount: uint,
    compatibility-score: uint
})

(define-map compatibility-matrix (string-ascii 3) (list 8 (string-ascii 3)))

(define-data-var next-request-id uint u1)
(define-data-var total-blood-requests uint u0)
(define-data-var compatibility-bonus uint u500000)

(define-public (transfer (amount uint) (from principal) (to principal) (memo (optional (buff 34))))
    (begin
        (asserts! (or (is-eq from tx-sender) (is-eq from contract-caller)) err-not-token-owner)
        (ft-transfer? bloodbit amount from to)
    )
)

(define-public (verify-donor (donor principal) (blood-type (string-ascii 3)))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (is-none (map-get? verified-donors donor)) err-donor-already-verified)
        (map-set verified-donors donor true)
        (map-set donor-info donor {
            total-donations: u0,
            last-donation-block: u0,
            blood-type: blood-type,
            verification-date: stacks-block-height
        })
        (ok true)
    )
)

(define-public (record-donation (donation-type (string-ascii 10)))
    (let (
        (donor tx-sender)
        (current-block stacks-block-height)
        (is-verified (default-to false (map-get? verified-donors donor)))
        (donor-data (unwrap! (map-get? donor-info donor) err-donor-not-verified))
        (last-donation (get last-donation-block donor-data))
        (daily-count (default-to u0 (map-get? daily-donations donor)))
        (reward-amount (calculate-reward donation-type))
        (donation-id (+ (var-get total-donations) u1))
    )
        (asserts! is-verified err-donor-not-verified)
        (asserts! (> current-block (+ last-donation u144)) err-already-donated-today)
        (asserts! (> reward-amount u0) err-invalid-donation-type)
        
        (try! (ft-mint? bloodbit reward-amount donor))
        
        (map-set donor-info donor (merge donor-data {
            total-donations: (+ (get total-donations donor-data) u1),
            last-donation-block: current-block
        }))
        
        (map-set donation-records donation-id {
            donor: donor,
            donation-type: donation-type,
            block-height: current-block,
            reward-amount: reward-amount
        })
        
        (map-set daily-donations donor (+ daily-count u1))
        (var-set total-donations donation-id)
        
        (ok reward-amount)
    )
)

(define-public (redeem-tokens (amount uint) (recipient (string-ascii 50)))
    (let (
        (sender tx-sender)
        (balance (ft-get-balance bloodbit sender))
    )
        (asserts! (>= balance amount) err-insufficient-balance)
        (asserts! (> amount u0) err-invalid-amount)
        (try! (ft-burn? bloodbit amount sender))
        (ok true)
    )
)

(define-public (set-donation-reward (new-reward uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set donation-reward new-reward)
        (ok true)
    )
)

(define-public (mint-bonus (recipient principal) (amount uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ft-mint? bloodbit amount recipient)
    )
)

(define-public (emergency-burn (amount uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ft-burn? bloodbit amount contract-owner)
    )
)

(define-read-only (get-name)
    (ok (var-get token-name))
)

(define-read-only (get-symbol)
    (ok (var-get token-symbol))
)

(define-read-only (get-decimals)
    (ok (var-get token-decimals))
)

(define-read-only (get-balance (who principal))
    (ok (ft-get-balance bloodbit who))
)

(define-read-only (get-total-supply)
    (ok (ft-get-supply bloodbit))
)

(define-read-only (get-token-uri)
    (ok (var-get token-uri))
)

(define-read-only (is-verified-donor (donor principal))
    (default-to false (map-get? verified-donors donor))
)

(define-read-only (get-donor-info (donor principal))
    (map-get? donor-info donor)
)

(define-read-only (get-donation-record (donation-id uint))
    (map-get? donation-records donation-id)
)

(define-read-only (get-total-donations)
    (var-get total-donations)
)

(define-read-only (get-donation-reward)
    (var-get donation-reward)
)

(define-read-only (can-donate-today (donor principal))
    (match (map-get? donor-info donor)
        donor-data (> stacks-block-height (+ (get last-donation-block donor-data) u144))
        false
    )
)

(define-read-only (get-daily-donation-count (donor principal))
    (default-to u0 (map-get? daily-donations donor))
)

(define-private (calculate-reward (donation-type (string-ascii 10)))
    (let ((base-reward (var-get donation-reward)))
        (if (is-eq donation-type "whole")
            base-reward
            (if (is-eq donation-type "plasma")
                (/ (* base-reward u75) u100)
                (if (is-eq donation-type "platelets")
                    (/ (* base-reward u125) u100)
                    (if (is-eq donation-type "double-red")
                        (/ (* base-reward u150) u100)
                        u0
                    )
                )
            )
        )
    )
)

(define-private (is-rare-blood-type (blood-type (string-ascii 3)))
    (or 
        (is-eq blood-type "AB-")
        (is-eq blood-type "O-")
        (is-eq blood-type "B-")
        (is-eq blood-type "A-")
    )
)

(define-public (create-blood-request (blood-type-needed (string-ascii 3)) (quantity-needed uint) (urgency-level (string-ascii 10)) (duration-blocks uint) (reward-offered uint))
    (let (
        (request-id (var-get next-request-id))
        (requester tx-sender)
        (current-block stacks-block-height)
        (expiry-block (+ current-block duration-blocks))
    )
        (asserts! (is-valid-blood-type blood-type-needed) err-invalid-blood-type)
        (asserts! (> quantity-needed u0) err-invalid-amount)
        (asserts! (> duration-blocks u0) err-invalid-amount)
        (asserts! (>= (ft-get-balance bloodbit requester) reward-offered) err-insufficient-balance)
        
        (try! (ft-transfer? bloodbit reward-offered requester (as-contract tx-sender)))
        
        (map-set blood-requests request-id {
            requester: requester,
            blood-type-needed: blood-type-needed,
            quantity-needed: quantity-needed,
            urgency-level: urgency-level,
            expiry-block: expiry-block,
            reward-offered: reward-offered,
            is-fulfilled: false,
            created-block: current-block
        })
        
        (var-set next-request-id (+ request-id u1))
        (var-set total-blood-requests (+ (var-get total-blood-requests) u1))
        
        (ok request-id)
    )
)

(define-public (respond-to-blood-request (request-id uint) (donation-amount uint))
    (let (
        (responder tx-sender)
        (request-data (unwrap! (map-get? blood-requests request-id) err-request-not-found))
        (responder-info (unwrap! (map-get? donor-info responder) err-donor-not-verified))
        (responder-blood-type (get blood-type responder-info))
        (needed-blood-type (get blood-type-needed request-data))
        (current-block stacks-block-height)
        (is-compatible (is-blood-compatible responder-blood-type needed-blood-type))
        (compatibility-score (calculate-compatibility-score responder-blood-type needed-blood-type))
        (total-reward (+ (get reward-offered request-data) (if is-compatible (var-get compatibility-bonus) u0)))
    )
        (asserts! (not (is-eq responder (get requester request-data))) err-self-request)
        (asserts! (not (get is-fulfilled request-data)) err-request-already-fulfilled)
        (asserts! (< current-block (get expiry-block request-data)) err-request-expired)
        (asserts! is-compatible err-incompatible-blood-type)
        (asserts! (>= (get total-donations responder-info) u1) err-insufficient-donation-history)
        (asserts! (> donation-amount u0) err-invalid-amount)
        
        (map-set blood-requests request-id (merge request-data {is-fulfilled: true}))
        
        (map-set request-responses request-id {
            request-id: request-id,
            responder: responder,
            response-block: current-block,
            donation-amount: donation-amount,
            compatibility-score: compatibility-score
        })
        
        (try! (as-contract (ft-transfer? bloodbit total-reward tx-sender responder)))
        
        (ok total-reward)
    )
)

(define-public (cancel-blood-request (request-id uint))
    (let (
        (request-data (unwrap! (map-get? blood-requests request-id) err-request-not-found))
        (requester (get requester request-data))
        (reward-amount (get reward-offered request-data))
    )
        (asserts! (is-eq tx-sender requester) err-not-token-owner)
        (asserts! (not (get is-fulfilled request-data)) err-request-already-fulfilled)
        
        (map-set blood-requests request-id (merge request-data {is-fulfilled: true}))
        
        (try! (as-contract (ft-transfer? bloodbit reward-amount tx-sender requester)))
        
        (ok true)
    )
)

(define-public (initialize-compatibility-matrix)
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        
        (map-set compatibility-matrix "O+" (list "O+" "A+" "B+" "AB+"))
        (map-set compatibility-matrix "O-" (list "O+" "O-" "A+" "A-" "B+" "B-" "AB+" "AB-"))
        (map-set compatibility-matrix "A+" (list "A+" "AB+"))
        (map-set compatibility-matrix "A-" (list "A+" "A-" "AB+" "AB-"))
        (map-set compatibility-matrix "B+" (list "B+" "AB+"))
        (map-set compatibility-matrix "B-" (list "B+" "B-" "AB+" "AB-"))
        (map-set compatibility-matrix "AB+" (list "AB+"))
        (map-set compatibility-matrix "AB-" (list "AB+" "AB-"))
        
        (ok true)
    )
)

(define-public (update-compatibility-bonus (new-bonus uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set compatibility-bonus new-bonus)
        (ok true)
    )
)

(define-read-only (get-blood-request (request-id uint))
    (map-get? blood-requests request-id)
)

(define-read-only (get-request-response (request-id uint))
    (map-get? request-responses request-id)
)

(define-read-only (get-total-blood-requests)
    (var-get total-blood-requests)
)

(define-read-only (get-compatibility-bonus)
    (var-get compatibility-bonus)
)

(define-read-only (find-compatible-donors (blood-type-needed (string-ascii 3)))
    (default-to (list) (map-get? compatibility-matrix blood-type-needed))
)

(define-read-only (is-request-active (request-id uint))
    (match (map-get? blood-requests request-id)
        request-data (and 
            (not (get is-fulfilled request-data))
            (< stacks-block-height (get expiry-block request-data))
        )
        false
    )
)

(define-private (is-blood-compatible (donor-type (string-ascii 3)) (recipient-type (string-ascii 3)))
    (let ((compatible-types (default-to (list) (map-get? compatibility-matrix donor-type))))
        (is-some (index-of compatible-types recipient-type))
    )
)

(define-private (calculate-compatibility-score (donor-type (string-ascii 3)) (recipient-type (string-ascii 3)))
    (if (is-eq donor-type recipient-type)
        u100
        (if (is-eq donor-type "O-")
            u90
            (if (and (is-eq donor-type "O+") (or (is-eq recipient-type "A+") (is-eq recipient-type "B+") (is-eq recipient-type "AB+")))
                u85
                (if (is-blood-compatible donor-type recipient-type)
                    u75
                    u0
                )
            )
        )
    )
)

(define-private (is-valid-blood-type (blood-type (string-ascii 3)))
    (or 
        (is-eq blood-type "O+")
        (is-eq blood-type "O-")
        (is-eq blood-type "A+")
        (is-eq blood-type "A-")
        (is-eq blood-type "B+")
        (is-eq blood-type "B-")
        (is-eq blood-type "AB+")
        (is-eq blood-type "AB-")
    )
)

(ft-mint? bloodbit u10000000 contract-owner)