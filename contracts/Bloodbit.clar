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

(ft-mint? bloodbit u10000000 contract-owner)