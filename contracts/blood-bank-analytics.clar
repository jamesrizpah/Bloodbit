;; Blood Bank Network Analytics System
;; Provides analytics and optimization for blood bank inventory management

;; Constants
(define-constant ERR-NOT-AUTHORIZED (err u400))
(define-constant ERR-BANK-NOT-FOUND (err u401))
(define-constant ERR-INVALID-DATA (err u402))
(define-constant ERR-ANALYTICS-DISABLED (err u403))
(define-constant ERR-INSUFFICIENT-DATA (err u404))
(define-constant ERR-PREDICTION-FAILED (err u405))

;; Analytics constants
(define-constant ANALYSIS-DAILY u1)
(define-constant ANALYSIS-WEEKLY u2)
(define-constant ANALYSIS-MONTHLY u3)

;; Data variables
(define-data-var analytics-enabled bool true)
(define-data-var analysis-count uint u0)
(define-data-var prediction-accuracy uint u85)
(define-data-var contract-owner principal tx-sender)

;; Maps
(define-map bank-analytics
  { bank: principal, analysis-date: uint }
  {
    total-inventory: uint,
    inventory-by-type: (list 8 { blood-type: (string-ascii 3), units: uint }),
    utilization-rate: uint,
    waste-percentage: uint,
    efficiency-score: uint,
    demand-forecast: uint
  }
)

(define-map demand-predictions
  { bank: principal, blood-type: (string-ascii 3), prediction-date: uint }
  {
    predicted-demand: uint,
    confidence-level: uint,
    seasonal-factor: uint,
    trend-direction: bool, ;; true = increasing, false = decreasing
    last-updated: uint
  }
)

(define-map network-metrics
  { analysis-period: uint }
  {
    total-banks: uint,
    total-inventory: uint,
    average-utilization: uint,
    critical-shortages: uint,
    surplus-locations: uint,
    transfer-efficiency: uint
  }
)

(define-map supply-optimization
  { source-bank: principal, target-bank: principal }
  {
    recommended-transfer: uint,
    blood-type: (string-ascii 3),
    priority-score: uint,
    cost-benefit: uint,
    optimal-timing: uint
  }
)

(define-map regional-blood-trends
  { region-code: (string-utf8 10), time-period: uint }
  {
    donation-trend: bool,
    demand-trend: bool,
    seasonal-index: uint,
    shortage-risk: uint,
    surplus-probability: uint
  }
)

;; Public functions

;; Generate comprehensive analytics for a blood bank
(define-public (generate-bank-analytics (target-bank principal))
  (let
    ((analysis-id (+ (var-get analysis-count) u1))
     (bank-data (unwrap! (contract-call? .Bloodbit get-bank-info target-bank) ERR-BANK-NOT-FOUND)))
    
    (asserts! (var-get analytics-enabled) ERR-ANALYTICS-DISABLED)
    (asserts! (get is-active bank-data) ERR-BANK-NOT-FOUND)
    
    ;; Calculate analytics metrics
    (let
      ((total-inv (calculate-total-inventory target-bank))
       (utilization (calculate-utilization-rate target-bank))
       (waste-rate (calculate-waste-percentage target-bank))
       (efficiency (calculate-efficiency-score target-bank utilization waste-rate))
       (forecast (generate-demand-forecast target-bank)))
      
      (map-set bank-analytics
        { bank: target-bank, analysis-date: stacks-block-height }
        {
          total-inventory: total-inv,
          inventory-by-type: (get-inventory-breakdown target-bank),
          utilization-rate: utilization,
          waste-percentage: waste-rate,
          efficiency-score: efficiency,
          demand-forecast: forecast
        }
      )
      
      (var-set analysis-count analysis-id)
      (ok analysis-id)
    )
  )
)

;; Generate network-wide optimization recommendations
(define-public (optimize-network-transfers)
  (let
    ((optimization-period stacks-block-height))
    
    (asserts! (var-get analytics-enabled) ERR-ANALYTICS-DISABLED)
    
    ;; Calculate network metrics
    (let
      ((network-stats (calculate-network-statistics))
       (shortage-banks (identify-shortage-banks))
       (surplus-banks (identify-surplus-banks)))
      
      (map-set network-metrics
        { analysis-period: optimization-period }
        network-stats
      )
      
      ;; Generate transfer recommendations
      (generate-transfer-recommendations shortage-banks surplus-banks)
      (ok optimization-period)
    )
  )
)

;; Submit demand prediction for specific blood type
(define-public (submit-demand-prediction 
    (target-bank principal)
    (blood-type (string-ascii 3))
    (predicted-demand uint)
    (confidence-level uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (asserts! (and (>= confidence-level u50) (<= confidence-level u100)) ERR-INVALID-DATA)
    (asserts! (> predicted-demand u0) ERR-INVALID-DATA)
    
    (map-set demand-predictions
      { bank: target-bank, blood-type: blood-type, prediction-date: stacks-block-height }
      {
        predicted-demand: predicted-demand,
        confidence-level: confidence-level,
        seasonal-factor: (calculate-seasonal-factor),
        trend-direction: (analyze-trend-direction target-bank blood-type),
        last-updated: stacks-block-height
      }
    )
    
    (ok true)
  )
)

;; Toggle analytics system
(define-public (toggle-analytics (enabled bool))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (var-set analytics-enabled enabled)
    (ok true)
  )
)

;; Private helper functions

(define-private (calculate-total-inventory (bank principal))
  (get total (fold sum-inventory-by-type 
                   (list "O+" "O-" "A+" "A-" "B+" "B-" "AB+" "AB-") 
                   { bank: bank, total: u0 })))

(define-private (sum-inventory-by-type 
    (blood-type (string-ascii 3)) 
    (acc { bank: principal, total: uint }))
  (let
    ((inventory (contract-call? .Bloodbit get-bank-inventory (get bank acc) blood-type)))
    (match inventory
      inv-data { bank: (get bank acc), total: (+ (get total acc) (get total-units inv-data)) }
      { bank: (get bank acc), total: (get total acc) })))

(define-private (calculate-utilization-rate (bank principal))
  (let
    ((total-inv (calculate-total-inventory bank))
     (reserved-inv (calculate-reserved-inventory bank)))
    (if (> total-inv u0)
      (/ (* reserved-inv u100) total-inv)
      u0)))

(define-private (calculate-reserved-inventory (bank principal))
  (get total (fold sum-reserved-by-type 
                   (list "O+" "O-" "A+" "A-" "B+" "B-" "AB+" "AB-") 
                   { bank: bank, total: u0 })))

(define-private (sum-reserved-by-type 
    (blood-type (string-ascii 3)) 
    (acc { bank: principal, total: uint }))
  (let
    ((inventory (contract-call? .Bloodbit get-bank-inventory (get bank acc) blood-type)))
    (match inventory
      inv-data { bank: (get bank acc), total: (+ (get total acc) (get reserved-units inv-data)) }
      { bank: (get bank acc), total: (get total acc) })))

(define-private (calculate-waste-percentage (bank principal))
  ;; Simplified waste calculation based on expired inventory
  (let
    ((total-inv (calculate-total-inventory bank))
     (expired-est (/ total-inv u20))) ;; Estimate 5% waste rate
    (if (> total-inv u0)
      (/ (* expired-est u100) total-inv)
      u0)))

(define-private (calculate-efficiency-score (bank principal) (utilization uint) (waste uint))
  (if (> waste u0)
    (/ (* utilization (- u100 waste)) u100)
    utilization))

(define-private (generate-demand-forecast (bank principal))
  ;; Simple forecast based on historical trends
  (let
    ((current-inv (calculate-total-inventory bank))
     (seasonal-adj (calculate-seasonal-factor)))
    (/ (* current-inv seasonal-adj) u100)))

(define-private (get-inventory-breakdown (bank principal))
  (list
    { blood-type: "O+", units: (get-bank-units bank "O+") }
    { blood-type: "O-", units: (get-bank-units bank "O-") }
    { blood-type: "A+", units: (get-bank-units bank "A+") }
    { blood-type: "A-", units: (get-bank-units bank "A-") }
    { blood-type: "B+", units: (get-bank-units bank "B+") }
    { blood-type: "B-", units: (get-bank-units bank "B-") }
    { blood-type: "AB+", units: (get-bank-units bank "AB+") }
    { blood-type: "AB-", units: (get-bank-units bank "AB-") }))

(define-private (get-bank-units (bank principal) (blood-type (string-ascii 3)))
  (match (contract-call? .Bloodbit get-bank-inventory bank blood-type)
    inv-data (get total-units inv-data)
    u0))

(define-private (calculate-seasonal-factor)
  (let
    ((month-factor (mod stacks-block-height u4320))) ;; ~30 days
    (if (< month-factor u2160) u110 u90))) ;; Higher demand in first half of cycle

(define-private (analyze-trend-direction (bank principal) (blood-type (string-ascii 3)))
  (let
    ((current-units (get-bank-units bank blood-type)))
    (> current-units u10))) ;; Simple trend based on current levels

(define-private (calculate-network-statistics)
  {
    total-banks: u5, ;; Simplified for demo
    total-inventory: u1000,
    average-utilization: u75,
    critical-shortages: u2,
    surplus-locations: u3,
    transfer-efficiency: u80
  })

(define-private (identify-shortage-banks)
  (list 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM 'ST2CY5V39NHDPWSXMW9QDT3HC3GD6Q6XX4CFRK9AG))

(define-private (identify-surplus-banks)
  (list 'ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5 'ST2JHG361ZXG51QTKY2NQCVBPPRRE2KZB1HR05NNC))

(define-private (generate-transfer-recommendations 
    (shortage-banks (list 2 principal)) 
    (surplus-banks (list 2 principal)))
  true) ;; Simplified implementation - just return true

;; Read-only functions
(define-read-only (get-bank-analytics (bank principal) (analysis-date uint))
  (map-get? bank-analytics { bank: bank, analysis-date: analysis-date }))

(define-read-only (get-demand-prediction (bank principal) (blood-type (string-ascii 3)) (prediction-date uint))
  (map-get? demand-predictions { bank: bank, blood-type: blood-type, prediction-date: prediction-date }))

(define-read-only (get-network-metrics (analysis-period uint))
  (map-get? network-metrics { analysis-period: analysis-period }))

(define-read-only (get-optimization-recommendation (source-bank principal) (target-bank principal))
  (map-get? supply-optimization { source-bank: source-bank, target-bank: target-bank }))

(define-read-only (is-analytics-enabled)
  (var-get analytics-enabled))

(define-read-only (get-prediction-accuracy)
  (var-get prediction-accuracy))
