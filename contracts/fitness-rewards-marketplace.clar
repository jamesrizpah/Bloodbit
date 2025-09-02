;; Fitness Achievement Rewards & Marketplace
;; Users earn FIT tokens for achievements and spend them in marketplace

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u400))
(define-constant ERR_INSUFFICIENT_TOKENS (err u401))
(define-constant ERR_ITEM_NOT_FOUND (err u402))
(define-constant ERR_ITEM_OUT_OF_STOCK (err u403))
(define-constant ERR_ALREADY_PURCHASED (err u404))
(define-constant ERR_INVALID_PRICE (err u405))

;; FIT token supply and distribution
(define-data-var total-fit-tokens uint u0)
(define-data-var tokens-distributed uint u0)
(define-data-var marketplace-revenue uint u0)

;; User FIT token balances
(define-map user-fit-tokens
  principal
  uint
)

;; Marketplace items (rewards, gear, premium features)
(define-map marketplace-items
  uint
  {
    name: (string-ascii 50),
    description: (string-ascii 150),
    category: (string-ascii 20),
    cost-tokens: uint,
    available-quantity: uint,
    total-sold: uint,
    is-active: bool,
    premium-feature: bool
  }
)

;; User purchases and owned items
(define-map user-purchases
  { user: principal, item-id: uint }
  {
    purchase-block: uint,
    tokens-spent: uint,
    quantity: uint,
    is-redeemed: bool
  }
)

;; Achievement token rewards mapping
(define-map achievement-rewards
  (string-ascii 30)
  uint
)

;; Premium memberships
(define-map premium-memberships
  principal
  {
    membership-type: (string-ascii 20),
    expiry-block: uint,
    benefits-used: uint,
    auto-renew: bool
  }
)

;; Initialize marketplace with default items
(define-public (initialize-marketplace)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    
    ;; Add fitness gear rewards
    (try! (add-marketplace-item u1 "Fitness Tracker Band" "Premium fitness tracking wristband" "gear" u50 u100 false))
    (try! (add-marketplace-item u2 "Protein Shake Voucher" "Voucher for premium protein shake" "nutrition" u25 u200 false))
    (try! (add-marketplace-item u3 "Gym Day Pass" "One-day access to premium gym facilities" "access" u75 u50 false))
    (try! (add-marketplace-item u4 "Personal Trainer Session" "1-hour session with certified trainer" "training" u150 u20 false))
    
    ;; Add premium features
    (try! (add-marketplace-item u5 "Premium Dashboard" "Advanced analytics and insights" "premium" u100 u1000 true))
    (try! (add-marketplace-item u6 "Priority Challenge Access" "Early access to exclusive challenges" "premium" u80 u500 true))
    
    ;; Set achievement reward rates
    (map-set achievement-rewards "milestone_complete" u10)
    (map-set achievement-rewards "challenge_win" u50)
    (map-set achievement-rewards "streak_week" u25)
    (map-set achievement-rewards "first_challenge" u20)
    (map-set achievement-rewards "team_challenge" u30)
    
    (ok true)
  )
)

;; Award FIT tokens for achievements
(define-public (award-fitness-tokens (user principal) (achievement (string-ascii 30)) (multiplier uint))
  (let
    (
      (base-reward (default-to u0 (map-get? achievement-rewards achievement)))
      (bonus-tokens (* base-reward multiplier))
      (current-balance (default-to u0 (map-get? user-fit-tokens user)))
      (new-balance (+ current-balance bonus-tokens))
    )
    (asserts! (> bonus-tokens u0) ERR_INVALID_PRICE)
    
    ;; Update user balance
    (map-set user-fit-tokens user new-balance)
    
    ;; Update global stats
    (var-set tokens-distributed (+ (var-get tokens-distributed) bonus-tokens))
    (var-set total-fit-tokens (+ (var-get total-fit-tokens) bonus-tokens))
    
    (ok bonus-tokens)
  )
)

;; Purchase marketplace item with FIT tokens
(define-public (purchase-item (item-id uint) (quantity uint))
  (let
    (
      (item (unwrap! (map-get? marketplace-items item-id) ERR_ITEM_NOT_FOUND))
      (user-balance (default-to u0 (map-get? user-fit-tokens tx-sender)))
      (total-cost (* (get cost-tokens item) quantity))
      (existing-purchase (map-get? user-purchases { user: tx-sender, item-id: item-id }))
    )
    (asserts! (get is-active item) ERR_ITEM_NOT_FOUND)
    (asserts! (>= (get available-quantity item) quantity) ERR_ITEM_OUT_OF_STOCK)
    (asserts! (>= user-balance total-cost) ERR_INSUFFICIENT_TOKENS)
    
    ;; For premium features, check if already purchased
    (if (get premium-feature item)
      (asserts! (is-none existing-purchase) ERR_ALREADY_PURCHASED)
      true)
    
    ;; Process purchase
    (map-set user-fit-tokens tx-sender (- user-balance total-cost))
    
    ;; Record purchase
    (map-set user-purchases { user: tx-sender, item-id: item-id }
      {
        purchase-block: stacks-block-height,
        tokens-spent: total-cost,
        quantity: (+ (default-to u0 (get quantity (default-to { purchase-block: u0, tokens-spent: u0, quantity: u0, is-redeemed: false } existing-purchase))) quantity),
        is-redeemed: false
      }
    )
    
    ;; Update item statistics
    (map-set marketplace-items item-id
      (merge item {
        available-quantity: (- (get available-quantity item) quantity),
        total-sold: (+ (get total-sold item) quantity)
      })
    )
    
    ;; Update marketplace revenue
    (var-set marketplace-revenue (+ (var-get marketplace-revenue) total-cost))
    
    (ok total-cost)
  )
)

;; Add new marketplace item - simplified version 
(define-public (add-marketplace-item 
  (item-id uint)
  (name (string-ascii 50))
  (description (string-ascii 150))
  (category (string-ascii 20))
  (cost-tokens uint)
  (available-quantity uint)
  (premium-feature bool))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (> cost-tokens u0) ERR_INVALID_PRICE)
    
    (map-set marketplace-items item-id
      {
        name: name,
        description: description,
        category: category,
        cost-tokens: cost-tokens,
        available-quantity: available-quantity,
        total-sold: u0,
        is-active: true,
        premium-feature: premium-feature
      }
    )
    (ok true)
  )
)

;; Transfer FIT tokens between users
(define-public (transfer-tokens (recipient principal) (amount uint))
  (let
    (
      (sender-balance (default-to u0 (map-get? user-fit-tokens tx-sender)))
      (recipient-balance (default-to u0 (map-get? user-fit-tokens recipient)))
    )
    (asserts! (>= sender-balance amount) ERR_INSUFFICIENT_TOKENS)
    (asserts! (> amount u0) ERR_INVALID_PRICE)
    
    (map-set user-fit-tokens tx-sender (- sender-balance amount))
    (map-set user-fit-tokens recipient (+ recipient-balance amount))
    
    (ok amount)
  )
)

;; Read-only functions
(define-read-only (get-user-token-balance (user principal))
  (default-to u0 (map-get? user-fit-tokens user))
)

(define-read-only (get-marketplace-item (item-id uint))
  (map-get? marketplace-items item-id)
)

(define-read-only (get-user-purchase (user principal) (item-id uint))
  (map-get? user-purchases { user: user, item-id: item-id })
)

(define-read-only (get-achievement-reward (achievement (string-ascii 30)))
  (map-get? achievement-rewards achievement)
)

(define-read-only (get-premium-membership (user principal))
  (map-get? premium-memberships user)
)

(define-read-only (get-total-fit-tokens)
  (var-get total-fit-tokens)
)

(define-read-only (get-marketplace-revenue)
  (var-get marketplace-revenue)
)

(define-read-only (can-afford-item (user principal) (item-id uint) (quantity uint))
  (let
    (
      (user-balance (default-to u0 (map-get? user-fit-tokens user)))
      (item (map-get? marketplace-items item-id))
    )
    (match item
      item-data (>= user-balance (* (get cost-tokens item-data) quantity))
      false)
  )
)
