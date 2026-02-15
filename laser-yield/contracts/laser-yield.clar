;; LaserYield - Simplified DeFi Yield Optimization Protocol
;; A Clarity smart contract for automated yield farming on Stacks

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-insufficient-balance (err u101))
(define-constant err-not-staked (err u102))
(define-constant err-already-staked (err u103))
(define-constant err-invalid-amount (err u104))
(define-constant err-paused (err u105))

;; Data Variables
(define-data-var protocol-paused bool false)
(define-data-var total-staked uint u0)
(define-data-var total-yield-distributed uint u0)
(define-data-var performance-fee-rate uint u250) ;; 2.5% = 250 basis points

;; Data Maps
(define-map user-stakes principal uint)
(define-map user-yield-balance principal uint)
(define-map stake-timestamps principal uint)
(define-map user-risk-level principal uint) ;; 1=low, 2=medium, 3=high

;; Read-only functions
(define-read-only (get-user-stake (user principal))
  (default-to u0 (map-get? user-stakes user))
)

(define-read-only (get-user-yield (user principal))
  (default-to u0 (map-get? user-yield-balance user))
)

(define-read-only (get-total-staked)
  (var-get total-staked)
)

(define-read-only (get-stake-timestamp (user principal))
  (map-get? stake-timestamps user)
)

(define-read-only (get-user-risk-level (user principal))
  (default-to u1 (map-get? user-risk-level user))
)

(define-read-only (is-paused)
  (var-get protocol-paused)
)

;; Private functions
(define-private (calculate-yield (amount uint) (duration uint))
  ;; Simple yield calculation: 10% APY
  ;; yield = amount * 10% * (duration / 525600 blocks per year)
  (/ (* (* amount u10) duration) u5256000)
)

(define-private (apply-performance-fee (yield uint))
  (let ((fee (/ (* yield (var-get performance-fee-rate)) u10000)))
    (- yield fee)
  )
)

;; Public functions

;; Stake STX tokens
(define-public (stake (amount uint))
  (let (
    (sender tx-sender)
    (current-stake (get-user-stake sender))
  )
    (asserts! (not (var-get protocol-paused)) err-paused)
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (is-eq current-stake u0) err-already-staked)
    
    ;; Transfer STX to contract
    (try! (stx-transfer? amount sender (as-contract tx-sender)))
    
    ;; Update state
    (map-set user-stakes sender amount)
    (map-set stake-timestamps sender block-height)
    (var-set total-staked (+ (var-get total-staked) amount))
    
    (ok true)
  )
)

;; Unstake and claim yield
(define-public (unstake)
  (let (
    (sender tx-sender)
    (staked-amount (get-user-stake sender))
    (stake-start (unwrap! (get-stake-timestamp sender) err-not-staked))
    (duration (- block-height stake-start))
    (raw-yield (calculate-yield staked-amount duration))
    (net-yield (apply-performance-fee raw-yield))
  )
    (asserts! (not (var-get protocol-paused)) err-paused)
    (asserts! (> staked-amount u0) err-not-staked)
    
    ;; Transfer principal + yield back to user
    (try! (as-contract (stx-transfer? (+ staked-amount net-yield) tx-sender sender)))
    
    ;; Update state
    (map-delete user-stakes sender)
    (map-delete stake-timestamps sender)
    (var-set total-staked (- (var-get total-staked) staked-amount))
    (var-set total-yield-distributed (+ (var-get total-yield-distributed) net-yield))
    
    (ok net-yield)
  )
)

;; Claim yield without unstaking
(define-public (claim-yield)
  (let (
    (sender tx-sender)
    (staked-amount (get-user-stake sender))
    (stake-start (unwrap! (get-stake-timestamp sender) err-not-staked))
    (duration (- block-height stake-start))
    (raw-yield (calculate-yield staked-amount duration))
    (net-yield (apply-performance-fee raw-yield))
  )
    (asserts! (not (var-get protocol-paused)) err-paused)
    (asserts! (> staked-amount u0) err-not-staked)
    (asserts! (> net-yield u0) err-invalid-amount)
    
    ;; Transfer yield to user
    (try! (as-contract (stx-transfer? net-yield tx-sender sender)))
    
    ;; Reset stake timestamp
    (map-set stake-timestamps sender block-height)
    (var-set total-yield-distributed (+ (var-get total-yield-distributed) net-yield))
    
    (ok net-yield)
  )
)

;; Set user risk preference (1=low, 2=medium, 3=high)
(define-public (set-risk-level (level uint))
  (begin
    (asserts! (and (>= level u1) (<= level u3)) err-invalid-amount)
    (map-set user-risk-level tx-sender level)
    (ok true)
  )
)

;; Admin functions

;; Pause protocol in emergency
(define-public (pause-protocol)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set protocol-paused true)
    (ok true)
  )
)

;; Resume protocol
(define-public (resume-protocol)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set protocol-paused false)
    (ok true)
  )
)

;; Update performance fee rate (in basis points, e.g., 250 = 2.5%)
(define-public (set-performance-fee (new-rate uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-rate u1000) err-invalid-amount) ;; Max 10%
    (var-set performance-fee-rate new-rate)
    (ok true)
  )
)