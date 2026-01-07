;; GleamTransform - Decentralized Creative Economy Platform
;; A smart contract for tracking creative content, attributions, and royalty distribution

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-unauthorized (err u103))
(define-constant err-invalid-percentage (err u104))
(define-constant err-insufficient-funds (err u105))

;; Platform fee (in basis points, 250 = 2.5%)
(define-constant platform-fee u250)

;; Data Variables
(define-data-var content-nonce uint u0)

;; Data Maps
;; Store content metadata and ownership
(define-map content-registry
  { content-id: uint }
  {
    creator: principal,
    content-hash: (buff 32),
    title: (string-ascii 256),
    created-at: uint,
    total-earned: uint,
    active: bool
  }
)

;; Track derivative relationships (parent -> child)
(define-map content-attributions
  { content-id: uint, parent-id: uint }
  {
    attribution-percentage: uint,
    created-at: uint
  }
)

;; Track creator's content IDs
(define-map creator-content-list
  { creator: principal, index: uint }
  { content-id: uint }
)

(define-map creator-content-count
  { creator: principal }
  { count: uint }
)

;; Revenue distribution tracking
(define-map pending-royalties
  { creator: principal }
  { amount: uint }
)

;; Public Functions

;; Register new creative content
(define-public (register-content (content-hash (buff 32)) (title (string-ascii 256)))
  (let
    (
      (content-id (+ (var-get content-nonce) u1))
      (creator tx-sender)
      (creator-count (default-to u0 (get count (map-get? creator-content-count { creator: creator }))))
    )
    ;; Update nonce
    (var-set content-nonce content-id)
    
    ;; Store content
    (map-set content-registry
      { content-id: content-id }
      {
        creator: creator,
        content-hash: content-hash,
        title: title,
        created-at: block-height,
        total-earned: u0,
        active: true
      }
    )
    
    ;; Update creator's content list
    (map-set creator-content-list
      { creator: creator, index: creator-count }
      { content-id: content-id }
    )
    
    (map-set creator-content-count
      { creator: creator }
      { count: (+ creator-count u1) }
    )
    
    (ok content-id)
  )
)

;; Register derivative work with attribution
(define-public (register-derivative 
    (content-hash (buff 32))
    (title (string-ascii 256))
    (parent-ids (list 10 uint))
    (attribution-percentages (list 10 uint)))
  (let
    (
      (total-attribution (fold + attribution-percentages u0))
      (new-content-id (unwrap! (register-content content-hash title) err-not-found))
    )
    ;; Validate attribution percentages don't exceed 100% (10000 basis points)
    (asserts! (<= total-attribution u10000) err-invalid-percentage)
    
    ;; Set attributions
    (map set-attribution parent-ids attribution-percentages)
    
    (ok new-content-id)
  )
)

;; Helper function to set attribution (used in fold)
(define-private (set-attribution (parent-id uint) (percentage uint))
  (let
    (
      (content-id (var-get content-nonce))
    )
    (map-set content-attributions
      { content-id: content-id, parent-id: parent-id }
      {
        attribution-percentage: percentage,
        created-at: block-height
      }
    )
    true
  )
)

;; Distribute royalty payment for content usage
(define-public (distribute-royalty (content-id uint) (amount uint))
  (let
    (
      (content (unwrap! (map-get? content-registry { content-id: content-id }) err-not-found))
      (platform-cut (/ (* amount platform-fee) u10000))
      (creator-amount (- amount platform-cut))
    )
    ;; Update content earnings
    (map-set content-registry
      { content-id: content-id }
      (merge content { total-earned: (+ (get total-earned content) creator-amount) })
    )
    
    ;; Add to pending royalties
    (add-pending-royalty (get creator content) creator-amount)
    
    ;; Note: In production, this would include STX transfer logic
    (ok true)
  )
)

;; Helper to add pending royalties
(define-private (add-pending-royalty (creator principal) (amount uint))
  (let
    (
      (current (default-to u0 (get amount (map-get? pending-royalties { creator: creator }))))
    )
    (map-set pending-royalties
      { creator: creator }
      { amount: (+ current amount) }
    )
  )
)

;; Withdraw accumulated royalties
(define-public (withdraw-royalties)
  (let
    (
      (creator tx-sender)
      (royalty-data (unwrap! (map-get? pending-royalties { creator: creator }) err-not-found))
      (amount (get amount royalty-data))
    )
    (asserts! (> amount u0) err-insufficient-funds)
    
    ;; Reset pending royalties
    (map-set pending-royalties
      { creator: creator }
      { amount: u0 }
    )
    
    ;; Note: Add STX transfer here in production: (try! (stx-transfer? amount (as-contract tx-sender) creator))
    
    (ok amount)
  )
)

;; Read-only functions

;; Get content details
(define-read-only (get-content (content-id uint))
  (ok (map-get? content-registry { content-id: content-id }))
)

;; Get attribution details
(define-read-only (get-attribution (content-id uint) (parent-id uint))
  (ok (map-get? content-attributions { content-id: content-id, parent-id: parent-id }))
)

;; Get creator's pending royalties
(define-read-only (get-pending-royalties (creator principal))
  (ok (default-to u0 (get amount (map-get? pending-royalties { creator: creator }))))
)

;; Get creator's content count
(define-read-only (get-creator-content-count (creator principal))
  (ok (default-to u0 (get count (map-get? creator-content-count { creator: creator }))))
)

;; Get content ID from creator's list
(define-read-only (get-creator-content-at-index (creator principal) (index uint))
  (ok (map-get? creator-content-list { creator: creator, index: index }))
)

;; Get total content registered
(define-read-only (get-total-content)
  (ok (var-get content-nonce))
)