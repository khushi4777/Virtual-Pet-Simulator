;; Virtual Pet Simulator Contract
;; Simple blockchain pet game

(define-non-fungible-token virtual-pet uint)

;; Error constants
(define-constant err-pet-not-found (err u101))
(define-constant err-not-owner (err u102))
(define-constant err-pet-dead (err u103))

;; Pet constants
(define-constant max-health u100)
(define-constant initial-value u1000)

;; Next pet ID
(define-data-var next-pet-id uint u1)

;; Pet data
(define-map pets uint {
  owner: principal,
  name: (string-ascii 32),
  health: uint,
  happiness: uint,
  value: uint,
  last-care: uint
})

;; Function 1: Create pet
(define-public (create-pet (pet-name (string-ascii 32)))
  (let ((pet-id (var-get next-pet-id)))
    (begin
      (try! (nft-mint? virtual-pet pet-id tx-sender))
      (map-set pets pet-id {
        owner: tx-sender,
        name: pet-name,
        health: max-health,
        happiness: max-health,
        value: initial-value,
        last-care: stacks-block-height
      })
      (var-set next-pet-id (+ pet-id u1))
      (ok pet-id))))

;; Function 2: Care for pet
(define-public (feed-pet (pet-id uint))
  (let ((pet-data (unwrap! (map-get? pets pet-id) err-pet-not-found)))
    (begin
      (asserts! (is-eq tx-sender (get owner pet-data)) err-not-owner)
      (asserts! (> (get health pet-data) u0) err-pet-dead)
      
      (let ((new-health (if (< (+ (get health pet-data) u20) max-health)
                          (+ (get health pet-data) u20)
                          max-health))
            (new-happiness (if (< (+ (get happiness pet-data) u15) max-health)
                            (+ (get happiness pet-data) u15)
                            max-health))
            (new-value (+ (get value pet-data) u100)))
        
        (map-set pets pet-id {
          owner: (get owner pet-data),
          name: (get name pet-data),
          health: new-health,
          happiness: new-happiness,
          value: new-value,
          last-care: stacks-block-height
        })
        (ok true)))))

;; Read functions
(define-read-only (get-pet (pet-id uint))
  (map-get? pets pet-id))

(define-read-only (get-pet-owner (pet-id uint))
  (nft-get-owner? virtual-pet pet-id))

(define-read-only (get-total-pets)
  (var-get next-pet-id))