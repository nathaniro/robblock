;; title: donor
;; version:
;; summary:donor-engagement
;; description:A smart contract for managing donor recognition and engagement

;; Constants
(define-constant contract-owner tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u1))
(define-constant ERR-DONOR-EXISTS (err u2))
(define-constant ERR-INVALID-BADGE (err u3))
(define-constant ERR-INSUFFICIENT-DONATIONS (err u4))

;; Data Maps
(define-map donors
    principal
    {
        total-donations: uint,
        donation-count: uint,
        badges: (list 10 uint),
        story: (string-utf8 500)
    }
)

(define-map badges
    uint
    {
        name: (string-utf8 50),
        description: (string-utf8 200),
        required-amount: uint,
        required-count: uint
    }
)

;; NFT Setup
(impl-trait 'SP2PABAF9FTAJYNFZH93XENAJ8FVY99RRM50D2JG9.nft-trait.nft-trait)

(define-non-fungible-token donor-badge uint)

;; Private Functions
(define-private (is-contract-owner)
    (is-eq tx-sender contract-owner)
)

(define-private (has-badge (donor-principal principal) (badge-id uint))
    (let (
        (donor-data (unwrap! (map-get? donors donor-principal) false))
        (badges-list (get badges donor-data))
    )
    (is-some (index-of badges-list badge-id)))
)

;; Public Functions
(define-public (register-donor)
    (let (
        (new-donor {
            total-donations: u0,
            donation-count: u0,
            badges: (list),
            story: u""
        })
    )
    (if (is-none (map-get? donors tx-sender))
        (ok (map-set donors tx-sender new-donor))
        ERR-DONOR-EXISTS
    ))
)

(define-public (make-donation (amount uint))
    (let (
        (donor (unwrap! (map-get? donors tx-sender) ERR-NOT-AUTHORIZED))
        (new-total (+ (get total-donations donor) amount))
        (new-count (+ (get donation-count donor) u1))
    )
    (ok (map-set donors tx-sender (merge donor {
        total-donations: new-total,
        donation-count: new-count
    }))))
)

(define-public (create-badge (badge-id uint) (name (string-utf8 50)) (description (string-utf8 200)) (required-amount uint) (required-count uint))
    (if (is-contract-owner)
        (ok (map-set badges badge-id {
            name: name,
            description: description,
            required-amount: required-amount,
            required-count: required-count
        }))
        ERR-NOT-AUTHORIZED
    )
)

(define-public (award-badge (donor-principal principal) (badge-id uint))
    (let (
        (donor (unwrap! (map-get? donors donor-principal) ERR-NOT-AUTHORIZED))
        (badge (unwrap! (map-get? badges badge-id) ERR-INVALID-BADGE))
    )
    (if (and
            (>= (get total-donations donor) (get required-amount badge))
            (>= (get donation-count donor) (get required-count badge))
        )
        (begin 
    (try! (nft-mint? donor-badge badge-id donor-principal))
    (ok (map-set donors donor-principal (merge donor {
        badges: (unwrap! (as-max-len? (append (get badges donor) badge-id) u10) ERR-NOT-AUTHORIZED)
    })))
)
        ERR-INSUFFICIENT-DONATIONS
    ))
)

(define-public (set-donor-story (story (string-utf8 500)))
    (let (
        (donor (unwrap! (map-get? donors tx-sender) ERR-NOT-AUTHORIZED))
    )
    (ok (map-set donors tx-sender (merge donor {
        story: story
    }))))
)

;; Read-only Functions
(define-read-only (get-donor-info (donor-principal principal))
    (map-get? donors donor-principal)
)

(define-read-only (get-badge-info (badge-id uint))
    (map-get? badges badge-id)
)

(define-read-only (get-donor-badges (donor-principal principal))
    (match (map-get? donors donor-principal)
        donor (ok (get badges donor))
        ERR-NOT-AUTHORIZED
    )
)