
;; title: integration_with_tradition
;; version:
;; summary:
;; description:

;; traits
;;

;; token definitions
;;

;; constants
;;

;; data vars
;;

;; data maps
;;

;; public functions
;;

;; read only functions
;;

;; private functions
;;

;; charity-integration
;; Handles integration between traditional charities and crypto donations

(use-trait ft-trait .sip-010-trait-ft-standard.sip-010-trait)

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-invalid-charity (err u101))
(define-constant err-invalid-amount (err u102))
(define-constant err-unauthorized (err u103))

;; Data Variables
(define-map registered-charities 
    principal 
    {
        name: (string-ascii 50),
        description: (string-ascii 500),
        verified: bool,
        fiat-gateway: principal,
        total-donations: uint
    }
)

(define-map charity-admins principal bool)

;; Public Functions

;; Register a new charity
(define-public (register-charity 
    (charity-address principal)
    (charity-name (string-ascii 50))
    (charity-description (string-ascii 500))
    (fiat-gateway principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set registered-charities charity-address
            {
                name: charity-name,
                description: charity-description,
                verified: false,
                fiat-gateway: fiat-gateway,
                total-donations: u0
            }
        )
        (ok true)
    )
)

;; Verify a charity
(define-public (verify-charity (charity-address principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (match (map-get? registered-charities charity-address)
            charity
            (map-set registered-charities 
                charity-address
                (merge charity {verified: true})
            )
            err-invalid-charity
        )
    )
)

;; Make a donation using SIP-010 token
(define-public (donate 
    (charity-address principal)
    (amount uint)
    (token <ft-trait>))
    (let (
        (charity (unwrap! (map-get? registered-charities charity-address) err-invalid-charity))
    )
        (asserts! (get verified charity) err-unauthorized)
        (asserts! (> amount u0) err-invalid-amount)
        ;; Transfer tokens
        (try! (contract-call? token transfer amount tx-sender charity-address none))
        ;; Update donation total
        (map-set registered-charities 
            charity-address
            (merge charity {total-donations: (+ (get total-donations charity) amount)})
        )
        (ok true)
    )
)

;; Register fiat donation (called by fiat gateway)
(define-public (register-fiat-donation 
    (charity-address principal)
    (fiat-amount uint))
    (let (
        (charity (unwrap! (map-get? registered-charities charity-address) err-invalid-charity))
    )
        (asserts! (is-eq tx-sender (get fiat-gateway charity)) err-unauthorized)
        (asserts! (> fiat-amount u0) err-invalid-amount)
        ;; Update donation total
        (map-set registered-charities 
            charity-address
            (merge charity {total-donations: (+ (get total-donations charity) fiat-amount)})
        )
        (ok true)
    )
)

;; Read-only Functions

;; Get charity details
(define-read-only (get-charity-details (charity-address principal))
    (map-get? registered-charities charity-address)
)

;; Check if charity is verified
(define-read-only (is-charity-verified (charity-address principal))
    (match (map-get? registered-charities charity-address)
        charity (get verified charity)
        false
    )
)

;; Get total donations for a charity
(define-read-only (get-total-donations (charity-address principal))
    (match (map-get? registered-charities charity-address)
        charity (get total-donations charity)
        u0
    )
)