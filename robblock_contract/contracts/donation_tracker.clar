
;; title: donation_tracker
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

;; Define the contract
(define-data-var next-donation-id uint u0)

;; Define a map to store donation details
(define-map donations
  { donation-id: uint }
  {
    donor: principal,
    amount: uint,
    allocated: bool,
    allocation-details: (optional {project: (string-ascii 64), amount: uint})
  }
)

;; Define a map to track donor's donations
(define-map donor-donations
  { donor: principal }
  { donation-ids: (list 50 uint) }
)

;; Function to make a donation
(define-public (donate (amount uint))
  (let
    (
      (donation-id (var-get next-donation-id))
      (donor tx-sender)
    )
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (map-set donations
      { donation-id: donation-id }
      {
        donor: donor,
        amount: amount,
        allocated: false,
        allocation-details: none
      }
    )
    (map-set donor-donations
      { donor: donor }
      {
        donation-ids: (unwrap-panic (as-max-len? 
          (append (default-to (list) (get donation-ids (map-get? donor-donations { donor: donor }))) donation-id)
          u50))
      }
    )
    (var-set next-donation-id (+ donation-id u1))
    (ok donation-id)
  )
)

;; Function to allocate funds
(define-public (allocate-funds (donation-id uint) (project (string-ascii 64)) (allocation-amount uint))
  (let
    (
      (donation (unwrap-panic (map-get? donations { donation-id: donation-id })))
    )
    (asserts! (is-eq (get allocated donation) false) (err u403)) ;; Ensure donation hasn't been allocated yet
    (asserts! (<= allocation-amount (get amount donation)) (err u400)) ;; Ensure allocation amount doesn't exceed donation amount
    (map-set donations
      { donation-id: donation-id }
      (merge donation
        {
          allocated: true,
          allocation-details: (some {project: project, amount: allocation-amount})
        }
      )
    )
    (ok true)
  )
)

;; Read-only function to get donation details
(define-read-only (get-donation-details (donation-id uint))
  (map-get? donations { donation-id: donation-id })
)

;; Read-only function to get a donor's donations
(define-read-only (get-donor-donations (donor principal))
  (map-get? donor-donations { donor: donor })
)