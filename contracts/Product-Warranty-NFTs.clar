(define-non-fungible-token warranty-nft uint)

(define-data-var last-token-id uint u0)
(define-data-var contract-owner principal tx-sender)

(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-NFT-NOT-FOUND (err u101))
(define-constant ERR-ALREADY-EXISTS (err u102))
(define-constant ERR-INVALID-DURATION (err u103))
(define-constant ERR-WARRANTY-EXPIRED (err u104))
(define-constant ERR-CLAIM-NOT-FOUND (err u105))
(define-constant ERR-CLAIM-ALREADY-PROCESSED (err u106))
(define-constant ERR-INVALID-SELLER (err u107))
(define-constant ERR-INVALID-PRODUCT (err u108))

(define-map warranties
  uint
  {
    product-id: (string-ascii 64),
    seller: principal,
    buyer: principal,
    issue-block: uint,
    duration-blocks: uint,
    warranty-type: (string-ascii 32),
    product-info: (string-ascii 256),
    is-active: bool
  }
)

(define-map warranty-claims
  uint
  {
    warranty-id: uint,
    claimant: principal,
    claim-reason: (string-ascii 256),
    claim-block: uint,
    status: (string-ascii 16),
    resolution: (string-ascii 256)
  }
)

(define-map seller-profiles
  principal
  {
    business-name: (string-ascii 64),
    contact-info: (string-ascii 128),
    verified: bool,
    reputation-score: uint
  }
)

(define-map product-registry
  (string-ascii 64)
  {
    name: (string-ascii 64),
    category: (string-ascii 32),
    manufacturer: principal,
    base-warranty-blocks: uint
  }
)

(define-data-var claim-counter uint u0)

(define-read-only (get-last-token-id)
  (var-get last-token-id)
)

(define-read-only (get-token-uri (token-id uint))
  (ok none)
)

(define-read-only (get-owner (token-id uint))
  (ok (nft-get-owner? warranty-nft token-id))
)

(define-read-only (get-warranty (warranty-id uint))
  (map-get? warranties warranty-id)
)

(define-read-only (get-warranty-claim (claim-id uint))
  (map-get? warranty-claims claim-id)
)

(define-read-only (get-seller-profile (seller principal))
  (map-get? seller-profiles seller)
)

(define-read-only (get-product-info (product-id (string-ascii 64)))
  (map-get? product-registry product-id)
)

(define-read-only (is-warranty-valid (warranty-id uint))
  (match (map-get? warranties warranty-id)
    warranty
    (let ((current-block stacks-block-height)
          (expiry-block (+ (get issue-block warranty) (get duration-blocks warranty))))
      (and (get is-active warranty) (< current-block expiry-block)))
    false
  )
)

(define-read-only (get-warranty-expiry (warranty-id uint))
  (match (map-get? warranties warranty-id)
    warranty
    (ok (+ (get issue-block warranty) (get duration-blocks warranty)))
    ERR-NFT-NOT-FOUND
  )
)

(define-read-only (get-contract-owner)
  (var-get contract-owner)
)

(define-public (register-seller (business-name (string-ascii 64)) (contact-info (string-ascii 128)))
  (begin
    (map-set seller-profiles tx-sender {
      business-name: business-name,
      contact-info: contact-info,
      verified: false,
      reputation-score: u50
    })
    (ok true)
  )
)

(define-public (verify-seller (seller principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (match (map-get? seller-profiles seller)
      profile
      (begin
        (map-set seller-profiles seller (merge profile { verified: true }))
        (ok true)
      )
      ERR-INVALID-SELLER
    )
  )
)

(define-public (register-product (product-id (string-ascii 64)) (name (string-ascii 64)) (category (string-ascii 32)) (base-warranty-blocks uint))
  (begin
    (asserts! (is-none (map-get? product-registry product-id)) ERR-ALREADY-EXISTS)
    (map-set product-registry product-id {
      name: name,
      category: category,
      manufacturer: tx-sender,
      base-warranty-blocks: base-warranty-blocks
    })
    (ok true)
  )
)

(define-public (issue-warranty (product-id (string-ascii 64)) (buyer principal) (duration-blocks uint) (warranty-type (string-ascii 32)) (product-info (string-ascii 256)))
  (let ((token-id (+ (var-get last-token-id) u1)))
    (begin
      (asserts! (is-some (map-get? seller-profiles tx-sender)) ERR-INVALID-SELLER)
      (asserts! (is-some (map-get? product-registry product-id)) ERR-INVALID-PRODUCT)
      (asserts! (> duration-blocks u0) ERR-INVALID-DURATION)
      
      (try! (nft-mint? warranty-nft token-id buyer))
      
      (map-set warranties token-id {
        product-id: product-id,
        seller: tx-sender,
        buyer: buyer,
        issue-block: stacks-block-height,
        duration-blocks: duration-blocks,
        warranty-type: warranty-type,
        product-info: product-info,
        is-active: true
      })
      
      (var-set last-token-id token-id)
      (ok token-id)
    )
  )
)

(define-public (transfer (token-id uint) (sender principal) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender sender) ERR-NOT-AUTHORIZED)
    (match (nft-get-owner? warranty-nft token-id)
      owner
      (begin
        (asserts! (is-eq owner sender) ERR-NOT-AUTHORIZED)
        (try! (nft-transfer? warranty-nft token-id sender recipient))
        (match (map-get? warranties token-id)
          warranty
          (begin
            (map-set warranties token-id (merge warranty { buyer: recipient }))
            (ok true)
          )
          ERR-NFT-NOT-FOUND
        )
      )
      ERR-NFT-NOT-FOUND
    )
  )
)

(define-public (file-warranty-claim (warranty-id uint) (claim-reason (string-ascii 256)))
  (let ((claim-id (+ (var-get claim-counter) u1)))
    (begin
      (asserts! (is-some (map-get? warranties warranty-id)) ERR-NFT-NOT-FOUND)
      (asserts! (is-warranty-valid warranty-id) ERR-WARRANTY-EXPIRED)
      (match (nft-get-owner? warranty-nft warranty-id)
        owner
        (begin
          (asserts! (is-eq tx-sender owner) ERR-NOT-AUTHORIZED)
          (map-set warranty-claims claim-id {
            warranty-id: warranty-id,
            claimant: tx-sender,
            claim-reason: claim-reason,
            claim-block: stacks-block-height,
            status: "pending",
            resolution: ""
          })
          (var-set claim-counter claim-id)
          (ok claim-id)
        )
        ERR-NFT-NOT-FOUND
      )
    )
  )
)

(define-public (process-claim (claim-id uint) (status (string-ascii 16)) (resolution (string-ascii 256)))
  (match (map-get? warranty-claims claim-id)
    claim
    (let ((warranty-id (get warranty-id claim)))
      (match (map-get? warranties warranty-id)
        warranty
        (begin
          (asserts! (is-eq tx-sender (get seller warranty)) ERR-NOT-AUTHORIZED)
          (asserts! (is-eq (get status claim) "pending") ERR-CLAIM-ALREADY-PROCESSED)
          (map-set warranty-claims claim-id (merge claim {
            status: status,
            resolution: resolution
          }))
          (if (is-eq status "approved")
            (begin
              (match (map-get? seller-profiles (get seller warranty))
                profile
                (map-set seller-profiles (get seller warranty) (merge profile { reputation-score: (+ (get reputation-score profile) u5) }))
                true
              )
              (ok true)
            )
            (begin
              (match (map-get? seller-profiles (get seller warranty))
                profile
                (if (> (get reputation-score profile) u5)
                  (map-set seller-profiles (get seller warranty) (merge profile { reputation-score: (- (get reputation-score profile) u5) }))
                  (map-set seller-profiles (get seller warranty) (merge profile { reputation-score: u0 }))
                )
                true
              )
              (ok true)
            )
          )
        )
        ERR-NFT-NOT-FOUND
      )
    )
    ERR-CLAIM-NOT-FOUND
  )
)

(define-public (deactivate-warranty (warranty-id uint))
  (match (map-get? warranties warranty-id)
    warranty
    (begin
      (asserts! (is-eq tx-sender (get seller warranty)) ERR-NOT-AUTHORIZED)
      (map-set warranties warranty-id (merge warranty { is-active: false }))
      (ok true)
    )
    ERR-NFT-NOT-FOUND
  )
)

(define-public (update-reputation (seller principal) (score-change int))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (match (map-get? seller-profiles seller)
      profile
      (let ((current-score (get reputation-score profile))
            (new-score (if (> score-change 0)
                         (+ current-score (to-uint score-change))
                         (if (> current-score (to-uint (- 0 score-change)))
                           (- current-score (to-uint (- 0 score-change)))
                           u0))))
        (map-set seller-profiles seller (merge profile { reputation-score: new-score }))
        (ok true)
      )
      ERR-INVALID-SELLER
    )
  )
)

(define-public (set-contract-owner (new-owner principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (var-set contract-owner new-owner)
    (ok true)
  )
)
