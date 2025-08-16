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


(define-constant ERR-INSUFFICIENT-PAYMENT (err u109))
(define-constant ERR-EXTENSION-NOT-AVAILABLE (err u110))
(define-constant ERR-UPGRADE-NOT-AVAILABLE (err u111))

(define-constant ERR-INSUFFICIENT-POOL-FUNDS (err u109))
(define-constant ERR-POOL-NOT-FOUND (err u110))
(define-constant ERR-INVALID-CONTRIBUTION (err u111))
(define-constant ERR-WITHDRAWAL-DENIED (err u112))

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

(define-map warranty-upgrades
  {seller: principal, product-id: (string-ascii 64)}
  {
    extension-price-per-block: uint,
    premium-upgrade-price: uint,
    premium-features: (string-ascii 128),
    max-extensions: uint
  }
)

(define-map warranty-extensions
  uint
  {
    original-duration: uint,
    total-extensions: uint,
    extension-history: (list 5 uint),
    upgraded-type: (string-ascii 32),
    upgrade-block: uint
  }
)

(define-read-only (get-upgrade-options (seller principal) (product-id (string-ascii 64)))
  (map-get? warranty-upgrades {seller: seller, product-id: product-id})
)

(define-read-only (get-extension-history (warranty-id uint))
  (map-get? warranty-extensions warranty-id)
)

(define-public (set-upgrade-options (product-id (string-ascii 64)) (extension-price uint) (premium-price uint) (premium-features (string-ascii 128)) (max-extensions uint))
  (begin
    (asserts! (is-some (map-get? seller-profiles tx-sender)) ERR-INVALID-SELLER)
    (asserts! (is-some (map-get? product-registry product-id)) ERR-INVALID-PRODUCT)
    (map-set warranty-upgrades {seller: tx-sender, product-id: product-id} {
      extension-price-per-block: extension-price,
      premium-upgrade-price: premium-price,
      premium-features: premium-features,
      max-extensions: max-extensions
    })
    (ok true)
  )
)

(define-public (extend-warranty (warranty-id uint) (additional-blocks uint))
  (match (map-get? warranties warranty-id)
    warranty
    (match (nft-get-owner? warranty-nft warranty-id)
      owner
      (begin
        (asserts! (is-eq tx-sender owner) ERR-NOT-AUTHORIZED)
        (asserts! (is-warranty-valid warranty-id) ERR-WARRANTY-EXPIRED)
        (match (map-get? warranty-upgrades {seller: (get seller warranty), product-id: (get product-id warranty)})
          upgrade-options
          (let ((extension-cost (* (get extension-price-per-block upgrade-options) additional-blocks))
                (current-extensions (default-to {original-duration: (get duration-blocks warranty), total-extensions: u0, extension-history: (list), upgraded-type: "", upgrade-block: u0} (map-get? warranty-extensions warranty-id))))
            (begin
              (asserts! (< (get total-extensions current-extensions) (get max-extensions upgrade-options)) ERR-EXTENSION-NOT-AVAILABLE)
              (try! (stx-transfer? extension-cost tx-sender (get seller warranty)))
              (map-set warranties warranty-id (merge warranty {
                duration-blocks: (+ (get duration-blocks warranty) additional-blocks)
              }))
              (map-set warranty-extensions warranty-id (merge current-extensions {
                total-extensions: (+ (get total-extensions current-extensions) u1),
                extension-history: (unwrap-panic (as-max-len? (append (get extension-history current-extensions) additional-blocks) u5))
              }))
              (ok true)
            )
          )
          ERR-EXTENSION-NOT-AVAILABLE
        )
      )
      ERR-NFT-NOT-FOUND
    )
    ERR-NFT-NOT-FOUND
  )
)

(define-public (upgrade-warranty (warranty-id uint) (new-type (string-ascii 32)))
  (match (map-get? warranties warranty-id)
    warranty
    (match (nft-get-owner? warranty-nft warranty-id)
      owner
      (begin
        (asserts! (is-eq tx-sender owner) ERR-NOT-AUTHORIZED)
        (asserts! (is-warranty-valid warranty-id) ERR-WARRANTY-EXPIRED)
        (match (map-get? warranty-upgrades {seller: (get seller warranty), product-id: (get product-id warranty)})
          upgrade-options
          (let ((upgrade-cost (get premium-upgrade-price upgrade-options))
                (current-extensions (default-to {original-duration: (get duration-blocks warranty), total-extensions: u0, extension-history: (list), upgraded-type: "", upgrade-block: u0} (map-get? warranty-extensions warranty-id))))
            (begin
              (asserts! (is-eq (get upgraded-type current-extensions) "") ERR-UPGRADE-NOT-AVAILABLE)
              (try! (stx-transfer? upgrade-cost tx-sender (get seller warranty)))
              (map-set warranties warranty-id (merge warranty {warranty-type: new-type}))
              (map-set warranty-extensions warranty-id (merge current-extensions {
                upgraded-type: new-type,
                upgrade-block: stacks-block-height
              }))
              (ok true)
            )
          )
          ERR-UPGRADE-NOT-AVAILABLE
        )
      )
      ERR-NFT-NOT-FOUND
    )
    ERR-NFT-NOT-FOUND
  )
)


(define-map insurance-pools
  (string-ascii 32)
  {
    total-funds: uint,
    total-contributors: uint,
    claims-paid: uint,
    pool-active: bool
  }
)

(define-map pool-contributions
  {contributor: principal, category: (string-ascii 32)}
  {
    amount: uint,
    join-block: uint,
    rewards-earned: uint,
    active: bool
  }
)

(define-read-only (get-pool-status (category (string-ascii 32)))
  (map-get? insurance-pools category)
)

(define-read-only (get-contribution-info (contributor principal) (category (string-ascii 32)))
  (map-get? pool-contributions {contributor: contributor, category: category})
)

(define-read-only (calculate-pool-share (contributor principal) (category (string-ascii 32)))
  (match (map-get? pool-contributions {contributor: contributor, category: category})
    contribution
    (match (map-get? insurance-pools category)
      pool
      (if (> (get total-funds pool) u0)
        (ok (/ (* (get amount contribution) u10000) (get total-funds pool)))
        (ok u0)
      )
      (ok u0)
    )
    (ok u0)
  )
)

(define-public (contribute-to-pool (category (string-ascii 32)) (amount uint))
  (let ((existing-contribution (map-get? pool-contributions {contributor: tx-sender, category: category}))
        (existing-pool (default-to {total-funds: u0, total-contributors: u0, claims-paid: u0, pool-active: true} (map-get? insurance-pools category))))
    (begin
      (asserts! (> amount u0) ERR-INVALID-CONTRIBUTION)
      (asserts! (get pool-active existing-pool) ERR-POOL-NOT-FOUND)
      (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
      (match existing-contribution
        contribution
        (map-set pool-contributions {contributor: tx-sender, category: category} (merge contribution {
          amount: (+ (get amount contribution) amount)
        }))
        (map-set pool-contributions {contributor: tx-sender, category: category} {
          amount: amount,
          join-block: stacks-block-height,
          rewards-earned: u0,
          active: true
        })
      )
      (map-set insurance-pools category (merge existing-pool {
        total-funds: (+ (get total-funds existing-pool) amount),
        total-contributors: (if (is-none existing-contribution) (+ (get total-contributors existing-pool) u1) (get total-contributors existing-pool))
      }))
      (ok true)
    )
  )
)

(define-public (process-insured-claim (claim-id uint) (compensation-amount uint))
  (match (map-get? warranty-claims claim-id)
    claim
    (let ((warranty-id (get warranty-id claim)))
      (match (map-get? warranties warranty-id)
        warranty
        (match (map-get? product-registry (get product-id warranty))
          product
          (let ((category (get category product))
                (pool (unwrap! (map-get? insurance-pools category) ERR-POOL-NOT-FOUND)))
            (begin
              (asserts! (is-eq tx-sender (get seller warranty)) ERR-NOT-AUTHORIZED)
              (asserts! (is-eq (get status claim) "pending") ERR-CLAIM-ALREADY-PROCESSED)
              (asserts! (>= (get total-funds pool) compensation-amount) ERR-INSUFFICIENT-POOL-FUNDS)
              (try! (as-contract (stx-transfer? compensation-amount tx-sender (get claimant claim))))
              (map-set warranty-claims claim-id (merge claim {
                status: "approved",
                resolution: "Compensated from insurance pool"
              }))
              (map-set insurance-pools category (merge pool {
                total-funds: (- (get total-funds pool) compensation-amount),
                claims-paid: (+ (get claims-paid pool) u1)
              }))
              (ok true)
            )
          )
          ERR-INVALID-PRODUCT
        )
        ERR-NFT-NOT-FOUND
      )
    )
    ERR-CLAIM-NOT-FOUND
  )
)