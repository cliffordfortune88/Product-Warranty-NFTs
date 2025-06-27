# 🛡️ Product Warranty NFTs

A blockchain-based warranty system that issues NFT warranties for products, enabling secure warranty verification and claim processing.

## 🚀 Features

- **🎫 NFT-based Warranties**: Each product warranty is represented as a unique NFT
- **👥 Seller Registration**: Businesses can register and get verified as warranty providers
- **📦 Product Registry**: Register products with base warranty information
- **⏰ Automatic Expiry**: Smart contract validates warranty expiration based on block height
- **📋 Claim Processing**: File and process warranty claims with reputation tracking
- **⭐ Reputation System**: Sellers earn/lose reputation based on claim resolutions
- **🔄 Transferable**: Warranty NFTs can be transferred with product ownership

## 📋 Contract Functions

### 🏪 Seller Management
- `register-seller` - Register as a warranty provider
- `verify-seller` - Admin function to verify sellers
- `update-reputation` - Admin function to adjust seller reputation

### 📦 Product Management
- `register-product` - Register a product with warranty information
- `get-product-info` - Retrieve product details

### 🎫 Warranty Operations
- `issue-warranty` - Issue a new warranty NFT to a buyer
- `get-warranty` - Retrieve warranty details
- `is-warranty-valid` - Check if warranty is still active
- `get-warranty-expiry` - Get warranty expiration block
- `deactivate-warranty` - Seller can deactivate a warranty
- `transfer` - Transfer warranty NFT to new owner

### 📋 Claims Management
- `file-warranty-claim` - File a warranty claim
- `process-claim` - Seller processes filed claims
- `get-warranty-claim` - Retrieve claim details

## 🛠️ Usage Examples

### Register as a Seller
```clarity
(contract-call? .product-warranty-nfts register-seller "TechCorp Inc" "contact@techcorp.com")
```

### Register a Product
```clarity
(contract-call? .product-warranty-nfts register-product "LAPTOP-2024-001" "Gaming Laptop" "Electronics" u52560)
```

### Issue a Warranty
```clarity
(contract-call? .product-warranty-nfts issue-warranty 
  "LAPTOP-2024-001" 
  'ST1HTBVD3JG9C05J7HBJTHGR0GGW7KXW28M5JS8QE 
  u52560 
  "Standard" 
  "Dell Gaming Laptop, Serial: ABC123")
```

### File a Warranty Claim
```clarity
(contract-call? .product-warranty-nfts file-warranty-claim u1 "Screen flickering issue")
```

### Process a Claim
```clarity
(contract-call? .product-warranty-nfts process-claim u1 "approved" "Replacement approved")
```

## 🎯 Key Benefits

- **🔒 Tamper-proof**: Warranty details stored immutably on blockchain
- **✅ Easy Verification**: Anyone can verify warranty validity
- **📊 Transparent Claims**: All claims and resolutions are recorded
- **🏆 Reputation System**: Builds trust through seller reputation tracking
- **💰 Cost Effective**: Reduces warranty fraud and administrative costs
- **🌐 Interoperable**: Works across different platforms and marketplaces

## 🔧 Technical Details

- **Blockchain**: Stacks (Bitcoin Layer 2)
- **Language**: Clarity Smart Contract
- **Token Standard**: SIP-009 NFT
- **Block Time**: Warranty duration measured in Stacks blocks
- **Storage**: On-chain warranty and claim data

## 📈 Reputation System

- Sellers start with 50 reputation points
- +5 points for approved claims
- -5 points for rejected claims
- Reputation affects seller credibility

## ⚡ Getting Started

1. Deploy the contract to Stacks testnet/mainnet
2. Register as a seller using `register-seller`
3. Register your products with `register-product` 
4. Issue warranties to customers with `issue-warranty`
5. Process claims as they come in with `process-claim`

## 🤝 Contributing

This is an open-source project. Feel free to contribute improvements, bug fixes, or additional features!

## 📄 License

MIT License - Feel free to use this code for your warranty system needs.
