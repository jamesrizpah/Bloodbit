# 🩸 Bloodbit - Tokenized Blood Donation Incentive System

A Clarity smart contract that rewards verified blood donors with BLOOD tokens, creating a decentralized incentive system for blood donation.

## 🌟 Features

- 🏥 **Verified Donor System**: Only verified donors can earn rewards
- 🎁 **Donation Rewards**: Earn BLOOD tokens for each donation
- 📊 **Multiple Donation Types**: Different rewards for whole blood, plasma, platelets, and double-red donations
- ⏰ **Daily Limits**: Prevents gaming the system with donation frequency limits
- 🔄 **Token Redemption**: Redeem tokens for real-world benefits
- 📈 **Donation Tracking**: Complete history of all donations and rewards

## 💰 Token Economics

| Donation Type | Reward Multiplier |
|---------------|-------------------|
| Whole Blood   | 1.0x (1,000,000 BLOOD) |
| Plasma        | 0.75x (750,000 BLOOD) |
| Platelets     | 1.25x (1,250,000 BLOOD) |
| Double Red    | 1.5x (1,500,000 BLOOD) |

## 🚀 Getting Started

### Prerequisites
- Clarinet installed
- Stacks wallet for testing

### Installation

```bash
clarinet new bloodbit-project
cd bloodbit-project
```

Copy the contract code to `contracts/Bloodbit.clar`

### Testing

```bash
clarinet console
```

## 📖 Usage

### For Contract Owners

#### Verify a Donor
```clarity
(contract-call? .Bloodbit verify-donor 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM "O+")
```

#### Set Donation Reward
```clarity
(contract-call? .Bloodbit set-donation-reward u2000000)
```

### For Donors

#### Record a Donation
```clarity
(contract-call? .Bloodbit record-donation "whole")
```

#### Check Balance
```clarity
(contract-call? .Bloodbit get-balance tx-sender)
```

#### Redeem Tokens
```clarity
(contract-call? .Bloodbit redeem-tokens u1000000 "hospital-voucher-123")
```

### Read-Only Functions

#### Check if Donor is Verified
```clarity
(contract-call? .Bloodbit is-verified-donor 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM)
```

#### Get Donor Information
```clarity
(contract-call? .Bloodbit get-donor-info 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM)
```

#### Check if Can Donate Today
```clarity
(contract-call? .Bloodbit can-donate-today 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM)
```

## 🔧 Contract Functions

### Public Functions
- `verify-donor` - Verify a new donor (owner only)
- `record-donation` - Record a new donation and mint rewards
- `redeem-tokens` - Redeem tokens for benefits
- `set-donation-reward` - Update base reward amount (owner only)
- `transfer` - Transfer tokens between accounts

### Read-Only Functions
- `get-balance` - Get token balance
- `is-verified-donor` - Check verification status
- `get-donor-info` - Get complete donor information
- `can-donate-today` - Check if donor can donate
- `get-donation-record` - Get specific donation details

## 🛡️ Security Features

- Owner-only functions for critical operations
- Donation frequency limits (24 hours minimum between donations)
- Verified donor requirements
- Input validation for all parameters

## 🎯 Use Cases

1. **Blood Banks**: Incentivize regular donations
2. **Hospitals**: Reward frequent donors with priority services
3. **Health Organizations**: Track and reward community health contributions
4. **Insurance**: Provide discounts for regular donors

## 📊 Token Information

- **Name**: Bloodbit
- **Symbol**: BLOOD
- **Decimals**: 6
- **Type**: SIP-010 Fungible Token

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Test thoroughly
5. Submit a pull request

## 📄 License

This project is open source and available under the MIT License.


