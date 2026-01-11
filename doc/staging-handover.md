# Staging Environment Handover

## Project Status: Staging Setup

Staging environment infrastructure has been successfully provisioned using Terraform. The application configuration in `flake.nix` has been updated to include a staging target.

### ✅ Completed Works

1.  **Terraform Infrastructure (Staging)**
    -   Workspace: `cartographer-staging` (TFC Organization: `plural-reality`)
    -   Resources: VPC, EC2, EFS, Security Groups, DNS (`staging.baisoku-kaigi.com`)
    -   **Automatic Metadata**: Infrastructure details are now automatically synced to `infra-cartographer-staging.json`.

2.  **Colmena Configuration**
    -   Added `cartographer-staging` target to `flake.nix`.
    -   **Dynamic Config**: IP address and EFS ID are read automatically from JSON. 
    -   **Portability**: Absolute paths have been removed; uses `$PWD` for dynamic resolution.

### 🚀 Deployment Command

最新のデプロイ手順は [deployment.md](./deployment.md) を参照してください。

```bash
# Navigate to project root
cd /Volumes/External/Develop/cartographer

# Deploy to staging (reads IP from JSON automatically)
colmena apply --on cartographer-staging --impure
```

### 🔍 Verification

After successful deployment:
1.  Access: https://staging.baisoku-kaigi.com
2.  Verify application functionality.
3.  Verify EFS mounting and data persistence.

### 📝 Key Information

-   **Staging URL**: https://staging.baisoku-kaigi.com
-   **Terraform Workspace**: `cartographer-staging`
