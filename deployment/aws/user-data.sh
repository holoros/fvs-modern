#!/bin/bash
# =============================================================================
# EC2 User Data Script
#
# Run this as user-data when launching an FVS-Online AMI instance.
# It starts the services and optionally configures SSL.
#
# Usage:
#   aws ec2 run-instances \
#     --image-id ami-XXXXXXXXXXXX \
#     --instance-type t3.medium \
#     --user-data file://user-data.sh
# =============================================================================

set -euo pipefail

# Start services
systemctl start fvsonline
systemctl start nginx

# Log startup
echo "FVS-Online started at $(date)" >> /srv/fvs/logs/startup.log
echo "Variants: $(ls /srv/fvs/bin/FVS*.so | wc -l)" >> /srv/fvs/logs/startup.log

# Optional: configure SSL if DOMAIN is set via instance tags or environment
# Uncomment and customize:
#
# DOMAIN="fvs.yourdomain.edu"
# EMAIL="admin@yourdomain.edu"
# certbot --nginx -d "$DOMAIN" --non-interactive --agree-tos --email "$EMAIL"
# systemctl reload nginx
