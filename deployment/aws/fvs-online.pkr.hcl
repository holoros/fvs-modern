# =============================================================================
# FVS-Online Amazon Machine Image (AMI) via Packer
#
# Builds an Ubuntu 24.04 AMI with FVS-Online pre-installed and ready to run.
#
# Prerequisites:
#   1. Install Packer: https://developer.hashicorp.com/packer/install
#   2. Configure AWS credentials (aws configure or env vars)
#   3. Place FVS source repos in the same directory as this file
#
# Build:
#   packer init fvs-online.pkr.hcl
#   packer build \
#     -var 'fvs_source_path=./ForestVegetationSimulator-main' \
#     -var 'iface_source_path=./ForestVegetationSimulator-Interface-main' \
#     -var 'aws_region=us-east-1' \
#     fvs-online.pkr.hcl
#
# Launch:
#   aws ec2 run-instances \
#     --image-id ami-XXXXXXXXXXXX \
#     --instance-type t3.medium \
#     --security-group-ids sg-XXXX \
#     --key-name your-key \
#     --user-data file://user-data.sh
# =============================================================================

packer {
  required_plugins {
    amazon = {
      version = ">= 1.2.0"
      source  = "github.com/hashicorp/amazon"
    }
  }
}

# -------------------------------------------------------------------------
# Variables
# -------------------------------------------------------------------------

variable "aws_region" {
  type    = string
  default = "us-east-1"
}

variable "instance_type" {
  type    = string
  default = "t3.large"
  description = "Instance type for the build (needs enough RAM for R package compilation)"
}

variable "fvs_source_path" {
  type    = string
  default = "./ForestVegetationSimulator-main"
}

variable "iface_source_path" {
  type    = string
  default = "./ForestVegetationSimulator-Interface-main"
}

variable "ami_name_prefix" {
  type    = string
  default = "fvs-online"
}

# -------------------------------------------------------------------------
# Source: Ubuntu 24.04 LTS
# -------------------------------------------------------------------------

source "amazon-ebs" "fvs_online" {
  ami_name      = "${var.ami_name_prefix}-{{timestamp}}"
  instance_type = var.instance_type
  region        = var.aws_region

  source_ami_filter {
    filters = {
      name                = "ubuntu/images/hvm-ssd-gp3/ubuntu-noble-24.04-amd64-server-*"
      root-device-type    = "ebs"
      virtualization-type = "hvm"
    }
    most_recent = true
    owners      = ["099720109477"] # Canonical
  }

  ssh_username = "ubuntu"

  launch_block_device_mappings {
    device_name           = "/dev/sda1"
    volume_size           = 30
    volume_type           = "gp3"
    delete_on_termination = true
  }

  tags = {
    Name        = "FVS-Online"
    Environment = "production"
    BuildDate   = "{{timestamp}}"
  }
}

# -------------------------------------------------------------------------
# Build
# -------------------------------------------------------------------------

build {
  sources = ["source.amazon-ebs.fvs_online"]

  # Upload FVS source
  provisioner "file" {
    source      = var.fvs_source_path
    destination = "/tmp/ForestVegetationSimulator-main"
  }

  # Upload Interface source
  provisioner "file" {
    source      = var.iface_source_path
    destination = "/tmp/ForestVegetationSimulator-Interface-main"
  }

  # Upload deployment configs
  provisioner "file" {
    source      = "../config"
    destination = "/tmp/fvs-deploy-config"
  }

  # Main provisioning script
  provisioner "shell" {
    inline = [
      # ---------------------------------------------------------------
      # System packages
      # ---------------------------------------------------------------
      "sudo apt-get update",
      "sudo DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends gfortran gcc g++ make cmake r-base r-base-dev libcairo2-dev libcurl4-openssl-dev libssl-dev libxml2-dev libsqlite3-dev libglu1-mesa-dev libxt-dev libx11-dev libfreetype6-dev libharfbuzz-dev libfribidi-dev libtiff-dev libjpeg-turbo8-dev libpng-dev libfontconfig1-dev mdbtools sqlite3 nginx certbot python3-certbot-nginx uuid-dev",

      # ---------------------------------------------------------------
      # Create directory structure
      # ---------------------------------------------------------------
      "sudo mkdir -p /srv/fvs/{bin,config,projects,R-libs,logs,www}",
      "sudo useradd -r -m -d /srv/fvs -s /bin/bash fvs || true",

      # ---------------------------------------------------------------
      # Fix known FORMAT bugs
      # ---------------------------------------------------------------
      "cd /tmp/ForestVegetationSimulator-main",
      "sed -i \"s/A8'/A8,'/g\" vbase/initre.f",
      "sed -i \"s/A10'; STAND ORIGIN='/A10, '; STAND ORIGIN='/g\" vbase/initre.f",
      "sed -i \"s/I3' TARGETED/I3, ' TARGETED/\" vbase/initre.f",

      # ---------------------------------------------------------------
      # Build all US variant libraries
      # ---------------------------------------------------------------
      "cd /tmp/ForestVegetationSimulator-main/bin",
      "OSTYPE=linux-gnu make -j$(nproc) US 2>&1 | tail -10",
      "sudo cp /tmp/ForestVegetationSimulator-main/bin/FVS*.so /srv/fvs/bin/",
      "echo \"Built $(ls /srv/fvs/bin/FVS*.so | wc -l) variant libraries\"",

      # ---------------------------------------------------------------
      # Install R dependencies
      # ---------------------------------------------------------------
      "sudo R --no-save -e \"install.packages(c('shiny','Cairo','rhandsontable','ggplot2','RSQLite','plyr','dplyr','colourpicker','rgl','leaflet','zip','openxlsx','shinyFiles','nlme','yaml'), repos='https://cloud.r-project.org', lib='/srv/fvs/R-libs', Ncpus=$(nproc))\"",

      # ---------------------------------------------------------------
      # Install rFVS
      # ---------------------------------------------------------------
      "cp /tmp/fvs-deploy-config/rFVS_NAMESPACE /tmp/ForestVegetationSimulator-Interface-main/rFVS/NAMESPACE",
      "mkdir -p /tmp/ForestVegetationSimulator-Interface-main/rFVS/man",
      "printf '\\\\name{rFVS-package}\\n\\\\alias{rFVS}\\n\\\\docType{package}\\n\\\\title{rFVS}\\n\\\\description{R interface to FVS.}\\n\\\\author{N Crookston}\\n' > /tmp/ForestVegetationSimulator-Interface-main/rFVS/man/rFVS-package.Rd",
      "sudo R CMD INSTALL --library=/srv/fvs/R-libs /tmp/ForestVegetationSimulator-Interface-main/rFVS",

      # ---------------------------------------------------------------
      # Install fvsOL
      # ---------------------------------------------------------------
      "cp /tmp/fvs-deploy-config/fvsOL_NAMESPACE /tmp/ForestVegetationSimulator-Interface-main/fvsOL/NAMESPACE",
      "mkdir -p /tmp/ForestVegetationSimulator-Interface-main/fvsOL/man",
      "printf '\\\\name{fvsOL-package}\\n\\\\alias{fvsOL}\\n\\\\docType{package}\\n\\\\title{fvsOL}\\n\\\\description{FVS-Online.}\\n\\\\author{FVS Staff}\\n' > /tmp/ForestVegetationSimulator-Interface-main/fvsOL/man/fvsOL-package.Rd",
      "sudo R CMD INSTALL --library=/srv/fvs/R-libs /tmp/ForestVegetationSimulator-Interface-main/fvsOL",

      # ---------------------------------------------------------------
      # Copy configuration
      # ---------------------------------------------------------------
      "sudo cp /tmp/fvs-deploy-config/fvsol_config.yml /srv/fvs/config/",
      "sudo cp /tmp/fvs-deploy-config/fvsol_config.R /srv/fvs/config/",
      "sudo sed -i 's|fvs_bin:.*|fvs_bin: \"/srv/fvs/bin\"|' /srv/fvs/config/fvsol_config.yml",
      "sudo sed -i 's|work_dir:.*|work_dir: \"/srv/fvs/projects\"|' /srv/fvs/config/fvsol_config.yml",

      # ---------------------------------------------------------------
      # Create systemd service
      # ---------------------------------------------------------------
      "sudo tee /etc/systemd/system/fvsonline.service > /dev/null <<'UNIT'",
      "[Unit]",
      "Description=FVS-Online Shiny Application",
      "After=network.target",
      "",
      "[Service]",
      "Type=simple",
      "User=fvs",
      "WorkingDirectory=/srv/fvs/projects",
      "Environment=R_LIBS_USER=/srv/fvs/R-libs",
      "ExecStart=/usr/bin/R --no-save --quiet -e \".libPaths('/srv/fvs/R-libs'); library(rFVS); library(fvsOL); options(shiny.port=3838L, shiny.host='127.0.0.1'); fvsOL(prjDir='/srv/fvs/projects', fvsBin='/srv/fvs/bin')\"",
      "Restart=on-failure",
      "RestartSec=10",
      "NoNewPrivileges=true",
      "ProtectSystem=strict",
      "ReadWritePaths=/srv/fvs/projects /srv/fvs/logs",
      "PrivateTmp=true",
      "",
      "[Install]",
      "WantedBy=multi-user.target",
      "UNIT",

      # ---------------------------------------------------------------
      # Configure nginx
      # ---------------------------------------------------------------
      "sudo tee /etc/nginx/sites-available/fvsonline > /dev/null <<'NGINX'",
      "map $http_upgrade $connection_upgrade {",
      "    default upgrade;",
      "    ''      close;",
      "}",
      "server {",
      "    listen 80 default_server;",
      "    server_name _;",
      "    location / {",
      "        proxy_pass http://127.0.0.1:3838;",
      "        proxy_http_version 1.1;",
      "        proxy_set_header Upgrade $http_upgrade;",
      "        proxy_set_header Connection $connection_upgrade;",
      "        proxy_set_header Host $host;",
      "        proxy_set_header X-Real-IP $remote_addr;",
      "        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;",
      "        proxy_set_header X-Forwarded-Proto $scheme;",
      "        proxy_read_timeout 600s;",
      "        proxy_send_timeout 600s;",
      "        client_max_body_size 500M;",
      "    }",
      "}",
      "NGINX",
      "sudo ln -sf /etc/nginx/sites-available/fvsonline /etc/nginx/sites-enabled/",
      "sudo rm -f /etc/nginx/sites-enabled/default",

      # ---------------------------------------------------------------
      # Set permissions and enable services
      # ---------------------------------------------------------------
      "sudo chown -R fvs:fvs /srv/fvs",
      "sudo systemctl daemon-reload",
      "sudo systemctl enable fvsonline",
      "sudo systemctl enable nginx",

      # ---------------------------------------------------------------
      # Verification test
      # ---------------------------------------------------------------
      "sudo -u fvs R --no-save --quiet -e \".libPaths('/srv/fvs/R-libs'); library(rFVS); fvsLoad('FVSne', bin='/srv/fvs/bin'); dims <- fvsGetDims(); cat('Verification:', dims['maxspecies'], 'species,', dims['maxtrees'], 'maxtrees\\n')\"",

      # ---------------------------------------------------------------
      # Cleanup build artifacts
      # ---------------------------------------------------------------
      "sudo rm -rf /tmp/ForestVegetationSimulator-main /tmp/ForestVegetationSimulator-Interface-main /tmp/fvs-deploy-config",
      "sudo apt-get clean",

      "echo 'FVS-Online AMI build complete.'"
    ]
  }
}
