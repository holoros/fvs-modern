# FVS-Online Hosting Guide: Production Deployment on Linux

A comprehensive guide for deploying the Forest Vegetation Simulator (FVS) Online interface on a public-facing Linux server.

**Last Updated:** March 2026
**Target Audience:** System Administrators, Research Computing Staff
**FVS Version:** 4.0+
**Shiny Server:** Open Source

---

## 1. Prerequisites and Server Requirements

### Hardware Requirements

* **CPU:** Minimum 4 cores (8+ recommended for concurrent users)
* **Memory:** 8 GB RAM minimum; 16 GB or more for production
* **Storage:** 100 GB+ SSD storage (25 GB OS + system, 75 GB+ for FVS data and project files)
* **Network:** Gigabit ethernet recommended
* **Uptime SLA:** Plan for adequate redundancy and failover if mission-critical

### Operating System

* **Ubuntu 22.04 LTS** (primary recommendation)
  * Tested with Canonical LTS support
  * Package availability is excellent

* **RHEL 8/9** (supported alternative)
  * Use EPEL repository for additional packages
  * Some package names and versions differ (noted throughout)

Do NOT use CentOS 8 (end-of-life). Use CentOS Stream or RHEL.

### Network Requirements

* **Inbound Ports:**
  * Port 80 (HTTP) for redirects and Let's Encrypt validation
  * Port 443 (HTTPS) for encrypted traffic
  * SSH port 22 (restricted to admin networks recommended)

* **Outbound Ports:**
  * Port 25, 465, or 587 for SMTP (to institutional mail relay)
  * DNS resolution (port 53 to corporate DNS)

* **DNS:** A records for your domain (e.g., fvs.yourinstitution.edu)

### Prerequisites Checklist

- [ ] Ubuntu 22.04 LTS or RHEL 8/9 installed and updated
- [ ] Root or sudo access configured
- [ ] Adequate disk space verified
- [ ] Network connectivity tested
- [ ] SMTP relay endpoint identified
- [ ] SSL certificate plan (Let's Encrypt or institutional CA)
- [ ] Domain name registered and DNS configured

---

## 2. Installing System Dependencies

### Initial System Update

**Ubuntu 22.04:**
```bash
sudo apt update
sudo apt upgrade -y
```

**RHEL 8/9:**
```bash
sudo yum update -y
sudo yum install -y epel-release
```

### Build Tools and Compilers

These are essential for compiling FVS from source.

**Ubuntu 22.04:**
```bash
sudo apt install -y \
  build-essential \
  gcc \
  gfortran \
  cmake \
  git \
  wget \
  curl
```

**RHEL 8/9:**
```bash
sudo yum groupinstall -y "Development Tools"
sudo yum install -y \
  gcc \
  gcc-gfortran \
  cmake \
  git \
  wget \
  curl
```

### R Installation

FVS requires R 4.0 or later. Install from CRAN repositories for current versions.

**Ubuntu 22.04:**
```bash
# Add CRAN repository (Ubuntu-specific)
sudo apt install -y dirmngr gnupg apt-transport-https ca-certificates software-properties-common
sudo add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/"
sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9

# Install R
sudo apt install -y r-base r-base-dev
```

**RHEL 8/9:**
```bash
# Enable PowerTools repository for dependencies
sudo yum install -y dnf-plugins-core
sudo yum config-manager --set-enabled powertools

# Install R from EPEL
sudo yum install -y R-core R-core-devel
```

Verify installation:
```bash
R --version
```

Expected output: R version 4.0.0 or later

### System Libraries for R Packages

These libraries are required by various R packages.

**Ubuntu 22.04:**
```bash
sudo apt install -y \
  libsqlite3-dev \
  sqlite3 \
  mdbtools \
  libxml2-dev \
  libcurl4-openssl-dev \
  libssl-dev \
  libcairo2-dev \
  libpango1.0-dev \
  libjpeg-dev \
  libpng-dev \
  libtiff-dev \
  libgeos-dev \
  libproj-dev
```

**RHEL 8/9:**
```bash
sudo yum install -y \
  sqlite-devel \
  sqlite \
  mdbtools \
  libxml2-devel \
  curl-devel \
  openssl-devel \
  cairo-devel \
  pango-devel \
  libjpeg-turbo-devel \
  libpng-devel \
  libtiff-devel \
  geos-devel \
  proj-devel
```

### Email Tools

Choose one email tool for project notifications.

**ssmtp (simpler, lightweight):**

**Ubuntu 22.04:**
```bash
sudo apt install -y ssmtp
```

**RHEL 8/9:**
```bash
# ssmtp not in RHEL repos; use msmtp instead
sudo yum install -y msmtp
```

**msmtp (modern alternative, recommended):**

**Ubuntu 22.04:**
```bash
sudo apt install -y msmtp msmtp-mta
```

**RHEL 8/9:**
```bash
sudo yum install -y msmtp
```

### Node.js and npm (Optional for Development)

Only needed if you plan to modify the frontend:
```bash
curl -fsSL https://deb.nodesource.com/setup_lts.x | sudo -E bash -
sudo apt install -y nodejs
```

---

## 3. Building FVS from Source

### Clone FVS Repository

```bash
cd /tmp
git clone https://github.com/USDAForestService/ForestVegetationSimulator.git
cd ForestVegetationSimulator
```

### Prepare Build Directory

Create a dedicated directory for FVS binaries:

```bash
sudo mkdir -p /srv/fvs/bin
sudo mkdir -p /srv/fvs/lib
sudo mkdir -p /srv/fvs/source
```

### CMake Configuration for Linux

Navigate to the source directory and configure with CMake:

```bash
cd /tmp/ForestVegetationSimulator
mkdir -p build
cd build

# Configure with Unix Makefiles generator
cmake -G "Unix Makefiles" \
  -DCMAKE_BUILD_TYPE=Release \
  -DCMAKE_INSTALL_PREFIX=/srv/fvs \
  -DBUILD_SHARED_LIBS=ON \
  ..
```

**CMake Options Explanation:**

* `-G "Unix Makefiles"`: Use standard Unix Makefiles (default on Linux)
* `-DCMAKE_BUILD_TYPE=Release`: Optimize for performance; use `Debug` only for troubleshooting
* `-DCMAKE_INSTALL_PREFIX=/srv/fvs`: Installation root directory
* `-DBUILD_SHARED_LIBS=ON`: Build shared libraries (.so files) instead of static

### Build FVS Variants

Build all variant shared libraries. This may take 10-20 minutes:

```bash
cd /tmp/ForestVegetationSimulator/build
make -j$(nproc)
make install
```

The `-j$(nproc)` flag uses all available CPU cores for parallel compilation.

### Verify Compiled Binaries

After installation, verify that shared libraries are present:

```bash
ls -la /srv/fvs/lib/
```

Expected output should include files like:

```
libFVS.so
libFVSne.so
libFVSwc.so
libFVSwc.so.0
libFVSwc.so.0.0.0
libFVSkc.so
... (other variant libraries)
```

The exact filenames depend on your FVS version and variants compiled. Common variants include:

* `FVSne` (Northeast)
* `FVSwc` (Westside Cascade)
* `FVSkc` (Eastside Cascade)
* `FVSec` (East Central)
* `FVSrm` (Rocky Mountain)
* `FVSso` (South)
* `FVSpn` (Pacific Northwest)
* `FVSsw` (Southwest)

Set correct permissions:

```bash
sudo chmod 755 /srv/fvs/lib/*.so*
sudo chmod 755 /srv/fvs/bin/*
```

### Backup Source Code

Keep the source for reference:

```bash
sudo cp -r /tmp/ForestVegetationSimulator /srv/fvs/source/
```

---

## 4. Installing the FVS Interface Components

### Install rFVS R Package

Clone and build the rFVS wrapper package:

```bash
cd /tmp
git clone https://github.com/USDAForestService/ForestVegetationSimulator-Interface.git
cd ForestVegetationSimulator-Interface/rFVS

# Install as root to make it system-wide
sudo R CMD INSTALL .
```

Verify installation:
```bash
R -e "library(rFVS); cat('rFVS loaded successfully\n')"
```

### Install fvsOL R Package

Within the same Interface repository:

```bash
cd /tmp/ForestVegetationSimulator-Interface/fvsOL
sudo R CMD INSTALL .
```

Verify:
```bash
R -e "library(fvsOL); cat('fvsOL loaded successfully\n')"
```

### Install Required R Packages

Install all CRAN dependencies as root (system-wide):

```bash
sudo R -e "install.packages(c(
  'shiny',
  'RSQLite',
  'ggplot2',
  'rgl',
  'leaflet',
  'rhandsontable',
  'openxlsx',
  'zip',
  'dplyr',
  'plyr',
  'parallel',
  'colourpicker',
  'digest',
  'yaml'
), repos='https://cloud.r-project.org')"
```

This may take 5-15 minutes depending on network speed and system resources.

### Verify All Packages Load

```bash
sudo R -e "
library(shiny)
library(rFVS)
library(fvsOL)
library(RSQLite)
library(ggplot2)
library(rgl)
library(leaflet)
library(rhandsontable)
library(openxlsx)
library(zip)
library(dplyr)
library(plyr)
library(colourpicker)
library(digest)
library(yaml)
cat('All packages loaded successfully\n')
"
```

### Copy Shiny App Files

The FVSPrjBldr (Project Builder) is the main Shiny app. Copy it to Shiny Server:

```bash
sudo mkdir -p /srv/shiny-server/fvs-online
sudo cp -r /tmp/ForestVegetationSimulator-Interface/FVSPrjBldr/* /srv/shiny-server/fvs-online/
```

Verify directory structure:
```bash
ls -la /srv/shiny-server/fvs-online/
```

Should contain: `server.R`, `ui.R`, `www/`, and other app files.

---

## 5. Configuration with fvsol_config.yml

FVS-Online uses a new centralized configuration system that separates environment-specific settings from application code.

### Configuration Architecture

The system consists of two parts:

1. **fvsol_config.R**: R script that reads YAML config and sets up FVS environment
2. **fvsol_config.yml**: YAML file with all deployment-specific settings

### Creating fvsol_config.yml

Create the main configuration file. This is typically placed in the FVSPrjBldr directory:

```bash
sudo tee /srv/shiny-server/fvs-online/fvsol_config.yml > /dev/null << 'EOF'
# FVS-Online Configuration File
# Production Deployment Example

# Application settings
app:
  title: "Forest Vegetation Simulator Online"
  version: "1.0.0"
  environment: "production"
  debug: false

# FVS paths and binaries
fvs:
  # Path to compiled FVS shared libraries
  lib_path: "/srv/fvs/lib"

  # Path to FVS binary directory
  bin_path: "/srv/fvs/bin"

  # List of available variants (case-sensitive)
  variants:
    - "FVSne"
    - "FVSwc"
    - "FVSwc"
    - "FVSkc"
    - "FVSec"
    - "FVSrm"
    - "FVSso"
    - "FVSpn"
    - "FVSsw"

# Project management
projects:
  # Base directory where user projects are stored
  work_dir: "/srv/fvs/projects"

  # Auto-cleanup settings (optional)
  auto_cleanup:
    enabled: true
    max_age_days: 30
    cleanup_time: "02:00"  # UTC time

  # Project file size limits
  max_project_size_mb: 500

# Database settings
database:
  # FVS data database path
  fvs_data_db: "/srv/fvs/projects/FVS_Data.db.default"

  # Optional: separate SQLite WAL settings for performance
  sqlite:
    journal_mode: "WAL"
    synchronous: "NORMAL"

# Email configuration
email:
  # Enable or disable email notifications
  enabled: true

  # SMTP relay settings
  smtp:
    host: "mail.yourinstitution.edu"
    port: 25
    use_tls: false
    use_ssl: false
    from_address: "fvs-online@yourinstitution.edu"
    from_name: "FVS-Online System"

  # Project completion notifications
  notifications:
    enabled: true
    notify_on_completion: true
    notify_on_error: true

# Logging configuration
logging:
  level: "INFO"
  file_path: "/var/log/fvs-online/app.log"
  max_file_size_mb: 100
  backup_count: 10

# Security settings
security:
  # Enable CORS for API endpoints (if applicable)
  cors_enabled: false

  # Maximum upload size in MB
  max_upload_size_mb: 100

  # Session timeout in minutes
  session_timeout_minutes: 120

  # Enable rate limiting (optional)
  rate_limiting:
    enabled: false
    requests_per_minute: 100

# Performance tuning
performance:
  # Maximum concurrent simulations
  max_concurrent_sims: 4

  # Parallel processing for R operations
  parallel:
    enabled: true
    cores: 4

  # Cache settings
  cache:
    enabled: true
    max_age_hours: 24

# Monitoring and analytics
monitoring:
  enabled: false
  # Optional: Sentry, NewRelic, or other APM endpoints
  apm_endpoint: ""
EOF
```

### Understanding Configuration Keys

**fvs.lib_path:** Absolute path to compiled FVS shared libraries (from Section 3)

**fvs.variants:** List of compiled variant codes. Remove any not built on your system.

**projects.work_dir:** Base directory for all user projects. Must have read/write by shiny user.

**projects.auto_cleanup.enabled:** If true, old projects are deleted automatically.

**projects.auto_cleanup.max_age_days:** Projects older than this are removed if auto_cleanup is enabled.

**database.fvs_data_db:** Path to the default FVS database (copy from source).

**email.smtp.host:** Your institutional SMTP relay endpoint.

**email.smtp.port:** Usually 25 (no auth), 465 (TLS), or 587 (STARTTLS).

**logging.file_path:** Where application logs are written.

**performance.parallel.cores:** Should not exceed actual CPU cores on the server.

### Environment Variable Overrides

For flexibility, you can override configuration values with environment variables. This is useful for secrets and per-deployment tweaks:

```bash
# All environment variable names follow the pattern FVSOL_<SECTION>_<KEY>

# Examples:
export FVSOL_EMAIL_SMTP_HOST="mail.example.com"
export FVSOL_EMAIL_SMTP_PORT="587"
export FVSOL_PROJECTS_WORK_DIR="/mnt/fvs-data"
export FVSOL_PROJECTS_AUTO_CLEANUP_ENABLED="false"
```

To use in systemd service file:
```ini
[Service]
Environment="FVSOL_EMAIL_SMTP_HOST=mail.yourinstitution.edu"
Environment="FVSOL_PROJECTS_WORK_DIR=/srv/fvs/projects"
```

### Creating fvsol_config.R

Create the configuration loader script. Place in FVSPrjBldr directory:

```bash
sudo tee /srv/shiny-server/fvs-online/fvsol_config.R > /dev/null << 'EOF'
# FVS-Online Configuration Loader
# This script reads fvsol_config.yml and sets up the R environment

library(yaml)

# Load configuration from YAML file
config_file <- file.path(dirname(getwd()), "fvsol_config.yml")
if (!file.exists(config_file)) {
  config_file <- "fvsol_config.yml"
}

if (!file.exists(config_file)) {
  stop("Configuration file not found: ", config_file)
}

# Parse YAML
fvsol_config <- yaml::read_yaml(config_file)

# Function to get config value with environment variable override
get_config <- function(path, default = NULL) {
  # Convert path like "email.smtp.host" to env var "FVSOL_EMAIL_SMTP_HOST"
  env_var <- paste0("FVSOL_",
                   toupper(gsub("\\.", "_", path)))

  # Check environment variable first
  env_value <- Sys.getenv(env_var, unset = NA)
  if (!is.na(env_value)) {
    return(env_value)
  }

  # Fall back to YAML config
  parts <- strsplit(path, "\\.")[[1]]
  value <- fvsol_config
  for (part in parts) {
    if (is.null(value[[part]])) {
      return(default)
    }
    value <- value[[part]]
  }
  return(value)
}

# Set up FVS paths
FVS_LIB_PATH <- get_config("fvs.lib_path")
FVS_BIN_PATH <- get_config("fvs.bin_path")
PROJECTS_DIR <- get_config("projects.work_dir")
FVS_DATA_DB <- get_config("database.fvs_data_db")

# Verify paths exist
if (!dir.exists(FVS_LIB_PATH)) {
  stop("FVS library path does not exist: ", FVS_LIB_PATH)
}

# Set R library paths for rFVS to find compiled binaries
Sys.setenv(LD_LIBRARY_PATH = paste(FVS_LIB_PATH,
                                   Sys.getenv("LD_LIBRARY_PATH"),
                                   sep = ":"))

# Create projects directory if it doesn't exist
if (!dir.exists(PROJECTS_DIR)) {
  dir.create(PROJECTS_DIR, recursive = TRUE, mode = "0755")
}

# Configure logging
LOG_FILE <- get_config("logging.file_path")
if (!is.null(LOG_FILE) && LOG_FILE != "") {
  log_dir <- dirname(LOG_FILE)
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE, mode = "0755")
  }
}

# Make config available globally
.GlobalEnv$fvsol_config <- fvsol_config
.GlobalEnv$fvsol_get_config <- get_config

# Verify FVS variant availability
available_variants <- list.files(FVS_LIB_PATH, pattern = "^libFVS.*\\.so$")
cat("Available FVS variants:", paste(available_variants, collapse = ", "), "\n")

# Export for use in Shiny app
.GlobalEnv$FVS_LIB_PATH <- FVS_LIB_PATH
.GlobalEnv$FVS_BIN_PATH <- FVS_BIN_PATH
.GlobalEnv$PROJECTS_DIR <- PROJECTS_DIR
.GlobalEnv$FVS_DATA_DB <- FVS_DATA_DB

cat("FVS-Online configuration loaded successfully\n")
EOF
```

### Configuration File Permissions

```bash
sudo chmod 644 /srv/shiny-server/fvs-online/fvsol_config.yml
sudo chmod 644 /srv/shiny-server/fvs-online/fvsol_config.R
sudo chown root:root /srv/shiny-server/fvs-online/fvsol_config.*
```

---

## 6. Applying Configuration Patches

The FVS-Online interface requires several file modifications to integrate the new configuration system.

### File Overview

Three main files need to be updated:

* `FVSPrjBldr/server.R`: Main Shiny server logic
* `FVSPrjBldr/prjListEmail.R`: Email notification module
* `FVSPrjBldr/ui.R`: User interface definition
* `fvsol_config.R`: Configuration loader (created in Section 5)

### Patch 1: FVSPrjBldr/server.R

Replace the first 58 lines with configuration initialization:

**Original server.R starts with library() calls and direct path assignments.**

**Patch: Replace lines 1-58 with:**

```r
# Load configuration system
source("fvsol_config.R")

# Standard library imports
library(shiny)
library(RSQLite)
library(ggplot2)
library(rgl)
library(leaflet)
library(rhandsontable)
library(openxlsx)
library(zip)
library(dplyr)
library(plyr)
library(parallel)
library(colourpicker)
library(digest)

# Load rFVS and fvsOL packages
library(rFVS)
library(fvsOL)

# Configuration is now loaded via fvsol_config.R
# Access configuration values using:
# fvsol_get_config("email.smtp.host")
# or use global variables set by fvsol_config.R:
# FVS_LIB_PATH, FVS_BIN_PATH, PROJECTS_DIR, FVS_DATA_DB

# Initialize logging if configured
if (!is.null(fvsol_config$logging$file_path) &&
    fvsol_config$logging$file_path != "") {
  LOG_FILE <- fvsol_config$logging$file_path
}

# Rest of server.R continues...
```

### Patch 2: Integrate Configuration in Email Module (prjListEmail.R)

Locate the email sending function and update SMTP settings:

**Find this section:**
```r
# Old email configuration (hardcoded)
SMTP_HOST <- "localhost"
SMTP_PORT <- 25
FROM_ADDRESS <- "fvs@localhost"
```

**Replace with:**
```r
# Configuration-driven email settings
SMTP_HOST <- fvsol_get_config("email.smtp.host", "localhost")
SMTP_PORT <- fvsol_get_config("email.smtp.port", 25)
FROM_ADDRESS <- fvsol_get_config("email.smtp.from_address", "fvs@localhost")
FROM_NAME <- fvsol_get_config("email.smtp.from_name", "FVS System")
EMAIL_ENABLED <- fvsol_get_config("email.enabled", FALSE)

if (!EMAIL_ENABLED) {
  # Email notifications disabled in config
  cat("Email notifications are disabled\n")
}
```

### Patch 3: Place fvsol_config.R in Multiple Locations

FVSPrjBldr needs fvsol_config.R to load when the Shiny app starts:

```bash
# Already placed in FVSPrjBldr/ (Section 5)
ls -la /srv/shiny-server/fvs-online/fvsol_config.R

# Also place in fvsOL package for consistency
sudo cp /srv/shiny-server/fvs-online/fvsol_config.R \
  /tmp/ForestVegetationSimulator-Interface/fvsOL/inst/extdata/

# And reinstall fvsOL
cd /tmp/ForestVegetationSimulator-Interface/fvsOL
sudo R CMD INSTALL .
```

### Patch 4: Update ui.R for Configuration-Driven UI

If your ui.R has hardcoded paths or options, update references to use configuration:

**Find:**
```r
# Old hardcoded variant list
variant_list <- c("FVSne", "FVSwc", "FVSkc", "FVSec", "FVSrm", "FVSso", "FVSpn", "FVSsw")
```

**Replace with:**
```r
# Configuration-driven variant list
variant_list <- fvsol_get_config("fvs.variants",
  c("FVSne", "FVSwc", "FVSkc", "FVSec", "FVSrm", "FVSso", "FVSpn", "FVSsw"))
```

### Verify Patches Are Applied

Check that configuration loads correctly:

```bash
sudo -u shiny R -e "
setwd('/srv/shiny-server/fvs-online')
source('fvsol_config.R')
cat('Configuration loaded:', FVS_LIB_PATH, '\n')
"
```

---

## 7. Setting Up Shiny Server

### Install Shiny Server Open Source

**Ubuntu 22.04:**

```bash
# Download the latest version
cd /tmp
wget https://download3.rstudio.org/ubuntu-18.04/x86_64/shiny-server-latest-amd64.deb

# Install
sudo gdebi shiny-server-latest-amd64.deb

# Verify installation
shiny-server --version
```

**RHEL 8/9:**

```bash
# Download RPM
cd /tmp
wget https://download3.rstudio.org/centos-7/x86_64/shiny-server-latest-x86_64.rpm

# Install
sudo yum install -y ./shiny-server-latest-x86_64.rpm
```

### Configure Shiny Server

Shiny Server Open Source uses `/etc/shiny-server/shiny-server.conf`. Create a production configuration:

```bash
sudo tee /etc/shiny-server/shiny-server.conf > /dev/null << 'EOF'
# Shiny Server Configuration for FVS-Online Production

# Run Shiny Server as the 'shiny' user
run_as shiny;

# Define the main server block
server {
  listen 3838;

  # Site directory for uploaded projects
  site_dir /srv/fvs/projects;

  # App directory for FVS-Online application
  location /fvs-online {
    app_dir /srv/shiny-server/fvs-online;

    # Application settings
    app_init_timeout 120;
    app_idle_timeout 0;

    # Enable bookmarking for state preservation
    bookmark_state_dir /srv/shiny-server/fvs-online/bookmarks;

    # Log configuration
    log_dir /var/log/shiny-server;
    access_log /var/log/shiny-server/fvs-online-access.log;
  }

  # Static assets (optional)
  location / {
    # Serve static files from /srv/shiny-server/www/
    dir /srv/shiny-server/www;
  }

  # Disable default showcase mode
  disable_protocols http;
}

# Configure HTTPS if using Shiny Server Pro (not available in Open Source)
# For reverse proxy with nginx, use http on port 3838 only
EOF
```

**Key Configuration Options:**

* `listen 3838`: Shiny Server listens on port 3838 (internal only)
* `site_dir`: Directory for user-uploaded projects
* `app_dir`: Location of FVS-Online Shiny app
* `app_init_timeout 120`: Allow 120 seconds for app startup
* `app_idle_timeout 0`: Never auto-close idle apps (set to positive integer for timeout)
* `bookmark_state_dir`: For preserving user session state

### Create Required Directories

```bash
sudo mkdir -p /srv/shiny-server/fvs-online/bookmarks
sudo mkdir -p /srv/shiny-server/www
sudo mkdir -p /var/log/shiny-server

# Set correct ownership
sudo chown -R shiny:shiny /srv/shiny-server
sudo chown -R shiny:shiny /var/log/shiny-server
sudo chown -R shiny:shiny /srv/fvs/projects

# Set permissions
sudo chmod 755 /srv/shiny-server
sudo chmod 755 /srv/shiny-server/fvs-online
sudo chmod 755 /srv/fvs/projects
```

### Start and Enable Shiny Server

```bash
# Start the service
sudo systemctl start shiny-server

# Enable on boot
sudo systemctl enable shiny-server

# Check status
sudo systemctl status shiny-server

# View logs
sudo tail -f /var/log/shiny-server/fvs-online-access.log
```

### Verify Shiny Server is Running

```bash
# Check if listening on port 3838
sudo netstat -tlnp | grep 3838

# Test local connection (from server)
curl -s http://localhost:3838/fvs-online/ | head -20
```

---

## 8. Reverse Proxy with nginx

Shiny Server Open Source does not support HTTPS directly. Use nginx as a reverse proxy to handle HTTPS/TLS termination.

### Install nginx

**Ubuntu 22.04:**
```bash
sudo apt install -y nginx
```

**RHEL 8/9:**
```bash
sudo yum install -y nginx
```

### Install Certbot for Let's Encrypt

```bash
# Ubuntu 22.04
sudo apt install -y certbot python3-certbot-nginx

# RHEL 8/9
sudo yum install -y certbot python3-certbot-nginx
```

### Create nginx Configuration

Backup the default config:

```bash
sudo cp /etc/nginx/nginx.conf /etc/nginx/nginx.conf.backup
```

Create a site configuration for FVS-Online:

```bash
sudo tee /etc/nginx/sites-available/fvs-online.conf > /dev/null << 'EOF'
# FVS-Online nginx Reverse Proxy Configuration

# Rate limiting (optional but recommended)
limit_req_zone $binary_remote_addr zone=fvsol_limit:10m rate=10r/s;

server {
    listen 80;
    server_name fvs.yourinstitution.edu;

    # Redirect HTTP to HTTPS
    location / {
        return 301 https://$server_name$request_uri;
    }

    # Allow Let's Encrypt challenges
    location /.well-known/acme-challenge/ {
        root /var/www/certbot;
    }
}

server {
    listen 443 ssl http2;
    server_name fvs.yourinstitution.edu;

    # SSL Certificate Configuration (from Let's Encrypt)
    ssl_certificate /etc/letsencrypt/live/fvs.yourinstitution.edu/fullchain.pem;
    ssl_certificate_key /etc/letsencrypt/live/fvs.yourinstitution.edu/privkey.pem;

    # SSL Security Best Practices
    ssl_protocols TLSv1.2 TLSv1.3;
    ssl_ciphers 'ECDHE-ECDSA-AES128-GCM-SHA256:ECDHE-RSA-AES128-GCM-SHA256:ECDHE-ECDSA-AES256-GCM-SHA384:ECDHE-RSA-AES256-GCM-SHA384';
    ssl_prefer_server_ciphers on;
    ssl_session_cache shared:SSL:10m;
    ssl_session_timeout 10m;
    ssl_stapling on;
    ssl_stapling_verify on;

    # HSTS (optional but recommended)
    add_header Strict-Transport-Security "max-age=31536000; includeSubDomains" always;

    # Security headers
    add_header X-Frame-Options "SAMEORIGIN" always;
    add_header X-Content-Type-Options "nosniff" always;
    add_header X-XSS-Protection "1; mode=block" always;
    add_header Referrer-Policy "strict-origin-when-cross-origin" always;

    # Client upload size limit (matches Shiny config)
    client_max_body_size 100m;

    # Logging
    access_log /var/log/nginx/fvs-online-access.log;
    error_log /var/log/nginx/fvs-online-error.log;

    # FVS-Online proxy configuration
    location /fvs-online {
        # Apply rate limiting
        limit_req zone=fvsol_limit burst=20 nodelay;

        # Proxy settings
        proxy_pass http://localhost:3838;
        proxy_http_version 1.1;

        # Headers
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;

        # WebSocket support (CRITICAL for Shiny)
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "upgrade";

        # Timeouts
        proxy_connect_timeout 60s;
        proxy_send_timeout 60s;
        proxy_read_timeout 60s;

        # Buffering
        proxy_buffering off;
    }

    # Static file serving (if /srv/shiny-server/www is configured)
    location /static {
        alias /srv/shiny-server/www;
        expires 30d;
        add_header Cache-Control "public, immutable";
    }

    # Health check endpoint (optional)
    location /health {
        access_log off;
        return 200 "healthy\n";
        add_header Content-Type text/plain;
    }

    # Deny access to sensitive files
    location ~ /\. {
        deny all;
    }

    location ~ /fvsol_config {
        deny all;
    }
}
EOF
```

### Enable the Site Configuration

**Ubuntu 22.04:**
```bash
sudo ln -s /etc/nginx/sites-available/fvs-online.conf /etc/nginx/sites-enabled/
sudo rm /etc/nginx/sites-enabled/default
```

**RHEL 8/9:**
Create in `/etc/nginx/conf.d/`:
```bash
sudo cp /etc/nginx/sites-available/fvs-online.conf /etc/nginx/conf.d/fvs-online.conf
```

### Obtain SSL Certificate

```bash
sudo certbot certonly --standalone \
  -d fvs.yourinstitution.edu \
  --agree-tos \
  --no-eff-email \
  -m admins@yourinstitution.edu
```

If the domain resolves, you can also use the webroot method:

```bash
# Create webroot directory
sudo mkdir -p /var/www/certbot

# Obtain certificate
sudo certbot certonly --webroot \
  -w /var/www/certbot \
  -d fvs.yourinstitution.edu \
  --agree-tos \
  --no-eff-email \
  -m admins@yourinstitution.edu
```

### Test and Enable nginx

```bash
# Test configuration syntax
sudo nginx -t

# Reload nginx
sudo systemctl reload nginx

# Enable on boot
sudo systemctl enable nginx

# Check status
sudo systemctl status nginx
```

### Set Up Automatic Certificate Renewal

Certbot includes a systemd timer. Verify it's enabled:

```bash
sudo systemctl enable certbot.timer
sudo systemctl start certbot.timer

# Check renewal schedule
sudo systemctl list-timers certbot.timer
```

### Test HTTPS Access

```bash
curl -I https://fvs.yourinstitution.edu/fvs-online/
```

Should return HTTP 200.

---

## 9. Email Configuration

### Using msmtp (Recommended)

msmtp is modern, flexible, and works with any SMTP relay.

Create or edit `~/.msmtprc` for the shiny user:

```bash
sudo tee /var/lib/shiny/.msmtprc > /dev/null << 'EOF'
# msmtp configuration for FVS-Online
# This file should be owned by 'shiny' user

# Set default values for all accounts
defaults
logfile /var/log/msmtp.log
tls on
tls_trust_file /etc/ssl/certs/ca-certificates.crt

# Institutional SMTP relay account
account yourinstitution
host mail.yourinstitution.edu
port 25
from fvs-online@yourinstitution.edu
user your_smtp_user
password your_smtp_password

# If using TLS on port 587
# port 587
# tls on

# Set the default account
account default : yourinstitution
EOF
```

Set correct permissions:

```bash
sudo chmod 600 /var/lib/shiny/.msmtprc
sudo chown shiny:shiny /var/lib/shiny/.msmtprc
```

Create msmtp log location:

```bash
sudo touch /var/log/msmtp.log
sudo chown shiny:shiny /var/log/msmtp.log
sudo chmod 644 /var/log/msmtp.log
```

### Using ssmtp (Ubuntu 22.04)

Alternative if you prefer ssmtp:

```bash
sudo tee /etc/ssmtp/ssmtp.conf > /dev/null << 'EOF'
# /etc/ssmtp/ssmtp.conf -- a config file for sSMTP

# The user that gets all the mails (UID < 1000, usually the "mail" user)
root=postmaster

# The mail server (where the mail is sent to), both port 25 and/or 587 (submission) can be used
MailHub=mail.yourinstitution.edu:25

# Email address to use for MAIL FROM
FromLineOverride=YES
AuthUser=your_smtp_user
AuthPass=your_smtp_password
UseSTARTTLS=NO
EOF
```

Set permissions:

```bash
sudo chmod 640 /etc/ssmtp/ssmtp.conf
sudo chown root:mail /etc/ssmtp/ssmtp.conf
```

### Testing Email Configuration

Test sending an email:

```bash
# Using msmtp
echo "Test email from FVS-Online" | \
  sudo -u shiny msmtp -a yourinstitution admin@yourinstitution.edu

# Using ssmtp
echo -e "Subject: FVS Test\n\nTest email" | \
  sendmail admin@yourinstitution.edu
```

Check mail logs:

```bash
sudo tail -f /var/log/msmtp.log
sudo tail -f /var/log/mail.log
```

### Integrating Email in FVS Configuration

Update `fvsol_config.yml` with correct SMTP details:

```yaml
email:
  enabled: true
  smtp:
    host: "mail.yourinstitution.edu"
    port: 25
    use_tls: false
    from_address: "fvs-online@yourinstitution.edu"
    from_name: "FVS-Online System"
  notifications:
    enabled: true
    notify_on_completion: true
```

### Disabling Email (If Not Needed)

If email is not available or desired, disable it:

```yaml
email:
  enabled: false
```

The application will skip email notifications gracefully.

---

## 10. Project Directory Setup

### Create Project Base Directory

```bash
sudo mkdir -p /srv/fvs/projects
sudo mkdir -p /srv/fvs/projects/uploads
sudo mkdir -p /srv/fvs/projects/results
```

### Set Directory Permissions

The shiny user must have full access:

```bash
sudo chown -R shiny:shiny /srv/fvs/projects
sudo chmod 755 /srv/fvs/projects
sudo chmod 755 /srv/fvs/projects/uploads
sudo chmod 755 /srv/fvs/projects/results
```

### Copy Default FVS Database

The default FVS database file needs to be available:

```bash
# From FVS source repository
sudo find /tmp/ForestVegetationSimulator* -name "FVS_Data.db" 2>/dev/null | head -1

# Copy it to projects directory
sudo cp /tmp/ForestVegetationSimulator/data/FVS_Data.db \
  /srv/fvs/projects/FVS_Data.db.default

# Verify
ls -lh /srv/fvs/projects/FVS_Data.db.default
```

If the database isn't available in the source, it can be downloaded:

```bash
cd /srv/fvs/projects
sudo wget https://www.fs.fed.us/fvs/downloads/FVS_Data.db.default
```

### Copy Spatial Data (if available)

Some FVS installations include spatial data:

```bash
sudo find /tmp/ForestVegetationSimulator* -name "SpatialData.RData" 2>/dev/null | head -1

# Copy if found
sudo cp /tmp/ForestVegetationSimulator/data/SpatialData.RData.default \
  /srv/fvs/projects/
```

### Set Up Project Cleanup Cron Job

Create a script to automatically remove old projects:

```bash
sudo tee /usr/local/bin/fvs-project-cleanup.sh > /dev/null << 'EOF'
#!/bin/bash

# FVS-Online Project Cleanup Script
# Removes projects older than specified age

PROJECTS_DIR="/srv/fvs/projects"
MAX_AGE_DAYS=30
LOG_FILE="/var/log/fvs-cleanup.log"

# Function to log messages
log_message() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1" >> "$LOG_FILE"
}

log_message "Starting project cleanup (max age: ${MAX_AGE_DAYS} days)"

# Find and remove old directories
find "$PROJECTS_DIR" -maxdepth 1 -type d -mtime +${MAX_AGE_DAYS} ! -name "FVS_Data.db.default" ! -name "SpatialData*" | while read -r project_dir; do
    if [ -d "$project_dir" ]; then
        project_name=$(basename "$project_dir")
        log_message "Removing old project: $project_name"
        rm -rf "$project_dir"
    fi
done

log_message "Project cleanup completed"
EOF

sudo chmod 755 /usr/local/bin/fvs-project-cleanup.sh
```

Schedule the cleanup job:

```bash
# Create cron job (runs daily at 2:00 AM)
echo "0 2 * * * /usr/local/bin/fvs-project-cleanup.sh" | \
  sudo tee /etc/cron.d/fvs-cleanup > /dev/null

# Or use systemd timer (modern approach)
sudo tee /etc/systemd/system/fvs-cleanup.service > /dev/null << 'EOF'
[Unit]
Description=FVS-Online Project Cleanup
After=network.target

[Service]
Type=oneshot
ExecStart=/usr/local/bin/fvs-project-cleanup.sh
User=root
StandardOutput=journal
StandardError=journal
EOF

sudo tee /etc/systemd/system/fvs-cleanup.timer > /dev/null << 'EOF'
[Unit]
Description=Daily FVS-Online Project Cleanup
Requires=fvs-cleanup.service

[Timer]
OnCalendar=*-*-* 02:00:00
Persistent=true

[Install]
WantedBy=timers.target
EOF

# Enable the timer
sudo systemctl daemon-reload
sudo systemctl enable fvs-cleanup.timer
sudo systemctl start fvs-cleanup.timer

# Verify
sudo systemctl list-timers fvs-cleanup.timer
```

---

## 11. Security Hardening

### Firewall Configuration with ufw

Configure the firewall to allow only necessary traffic:

```bash
# Enable UFW
sudo ufw enable

# Allow SSH (adjust as needed for your network)
sudo ufw allow 22/tcp

# Allow HTTP and HTTPS
sudo ufw allow 80/tcp
sudo ufw allow 443/tcp

# Deny everything else by default (already set with 'enable')
sudo ufw default deny incoming
sudo ufw default allow outgoing

# Verify rules
sudo ufw status

# Optional: Allow only from specific networks
sudo ufw allow from 192.168.1.0/24 to any port 22 proto tcp
```

### RHEL/CentOS Firewall

If using firewalld:

```bash
# Start and enable
sudo systemctl enable firewalld
sudo systemctl start firewalld

# Add rules
sudo firewall-cmd --permanent --add-service=http
sudo firewall-cmd --permanent --add-service=https
sudo firewall-cmd --permanent --add-port=22/tcp
sudo firewall-cmd --reload

# Verify
sudo firewall-cmd --list-all
```

### File Upload Limits

Enforce reasonable upload limits in multiple places:

**nginx (already configured in Section 8):**
```
client_max_body_size 100m;
```

**Shiny Server (in shiny-server.conf):**
```
app_dir /srv/shiny-server/fvs-online;
```

**fvsol_config.yml:**
```yaml
projects:
  max_project_size_mb: 500
```

Enforce in R code within FVS-Online app:
```r
# In server.R
MAX_UPLOAD_SIZE <- fvsol_get_config("projects.max_project_size_mb", 500) * 1024 * 1024

# Validate user uploads
if (file.size(uploaded_file) > MAX_UPLOAD_SIZE) {
    showNotification("File size exceeds maximum allowed size", type = "error")
    return(NULL)
}
```

### Database Security

FVS uses SQLite for project data. Protect the database files:

```bash
# Set restrictive permissions on database
sudo chmod 644 /srv/fvs/projects/FVS_Data.db.default

# Regular users can read, shiny user can write via app logic
sudo chown shiny:shiny /srv/fvs/projects/*.db
```

**SQL Injection Prevention:** Ensure all database queries use parameterized statements:

```r
# GOOD: Parameterized query
query <- "SELECT * FROM projects WHERE project_id = ? AND user_id = ?"
result <- dbGetQuery(conn, query, list(project_id, user_id))

# BAD: String concatenation (avoid!)
query <- paste0("SELECT * FROM projects WHERE project_id = '", project_id, "'")
```

### HTTPS Enforcement

nginx configuration (Section 8) enforces HTTPS:

```
# Redirect HTTP to HTTPS
location / {
    return 301 https://$server_name$request_uri;
}
```

Add HSTS header to enforce HTTPS on client side:

```
add_header Strict-Transport-Security "max-age=31536000; includeSubDomains" always;
```

### Disable Unnecessary Services

Remove or disable unused services:

```bash
# Disable Apache if installed
sudo systemctl disable apache2 apache2.service 2>/dev/null || true

# Disable unused network services
sudo systemctl disable bluetooth 2>/dev/null || true
```

### System Logging

Ensure audit logging is enabled:

```bash
# Ubuntu 22.04
sudo apt install -y auditd
sudo systemctl enable auditd
sudo systemctl start auditd

# RHEL 8/9
sudo yum install -y audit
sudo systemctl enable auditd
sudo systemctl start auditd
```

### User Account Security

Restrict system accounts:

```bash
# Verify shiny user is restricted
grep shiny /etc/passwd
# Should show: shiny:x:XXX:XXX:Shiny User:/var/lib/shiny:/usr/sbin/nologin

# If not, fix it
sudo usermod -s /usr/sbin/nologin shiny
```

Disable root login via SSH:

```bash
sudo sed -i 's/^#PermitRootLogin.*/PermitRootLogin no/' /etc/ssh/sshd_config
sudo systemctl reload sshd
```

### Optional: Shiny Server Pro Authentication

If you need user authentication, consider Shiny Server Pro (commercial product):

```bash
# With Shiny Server Pro, configure authentication in shiny-server.conf:
location /fvs-online {
  auth_user_file /etc/shiny-server/.htpasswd;
}
```

**Temporary Alternative: nginx Basic Auth**

For quick authentication without Shiny Pro:

```bash
# Install apache2-utils for htpasswd
sudo apt install -y apache2-utils

# Create password file
sudo htpasswd -c /etc/nginx/.htpasswd admin
# Enter password when prompted

# Add to nginx config (in FVS location block):
auth_basic "FVS-Online Restricted";
auth_basic_user_file /etc/nginx/.htpasswd;
```

---

## 12. Testing the Deployment

### Test 1: Verify FVS Binaries Load

Test that the rFVS package can load compiled binaries:

```bash
# Run as shiny user to match production environment
sudo -u shiny R << 'EOF'
library(rFVS)

# Try loading a variant
result <- tryCatch({
    fvsLoad("FVSne")
    "SUCCESS"
}, error = function(e) {
    paste("FAILED:", e$message)
})

cat("FVS Binary Load Test:", result, "\n")
EOF
```

Expected output:
```
FVS Binary Load Test: SUCCESS
```

If you see a library loading error, verify:

1. FVS_LIB_PATH is set correctly
2. .so files exist in /srv/fvs/lib/
3. Dependencies are installed (ldd output)

### Test 2: Verify Configuration Loading

Test that the configuration system works:

```bash
cd /srv/shiny-server/fvs-online

sudo -u shiny R << 'EOF'
source("fvsol_config.R")

cat("Configuration Tests:\n")
cat("1. FVS_LIB_PATH:", FVS_LIB_PATH, "\n")
cat("2. PROJECTS_DIR:", PROJECTS_DIR, "\n")
cat("3. Config version:", fvsol_config$app$version, "\n")
cat("4. SMTP host:", fvsol_get_config("email.smtp.host"), "\n")
cat("\nAll configuration tests passed!\n")
EOF
```

### Test 3: Verify Database Access

Test that the application can access FVS database:

```bash
sudo -u shiny R << 'EOF'
library(RSQLite)

db_path <- "/srv/fvs/projects/FVS_Data.db.default"

conn <- tryCatch({
    dbConnect(SQLite(), db_path)
}, error = function(e) {
    cat("FAILED to connect:", e$message, "\n")
    return(NULL)
})

if (!is.null(conn)) {
    tables <- dbListTables(conn)
    cat("Database connection successful!\n")
    cat("Available tables:", paste(tables, collapse=", "), "\n")
    dbDisconnect(conn)
} else {
    cat("Database test FAILED\n")
}
EOF
```

### Test 4: Create and Run a Test Project

Create a simple test project through the web interface:

1. Open https://fvs.yourinstitution.edu/fvs-online/
2. Click "New Project"
3. Enter project name: "TEST_PROJECT"
4. Select a variant: "FVSne"
5. Upload a sample stand data file (if available)
6. Run simulation

**Monitor logs during the test:**

```bash
# Watch Shiny logs
sudo tail -f /var/log/shiny-server/fvs-online-access.log

# Watch nginx logs
sudo tail -f /var/log/nginx/fvs-online-access.log

# Watch email logs (if enabled)
sudo tail -f /var/log/msmtp.log
```

### Test 5: Verify Email Delivery

If email is configured:

```bash
# Find the test project directory
PROJECT_DIR=$(ls -td /srv/fvs/projects/*/ | head -1)

# Check if notification email was sent
echo "Checking email log..."
sudo grep "FVS_ONLINE\|TEST_PROJECT" /var/log/msmtp.log

# Check if email log exists
ls -lh /var/log/msmtp.log
```

### Test 6: HTTPS Certificate Verification

Verify SSL/TLS is working:

```bash
# Check certificate details
openssl s_client -connect fvs.yourinstitution.edu:443 -servername fvs.yourinstitution.edu < /dev/null

# Check certificate expiration
sudo certbot certificates

# Test certificate validation
curl -I https://fvs.yourinstitution.edu/health
```

### Test 7: WebSocket Connection

WebSockets are critical for Shiny to work properly:

```bash
# Test from command line (requires wscat or similar)
# Or use browser developer tools:
# 1. Open https://fvs.yourinstitution.edu/fvs-online/
# 2. Press F12 (Developer Tools)
# 3. Go to Network tab
# 4. Look for connections with "ws://" or "wss://" protocol
# 5. Connection should show "Status: 101 Switching Protocols"
```

If WebSocket fails:

1. Verify nginx `Upgrade` headers (see Section 8)
2. Check that `proxy_buffering off` is set
3. Verify no intermediate proxies are stripping WebSocket headers

### Test 8: Check Logs for Errors

Review all logs for any issues:

```bash
# Shiny application errors
sudo journalctl -u shiny-server -n 50

# nginx errors
sudo cat /var/log/nginx/fvs-online-error.log | tail -20

# System logs
sudo journalctl -n 50 --no-pager

# FVS-specific logs
ls -la /var/log/fvs-* 2>/dev/null
```

---

## 13. Maintenance

### Log Rotation

Prevent logs from consuming all disk space by rotating them.

**Ubuntu 22.04 (using logrotate):**

```bash
sudo tee /etc/logrotate.d/fvs-online > /dev/null << 'EOF'
/var/log/fvs-online/*.log {
    daily
    rotate 14
    compress
    delaycompress
    notifempty
    create 644 shiny shiny
    sharedscripts
    postrotate
        systemctl reload shiny-server > /dev/null 2>&1 || true
    endscript
}

/var/log/msmtp.log {
    weekly
    rotate 4
    compress
    delaycompress
    notifempty
    create 644 shiny shiny
}
EOF
```

Test logrotate:

```bash
sudo logrotate -f /etc/logrotate.d/fvs-online
sudo logrotate -f /etc/logrotate.d/fvs-online --debug
```

### Project Cleanup Cron Job

Already configured in Section 10. Monitor execution:

```bash
# Check cron job history
sudo journalctl -u cron -n 20

# For systemd timer:
sudo journalctl -u fvs-cleanup.service -n 20
```

### Updating FVS Binaries

When a new FVS release is available:

1. **Back up existing binaries:**
   ```bash
   sudo cp -r /srv/fvs/lib /srv/fvs/lib.backup.$(date +%Y%m%d)
   ```

2. **Build and install new binaries** (repeat Section 3)

3. **Test new binaries:**
   ```bash
   sudo -u shiny R -e "library(rFVS); fvsLoad('FVSne')"
   ```

4. **If successful, delete backup:**
   ```bash
   sudo rm -r /srv/fvs/lib.backup.20240115
   ```

5. **If not, restore backup:**
   ```bash
   sudo rm -r /srv/fvs/lib
   sudo mv /srv/fvs/lib.backup.20240115 /srv/fvs/lib
   ```

### Updating R Packages

Keep R packages current for security and bug fixes:

```bash
# Update CRAN packages
sudo R -e "update.packages(repos='https://cloud.r-project.org', ask=FALSE)"

# Update specific packages
sudo R -e "install.packages(c('shiny', 'rFVS', 'fvsOL'), repos='https://cloud.r-project.org')"

# Restart Shiny Server to load new versions
sudo systemctl restart shiny-server
```

### Updating FVS Configuration

To modify configuration for all instances:

```bash
# Edit configuration file
sudo nano /srv/shiny-server/fvs-online/fvsol_config.yml

# Reload Shiny Server
sudo systemctl reload shiny-server

# Or restart for immediate effect
sudo systemctl restart shiny-server
```

### Backup Strategy

Implement regular backups of critical data:

```bash
#!/bin/bash
# /usr/local/bin/fvs-backup.sh

BACKUP_DIR="/backup/fvs-online"
DATE=$(date +%Y%m%d_%H%M%S)

mkdir -p "$BACKUP_DIR"

# Backup configuration
tar -czf "$BACKUP_DIR/fvs-config_$DATE.tar.gz" \
    /srv/shiny-server/fvs-online/fvsol_config.yml \
    /etc/shiny-server/shiny-server.conf \
    /etc/nginx/sites-available/fvs-online.conf

# Backup FVS database (daily)
if [ "$(date +%H)" = "03" ]; then
    tar -czf "$BACKUP_DIR/fvs-database_$DATE.tar.gz" \
        /srv/fvs/projects/*.db*
fi

# Clean old backups (keep 30 days)
find "$BACKUP_DIR" -name "*.tar.gz" -mtime +30 -delete

echo "Backup completed: $BACKUP_DIR/fvs-config_$DATE.tar.gz"
```

Schedule backup:

```bash
# Add to crontab
0 3 * * * /usr/local/bin/fvs-backup.sh
```

### Monitoring Disk Space

Monitor disk usage, especially in projects directory:

```bash
# Current disk usage
df -h /srv/fvs/

# Directory sizes
du -sh /srv/fvs/projects/*

# Monitor over time (add to cron)
echo "$(date): $(du -sh /srv/fvs/projects)" >> /var/log/fvs-disk-usage.log
```

### Monitoring Service Health

Set up alerts for service failures:

```bash
#!/bin/bash
# /usr/local/bin/fvs-health-check.sh

# Check if services are running
services=("nginx" "shiny-server")

for service in "${services[@]}"; do
    if ! systemctl is-active --quiet "$service"; then
        echo "WARNING: $service is not running" | mail -s "FVS Service Alert" admin@yourinstitution.edu
        systemctl restart "$service"
    fi
done

# Check disk space
usage=$(df /srv/fvs | awk 'NR==2 {print $5}' | sed 's/%//')
if [ "$usage" -gt 80 ]; then
    echo "WARNING: Disk usage at ${usage}%" | mail -s "FVS Disk Alert" admin@yourinstitution.edu
fi
```

Schedule health checks:

```bash
# Every 5 minutes
*/5 * * * * /usr/local/bin/fvs-health-check.sh
```

---

## 14. Troubleshooting

### Issue 1: FVS Library Loading Fails

**Error Message:**
```
Error: cannot load shared object '/srv/fvs/lib/libFVS.so':
libgfortran.so.5: cannot open shared object file
```

**Solutions:**

1. Verify gfortran is installed:
   ```bash
   gfortran --version
   find /usr -name "libgfortran.so*"
   ```

2. Set LD_LIBRARY_PATH for Shiny:
   ```bash
   # Add to /etc/shiny-server/shiny-server.conf or via systemd
   export LD_LIBRARY_PATH=/srv/fvs/lib:/usr/lib/x86_64-linux-gnu:$LD_LIBRARY_PATH
   ```

3. Check file dependencies:
   ```bash
   ldd /srv/fvs/lib/libFVSne.so
   ```

### Issue 2: Permission Errors in Projects Directory

**Error Message:**
```
Error: Permission denied when accessing /srv/fvs/projects/...
```

**Solutions:**

1. Verify shiny user ownership:
   ```bash
   sudo chown -R shiny:shiny /srv/fvs/projects
   sudo chmod -R 755 /srv/fvs/projects
   ```

2. Check SELinux (RHEL only):
   ```bash
   sudo getenforce
   # If enforcing, adjust policies or set to permissive
   sudo setenforce permissive
   ```

3. Verify no immutable flags:
   ```bash
   lsattr /srv/fvs/projects/
   sudo chattr -i /srv/fvs/projects/*  # Remove immutable flag if set
   ```

### Issue 3: Email Not Sending

**Error Message:**
```
ERROR: Failed to send email notification
```

**Solutions:**

1. Test SMTP connectivity:
   ```bash
   # Using telnet or nc
   nc -v mail.yourinstitution.edu 25
   # Should show: 220 ESMTP ...
   ```

2. Check email configuration:
   ```bash
   sudo -u shiny cat /var/lib/shiny/.msmtprc
   cat /srv/shiny-server/fvs-online/fvsol_config.yml | grep -A5 email:
   ```

3. Send test email:
   ```bash
   echo "Test" | sudo -u shiny msmtp admin@yourinstitution.edu
   sudo tail -f /var/log/msmtp.log
   ```

4. Check firewall:
   ```bash
   sudo ufw allow out to any port 25
   sudo ufw allow out to any port 587
   ```

### Issue 4: WebSocket Connection Failures

**Error Message in Browser Console:**
```
WebSocket is closed before the connection is established
```

**Solutions:**

1. Verify nginx WebSocket headers:
   ```bash
   sudo grep -A10 "location /fvs-online" /etc/nginx/sites-available/fvs-online.conf
   # Should have:
   # proxy_set_header Upgrade $http_upgrade;
   # proxy_set_header Connection "upgrade";
   ```

2. Check nginx proxy buffering:
   ```bash
   grep "proxy_buffering" /etc/nginx/sites-available/fvs-online.conf
   # Should be: proxy_buffering off;
   ```

3. Check for intermediate proxies:
   - If behind additional proxy/load balancer, ensure WebSocket support is enabled there
   - Add headers: `Connection: upgrade` and `Upgrade: websocket`

4. Verify Shiny Server is running:
   ```bash
   sudo systemctl status shiny-server
   curl -I http://localhost:3838/fvs-online/
   ```

### Issue 5: Slow Simulation Execution

**Problem:** Simulations run very slowly

**Solutions:**

1. Verify CPU usage:
   ```bash
   top
   htop  # More detailed view
   ```

2. Check for I/O bottleneck:
   ```bash
   iostat -x 1
   ```

3. Increase parallel processing:
   ```yaml
   # In fvsol_config.yml
   performance:
     parallel:
       enabled: true
       cores: 8  # Increase if available
   ```

4. Check system load:
   ```bash
   uptime
   nproc  # Number of CPUs
   free -h  # Memory availability
   ```

5. Monitor Shiny process:
   ```bash
   ps aux | grep "Rscript.*shiny-server"
   ```

### Issue 6: High Memory Usage

**Problem:** Shiny Server process uses excessive memory

**Solutions:**

1. Check memory usage:
   ```bash
   ps aux | grep shiny
   free -h
   ```

2. Restart Shiny Server to free memory:
   ```bash
   sudo systemctl restart shiny-server
   ```

3. Reduce max concurrent simulations:
   ```yaml
   performance:
     max_concurrent_sims: 2  # Reduce from 4
   ```

4. Monitor memory over time:
   ```bash
   watch -n 5 'free -h'
   ```

### Issue 7: HTTPS Certificate Errors

**Error Message in Browser:**
```
SSL_ERROR_RX_RECORD_TOO_LONG or certificate not found
```

**Solutions:**

1. Verify certificate exists:
   ```bash
   sudo certbot certificates
   ls -la /etc/letsencrypt/live/fvs.yourinstitution.edu/
   ```

2. Check nginx configuration:
   ```bash
   sudo nginx -t
   grep ssl_certificate /etc/nginx/sites-available/fvs-online.conf
   ```

3. Renew certificate if expired:
   ```bash
   sudo certbot renew
   sudo systemctl reload nginx
   ```

4. Check certificate validity:
   ```bash
   echo | openssl s_client -servername fvs.yourinstitution.edu -connect localhost:443 2>/dev/null | openssl x509 -noout -dates
   ```

### Issue 8: Application Won't Start

**Error Message:**
```
Shiny application failed to start
```

**Solutions:**

1. Check Shiny logs:
   ```bash
   sudo tail -50 /var/log/shiny-server/fvs-online-access.log
   sudo journalctl -u shiny-server -n 100
   ```

2. Verify configuration loads:
   ```bash
   cd /srv/shiny-server/fvs-online
   sudo -u shiny R -e "source('fvsol_config.R')"
   ```

3. Test individual package loading:
   ```bash
   sudo -u shiny R << 'EOF'
   library(shiny)
   library(rFVS)
   library(fvsOL)
   cat("Packages loaded successfully\n")
   EOF
   ```

4. Check for R syntax errors:
   ```bash
   cd /srv/shiny-server/fvs-online
   sudo -u shiny R CMD check .
   ```

### Issue 9: Database Lock Errors

**Error Message:**
```
Error: database is locked
```

**Solutions:**

1. Verify database is not corrupted:
   ```bash
   sqlite3 /srv/fvs/projects/FVS_Data.db.default "PRAGMA integrity_check;"
   ```

2. Close any open connections:
   ```bash
   sudo systemctl restart shiny-server
   ```

3. Check for .wal files (WAL mode):
   ```bash
   ls -la /srv/fvs/projects/FVS_Data.db*
   # May include .db-shm and .db-wal files
   ```

4. Increase SQLite busy timeout:
   ```r
   # In server.R
   dbExecute(conn, "PRAGMA busy_timeout = 10000")  # 10 seconds
   ```

### Issue 10: File Upload Failures

**Error Message:**
```
Error: Upload failed - file exceeds size limit
```

**Solutions:**

1. Check configured size limits:
   ```bash
   grep client_max_body_size /etc/nginx/sites-available/fvs-online.conf
   grep max_project_size /srv/shiny-server/fvs-online/fvsol_config.yml
   ```

2. Increase limits if needed:
   ```bash
   # nginx
   sudo sed -i 's/client_max_body_size.*/client_max_body_size 500m;/' /etc/nginx/sites-available/fvs-online.conf

   # fvsol_config.yml
   sudo sed -i 's/max_project_size_mb:.*/max_project_size_mb: 1000/' /srv/shiny-server/fvs-online/fvsol_config.yml

   sudo systemctl reload nginx
   sudo systemctl restart shiny-server
   ```

3. Check available disk space:
   ```bash
   df -h /srv/fvs/
   ```

### Log File Locations

For troubleshooting, check these log files:

| Component | Log File | View Command |
|-----------|----------|--------------|
| Shiny App | `/var/log/shiny-server/fvs-online-access.log` | `sudo tail -f /var/log/shiny-server/fvs-online-access.log` |
| Shiny Server | journalctl | `sudo journalctl -u shiny-server -n 50` |
| nginx Web Server | `/var/log/nginx/fvs-online-error.log` | `sudo tail -f /var/log/nginx/fvs-online-error.log` |
| nginx Access | `/var/log/nginx/fvs-online-access.log` | `sudo tail -f /var/log/nginx/fvs-online-access.log` |
| Email (msmtp) | `/var/log/msmtp.log` | `sudo tail -f /var/log/msmtp.log` |
| System | journalctl | `sudo journalctl -n 50 --no-pager` |
| FVS Cleanup | journalctl | `sudo journalctl -u fvs-cleanup.service -n 20` |

---

## Appendix A: Quick Reference Commands

### Start/Stop Services

```bash
# Start all services
sudo systemctl start nginx shiny-server

# Stop all services
sudo systemctl stop nginx shiny-server

# Restart services
sudo systemctl restart nginx shiny-server

# Enable on boot
sudo systemctl enable nginx shiny-server

# Check status
sudo systemctl status nginx shiny-server
```

### Test Configuration

```bash
# Test nginx configuration
sudo nginx -t

# Reload nginx without stopping
sudo systemctl reload nginx

# Test R configuration loading
cd /srv/shiny-server/fvs-online && sudo -u shiny R -e "source('fvsol_config.R')"
```

### View Logs

```bash
# Real-time log monitoring
sudo tail -f /var/log/nginx/fvs-online-error.log
sudo tail -f /var/log/shiny-server/fvs-online-access.log

# Last 100 lines
sudo tail -100 /var/log/nginx/fvs-online-error.log

# Search logs
sudo grep "ERROR" /var/log/shiny-server/fvs-online-access.log
```

### Database Operations

```bash
# Check database integrity
sqlite3 /srv/fvs/projects/FVS_Data.db.default "PRAGMA integrity_check;"

# Backup database
cp /srv/fvs/projects/FVS_Data.db.default /srv/fvs/projects/FVS_Data.db.backup

# List database tables
sqlite3 /srv/fvs/projects/FVS_Data.db.default ".tables"
```

### Certificate Management

```bash
# View certificate details
sudo certbot certificates

# Renew certificate
sudo certbot renew

# Test auto-renewal
sudo certbot renew --dry-run

# Check certificate expiration date
echo | openssl s_client -servername fvs.yourinstitution.edu -connect localhost:443 2>/dev/null | openssl x509 -noout -dates
```

---

## Appendix B: Deployment Checklist

Use this checklist to verify your FVS-Online deployment:

### Pre-Deployment

- [ ] Ubuntu 22.04 LTS or RHEL 8/9 installed
- [ ] 4+ CPU cores available
- [ ] 8+ GB RAM available
- [ ] 100+ GB SSD storage available
- [ ] Root or sudo access available
- [ ] Domain name registered and DNS configured
- [ ] SMTP relay endpoint identified
- [ ] Network ports 80, 443 open for internet traffic

### System Dependencies

- [ ] System updated: `sudo apt update && sudo apt upgrade -y`
- [ ] gcc, gfortran, cmake installed
- [ ] R 4.0+ installed and verified
- [ ] System libraries installed (libsqlite3, libxml2, etc.)
- [ ] Email tool installed (msmtp or ssmtp)

### FVS Build

- [ ] FVS source cloned from GitHub
- [ ] CMake build completed successfully
- [ ] FVS binaries verified in /srv/fvs/lib/
- [ ] FVS permissions set correctly (755)

### Interface Components

- [ ] rFVS package installed and verified
- [ ] fvsOL package installed and verified
- [ ] All R dependencies installed (shiny, RSQLite, ggplot2, etc.)
- [ ] FVSPrjBldr copied to /srv/shiny-server/fvs-online/

### Configuration

- [ ] fvsol_config.yml created with deployment settings
- [ ] fvsol_config.R created and placed in FVSPrjBldr/
- [ ] Configuration tested: `source('fvsol_config.R')`
- [ ] Environment variables verified (FVSOL_*)

### Patches Applied

- [ ] server.R patched with configuration initialization
- [ ] prjListEmail.R updated with config-driven SMTP
- [ ] ui.R updated with configuration-driven options
- [ ] All patches verified to work

### Shiny Server

- [ ] Shiny Server installed
- [ ] shiny-server.conf configured
- [ ] Required directories created (/srv/shiny-server/fvs-online/, /var/log/shiny-server/)
- [ ] Directory permissions set (shiny user)
- [ ] Shiny Server started and enabled
- [ ] Shiny Server listens on port 3838

### nginx Reverse Proxy

- [ ] nginx installed
- [ ] fvs-online.conf created in /etc/nginx/sites-available/
- [ ] Site enabled: `/etc/nginx/sites-enabled/fvs-online.conf`
- [ ] nginx configuration tested: `sudo nginx -t`
- [ ] nginx reloaded

### SSL/HTTPS

- [ ] certbot installed
- [ ] SSL certificate obtained (Let's Encrypt)
- [ ] Certificate paths correct in nginx config
- [ ] HTTPS working: https://fvs.yourinstitution.edu/fvs-online/
- [ ] HTTP redirects to HTTPS

### Email

- [ ] SMTP tool configured (msmtp or ssmtp)
- [ ] SMTP credentials configured
- [ ] Test email sent and received
- [ ] Email configuration in fvsol_config.yml

### Projects & Data

- [ ] /srv/fvs/projects/ created with correct permissions
- [ ] FVS_Data.db.default copied to projects directory
- [ ] Cleanup cron job configured
- [ ] Cleanup script tested

### Security

- [ ] Firewall configured (ufw or firewalld)
- [ ] Only ports 22, 80, 443 open to internet
- [ ] SSH key-based auth configured
- [ ] Root login disabled
- [ ] File upload limits configured
- [ ] Database permissions restricted

### Testing

- [ ] FVS binary load test passed
- [ ] Configuration load test passed
- [ ] Database connectivity test passed
- [ ] Test project created and simulation run
- [ ] Email delivery tested
- [ ] WebSocket connection verified
- [ ] HTTPS certificate verified
- [ ] All logs checked for errors

### Monitoring & Maintenance

- [ ] Log rotation configured
- [ ] Disk usage monitoring set up
- [ ] Service health checks configured
- [ ] Backup strategy implemented
- [ ] Certificate auto-renewal tested
- [ ] R package update schedule planned
- [ ] FVS binary update procedure documented

---

## Appendix C: Additional Resources

### Official Documentation

* FVS Documentation: https://www.fs.fed.us/fvs/
* R Shiny Documentation: https://shiny.posit.co/r/
* nginx Documentation: https://nginx.org/en/docs/
* Let's Encrypt Documentation: https://letsencrypt.org/docs/

### Useful Tools and Commands

* **System Monitoring:** htop, top, iotop, iftop
* **Log Analysis:** tail, grep, journalctl, logwatch
* **Network Testing:** netstat, ss, nc, traceroute
* **Database Tools:** sqlite3, db-shell
* **Performance:** perf, flamegraph, bench

### Common SMTP Configuration

| Provider | Host | Port | TLS/SSL |
|----------|------|------|---------|
| Institutional SMTP | mail.yourinstitution.edu | 25 | None |
| Gmail | smtp.gmail.com | 587 | STARTTLS |
| Office 365 | smtp.office365.com | 587 | STARTTLS |

### Ubuntu vs RHEL Command Reference

| Task | Ubuntu | RHEL |
|------|--------|------|
| Install package | apt install | yum install |
| Update system | apt update && apt upgrade | yum update |
| List packages | apt list | yum list |
| Start service | systemctl start | systemctl start |
| Edit crontab | crontab -e | crontab -e |
| Check firewall | ufw status | firewall-cmd --list-all |

---

## Support and Contact

For questions or issues with FVS-Online:

* FVS Support: https://www.fs.fed.us/fvs/
* USDAForestService GitHub: https://github.com/USDAForestService/
* R Shiny Community: https://community.rstudio.com/

---

**Document Version:** 1.0
**Last Updated:** March 2026
**Maintained By:** [Your Organization]
