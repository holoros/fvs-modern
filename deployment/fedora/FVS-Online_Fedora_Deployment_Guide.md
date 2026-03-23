# FVS-Online Local Deployment Guide for Fedora

**Version:** 1.0
**Date:** March 22, 2026
**Target OS:** Fedora 39/40/41 (workstation or server)
**Author:** Aaron Weiskittel, University of Maine

This guide walks through deploying a fully working local instance of the Forest Vegetation Simulator (FVS) and its web interface (FVS-Online) on a Fedora Linux system. It covers compiling the Fortran shared libraries from source, installing the R interface packages, configuring the Shiny applications, and optionally exposing the service through nginx for network access.

## Table of Contents

1. Prerequisites and System Packages
2. Directory Structure
3. Build FVS Fortran Shared Libraries
4. Install R and Required Packages
5. Install rFVS Package
6. Install fvsOL Package
7. Configure FVS-Online
8. Launch FVS-Online Locally
9. Set Up FVS Project Builder
10. Set Up FVS Data Converter
11. Optional: nginx Reverse Proxy for Network Access
12. Optional: systemd Services for Persistent Operation
13. Optional: SSL/TLS with Let's Encrypt
14. Automated Project Cleanup
15. Troubleshooting
16. Architecture Reference

---

## 1. Prerequisites and System Packages

### 1.1 Enable RPM Fusion (if not already enabled)

```bash
sudo dnf install https://mirrors.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm
```

### 1.2 Install System Dependencies

```bash
# Fortran compiler and build tools
sudo dnf install gcc-gfortran gcc gcc-c++ make cmake

# R statistical computing
sudo dnf install R R-devel

# Libraries needed by R packages
sudo dnf install \
    cairo-devel \
    libcurl-devel \
    openssl-devel \
    libxml2-devel \
    sqlite-devel \
    mesa-libGLU-devel \
    libXt-devel \
    libX11-devel \
    freetype-devel \
    harfbuzz-devel \
    fribidi-devel \
    libtiff-devel \
    libjpeg-turbo-devel \
    libpng-devel \
    udunits2-devel \
    gdal-devel \
    geos-devel \
    proj-devel

# Database tools (for Access .mdb/.accdb file import)
sudo dnf install mdbtools sqlite

# Email support (optional, for project builder email notifications)
sudo dnf install mailx

# Nginx (optional, for reverse proxy)
sudo dnf install nginx
```

### 1.3 Verify Installations

```bash
gfortran --version
R --version | head -3
mdb-schema --help 2>&1 | head -1
sqlite3 --version
```

All four should return version information without errors.

---

## 2. Directory Structure

Create the working directory layout. This guide uses `/srv/fvs/` as the root, but you can substitute any location (e.g., `~/fvs-local/` for a personal installation).

```bash
# For system-wide installation
sudo mkdir -p /srv/fvs/{bin,config,projects,interface,patches,R-libs,www,logs}
sudo chown -R $USER:$USER /srv/fvs

# Or for personal installation (no sudo needed)
mkdir -p ~/fvs-local/{bin,config,projects,interface,patches,R-libs,www,logs}
```

Throughout this guide, replace `/srv/fvs` with your chosen root if different.

```
/srv/fvs/
  bin/              FVS Fortran shared libraries (FVSne.so, FVSak.so, etc.)
  config/           Configuration files (fvsol_config.yml, fvsol_config.R)
  projects/         User project directories (created by Project Builder)
  interface/        FVS-Online Interface source (fvsOL, FVSPrjBldr, FVSDataConvert, rFVS)
  patches/          Drop-in R patches for configurable deployment
  R-libs/           R package library (isolated from system R)
  www/              Static web content (landing page, etc.)
  logs/             Application log files
```

---

## 3. Build FVS Fortran Shared Libraries

The FVS Fortran source must be compiled into shared libraries (`.so` files) that R can load at runtime via the rFVS package. Each regional variant produces one shared library (e.g., `FVSne.so` for the Northeast variant).

### 3.1 Clone or Copy the FVS Source

```bash
# If you have a local copy:
cp -r /path/to/ForestVegetationSimulator-main ~/fvs-source

# Or clone from GitHub:
git clone https://github.com/USDAForestService/ForestVegetationSimulator.git ~/fvs-source
```

### 3.2 Build Using the Included Makefile

The FVS repository includes a makefile that handles Linux builds. This is the most reliable approach:

```bash
cd ~/fvs-source/bin

# Build all US variants (takes 10 to 20 minutes depending on hardware)
make all CC=gcc FC=gfortran

# Or build specific variants only:
make FVSne FVSak FVSls
```

After building, copy the shared libraries to your FVS bin directory:

```bash
cp ~/fvs-source/FVS*.so /srv/fvs/bin/
ls -lh /srv/fvs/bin/*.so
```

You should see files like `FVSne.so`, `FVSak.so`, `FVSie.so`, etc.

### 3.3 Alternative: Use the fvs-modern Build Script

If the makefile does not work (e.g., missing NVEL submodule or other issues), the fvs-modern package includes an alternative build script:

```bash
chmod +x /path/to/fvs-modern/deployment/scripts/build_fvs_libraries.sh
/path/to/fvs-modern/deployment/scripts/build_fvs_libraries.sh \
    ~/fvs-source \
    /srv/fvs/bin \
    ne ak ie ls
```

### 3.4 Verify Libraries

```bash
# Check that the libraries contain the expected Fortran symbols
nm -D /srv/fvs/bin/FVSne.so | grep -i "fvs$"
nm -D /srv/fvs/bin/FVSne.so | grep -i "cfvssetcmdline"
```

Both commands should return at least one symbol. If `nm` shows no output, the library was not compiled correctly.

---

## 4. Install R and Required Packages

### 4.1 Set Up a Dedicated R Library

Using a dedicated library avoids conflicts with system R packages:

```bash
export R_LIBS_USER=/srv/fvs/R-libs
echo 'R_LIBS_USER=/srv/fvs/R-libs' >> ~/.Renviron
```

### 4.2 Install R Package Dependencies

Start R and install the packages required by fvsOL:

```bash
R --no-save << 'REOF'
# Set library path
.libPaths("/srv/fvs/R-libs")

# Install from CRAN
install.packages(c(
    "shiny",
    "Cairo",
    "rhandsontable",
    "ggplot2",
    "RSQLite",
    "plyr",
    "dplyr",
    "colourpicker",
    "rgl",
    "leaflet",
    "zip",
    "openxlsx",
    "shinyFiles",
    "nlme",
    "yaml",
    "devtools"
), repos = "https://cloud.r-project.org", lib = "/srv/fvs/R-libs")

# Verify all packages load
pkgs <- c("shiny", "Cairo", "rhandsontable", "ggplot2", "RSQLite",
          "plyr", "dplyr", "colourpicker", "rgl", "leaflet", "zip",
          "openxlsx", "shinyFiles", "yaml")
for (p in pkgs) {
    if (require(p, character.only = TRUE, quietly = TRUE)) {
        cat(sprintf("  %s: OK\n", p))
    } else {
        cat(sprintf("  %s: FAILED\n", p))
    }
}
REOF
```

This takes roughly 15 to 30 minutes on first install due to compilation of packages with C/C++ components.

---

## 5. Install rFVS Package

The rFVS package provides the R-to-Fortran interface that allows R to call FVS simulation routines.

### 5.1 Copy Interface Source

```bash
cp -r /path/to/ForestVegetationSimulator-Interface-main/* /srv/fvs/interface/
```

### 5.2 Generate NAMESPACE (if missing)

The rFVS package in the repository may lack a NAMESPACE file. Create one:

```bash
cat > /srv/fvs/interface/rFVS/NAMESPACE << 'EOF'
export(fvsLoad, fvsRun, fvsInteractRun, fvsSetCmdLine)
export(fvsAddActivity, fvsAddTrees, fvsCutNow)
export(fvsGetDims, fvsGetRestartcode, fvsGetSummary)
export(fvsGetTreeAttrs, fvsSetTreeAttr)
export(fvsGetSpeciesAttrs, fvsSetSpeciesAttr)
export(fvsGetSpeciesCodes, fvsGetStandIDs)
export(fvsGetEventMonitorVariables)
export(fvsGetUnitConversion)
export(fvsGetSVSDims, fvsGetSVSObjectSet)
export(fvsSetupSummary, fvsCompositeSum)
export(fvsMakeyFile)
EOF
```

### 5.3 Install rFVS

```bash
R CMD INSTALL --library=/srv/fvs/R-libs /srv/fvs/interface/rFVS
```

### 5.4 Test rFVS

```bash
R --no-save << 'REOF'
.libPaths("/srv/fvs/R-libs")
library(rFVS)

# Test loading a variant library
result <- tryCatch({
    fvsLoad("FVSne", bin = "/srv/fvs/bin")
    cat("rFVS loaded FVSne.so successfully!\n")
    TRUE
}, error = function(e) {
    cat(sprintf("Error: %s\n", e$message))
    FALSE
})

if (result) {
    cat("rFVS is working correctly.\n")
} else {
    cat("Check that FVSne.so exists in /srv/fvs/bin/ and contains the expected symbols.\n")
}
REOF
```

---

## 6. Install fvsOL Package

### 6.1 Generate NAMESPACE (if missing)

```bash
cat > /srv/fvs/interface/fvsOL/NAMESPACE << 'EOF'
export(fvsOL)
import(shiny)
import(rFVS)
import(Cairo)
import(rhandsontable)
import(ggplot2)
import(RSQLite)
import(plyr)
import(dplyr)
import(colourpicker)
import(rgl)
import(leaflet)
import(zip)
import(openxlsx)
import(shinyFiles)
EOF
```

### 6.2 Install fvsOL

```bash
R CMD INSTALL --library=/srv/fvs/R-libs /srv/fvs/interface/fvsOL
```

If R CMD INSTALL fails with NAMESPACE errors, you can also source the package directly (see Section 8.2 for the direct-source approach).

---

## 7. Configure FVS-Online

### 7.1 Copy Configuration Files

```bash
cp /path/to/fvs-modern/deployment/config/fvsol_config.yml /srv/fvs/config/
cp /path/to/fvs-modern/deployment/config/fvsol_config.R /srv/fvs/config/
```

### 7.2 Edit Configuration

Open `/srv/fvs/config/fvsol_config.yml` and update these values:

```yaml
server:
    base_url: "http://localhost:3838/FVSwork"
    institution: "University of Maine"
    department: "Center for Research on Sustainable Forests"

paths:
    work_dir: "/srv/fvs/projects"
    fvs_bin: "/srv/fvs/bin"

email:
    send_command: "mail -s"
    subject_prefix: "FVS-Online project"

projects:
    retention_days: 60

limits:
    max_upload_mb: 500
```

For a purely local installation (no network access), the `server.base_url` can remain localhost. For a server accessible to others, set it to the machine's hostname or IP.

### 7.3 Copy Deployment Patches

```bash
cp /path/to/fvs-modern/deployment/patches/*.R /srv/fvs/patches/
```

These patches replace the Virginia Tech hardcoded paths with configurable values from `fvsol_config.yml`.

---

## 8. Launch FVS-Online Locally

### 8.1 Option A: Package-Based Launch (if R CMD INSTALL succeeded)

```bash
export R_LIBS_USER=/srv/fvs/R-libs
export FVSOL_CONFIG_FILE=/srv/fvs/config/fvsol_config.yml

cd /srv/fvs/projects

R --no-save << 'REOF'
.libPaths("/srv/fvs/R-libs")
source("/srv/fvs/config/fvsol_config.R")

library(rFVS)
library(fvsOL)

options(shiny.port = 3838L, shiny.host = "127.0.0.1")
fvsOL(fvsBin = cfg("paths", "fvs_bin"), logToConsole = TRUE)
REOF
```

Then open a browser to: **http://localhost:3838**

### 8.2 Option B: Direct-Source Launch (no package install needed)

If R CMD INSTALL had issues, you can source the fvsOL code directly:

```bash
export R_LIBS_USER=/srv/fvs/R-libs
export FVSOL_CONFIG_FILE=/srv/fvs/config/fvsol_config.yml

R --no-save << 'REOF'
.libPaths("/srv/fvs/R-libs")
source("/srv/fvs/config/fvsol_config.R")

# Source rFVS functions directly
for (f in list.files("/srv/fvs/interface/rFVS/R", pattern = "\\.R$", full.names = TRUE)) {
    source(f)
}

# Source fvsOL functions directly
source("/srv/fvs/interface/fvsOL/R/server.R")

# Set working directory and launch
setwd("/srv/fvs/projects")
options(shiny.port = 3838L, shiny.host = "127.0.0.1")
fvsOL(fvsBin = cfg("paths", "fvs_bin"), logToConsole = TRUE)
REOF
```

### 8.3 Verify FVS-Online Is Running

1. Open a browser and navigate to **http://localhost:3838**
2. You should see the FVS-Online interface with the USDA Forest Service logo
3. Try creating a new project and loading the default database
4. Select a variant (e.g., NE for Northeast) and run a test simulation

If the page does not load, check the R console output for error messages.

---

## 9. Set Up FVS Project Builder

The Project Builder is a separate Shiny app that creates new FVS-Online project directories and sends email notifications with project links.

### 9.1 Patch the Project Builder

The original Project Builder has Virginia Tech paths hardcoded. Use the patched version:

```bash
# Copy the launcher script
cp /path/to/fvs-modern/deployment/scripts/launch_prjbldr.R /srv/fvs/

# Or create a minimal launcher:
cat > /srv/fvs/launch_prjbldr.R << 'REOF'
.libPaths("/srv/fvs/R-libs")
source("/srv/fvs/config/fvsol_config.R")

library(shiny)

work_dir <- cfg("paths", "work_dir")
fvs_bin  <- cfg("paths", "fvs_bin")
base_url <- cfg("server", "base_url")

# Source UI from original, server from patched version
source("/srv/fvs/patches/FVSPrjBldr_server.R")
source("/srv/fvs/interface/FVSPrjBldr/ui.R")
source("/srv/fvs/interface/FVSPrjBldr/uuidgen.R")

options(shiny.port = 3839L, shiny.host = "127.0.0.1")
shinyApp(ui = FVSPrjBldr_ui, server = FVSPrjBldr_server)
REOF
```

### 9.2 Launch Project Builder

In a separate terminal:

```bash
export R_LIBS_USER=/srv/fvs/R-libs
export FVSOL_CONFIG_FILE=/srv/fvs/config/fvsol_config.yml
Rscript /srv/fvs/launch_prjbldr.R
```

Access at: **http://localhost:3839**

---

## 10. Set Up FVS Data Converter

The Data Converter handles importing Access databases (.mdb/.accdb) and Excel files into FVS-compatible SQLite databases.

### 10.1 Launch Data Converter

```bash
cat > /srv/fvs/launch_dataconvert.R << 'REOF'
.libPaths("/srv/fvs/R-libs")

library(shiny)

# Source the data converter
source("/srv/fvs/interface/FVSDataConvert/server.R")
source("/srv/fvs/interface/FVSDataConvert/ui.R")

options(shiny.port = 3840L, shiny.host = "127.0.0.1")
shinyApp(ui = ui, server = server)
REOF

Rscript /srv/fvs/launch_dataconvert.R
```

Access at: **http://localhost:3840**

### 10.2 Verify mdbtools Works

```bash
# Test with a sample Access database (if available)
mdb-tables /path/to/sample.mdb
```

If mdbtools is not available, Access database imports will fail. Excel (.xlsx) imports work independently through the openxlsx R package.

---

## 11. Optional: nginx Reverse Proxy for Network Access

If you want others on your network (or the internet) to access FVS-Online, set up nginx as a reverse proxy.

### 11.1 Install and Configure nginx

```bash
# Copy the provided nginx configuration
sudo cp /path/to/fvs-modern/deployment/services/nginx-fvsonline.conf /etc/nginx/conf.d/fvsonline.conf

# Edit server_name to match your hostname
sudo sed -i 's/server_name _;/server_name your.server.edu;/' /etc/nginx/conf.d/fvsonline.conf

# Test configuration
sudo nginx -t

# Start and enable nginx
sudo systemctl start nginx
sudo systemctl enable nginx
```

### 11.2 Open Firewall Ports

```bash
sudo firewall-cmd --permanent --add-service=http
sudo firewall-cmd --permanent --add-service=https
sudo firewall-cmd --reload
```

### 11.3 SELinux Configuration (Fedora-specific)

Fedora enables SELinux by default, which blocks nginx from making network connections to the Shiny backend:

```bash
# Allow nginx to connect to upstream Shiny processes
sudo setsebool -P httpd_can_network_connect 1

# If serving files from /srv/fvs/www, set the correct context
sudo semanage fcontext -a -t httpd_sys_content_t "/srv/fvs/www(/.*)?"
sudo restorecon -Rv /srv/fvs/www
```

### 11.4 Test

From another machine on the network, navigate to `http://your.server.edu/FVSPrjBldr/` to access the Project Builder.

---

## 12. Optional: systemd Services for Persistent Operation

For a server that should run FVS-Online continuously (surviving reboots and crashes):

### 12.1 Create a Service User

```bash
sudo useradd -r -s /sbin/nologin -d /srv/fvs fvs
sudo chown -R fvs:fvs /srv/fvs
sudo mkdir -p /var/log/fvs
sudo chown fvs:fvs /var/log/fvs
```

### 12.2 Install Service Files

```bash
sudo cp /path/to/fvs-modern/deployment/services/fvsonline.service /etc/systemd/system/
sudo cp /path/to/fvs-modern/deployment/services/fvsprjbldr.service /etc/systemd/system/

sudo systemctl daemon-reload
```

### 12.3 Start and Enable Services

```bash
sudo systemctl start fvsonline
sudo systemctl start fvsprjbldr
sudo systemctl enable fvsonline
sudo systemctl enable fvsprjbldr
```

### 12.4 Check Status

```bash
sudo systemctl status fvsonline
sudo journalctl -u fvsonline -f    # Follow live logs
```

---

## 13. Optional: SSL/TLS with Let's Encrypt

For public-facing deployments, HTTPS is essential. Use certbot with Let's Encrypt:

```bash
sudo dnf install certbot python3-certbot-nginx

# Obtain certificate (replace with your domain)
sudo certbot --nginx -d your.server.edu

# Auto-renewal is configured automatically by certbot
sudo systemctl enable certbot-renew.timer
```

After certbot runs, it updates the nginx configuration to redirect HTTP to HTTPS and adds the SSL certificate paths.

---

## 14. Automated Project Cleanup

FVS-Online projects accumulate over time. Set up a cron job to remove projects older than the retention period:

```bash
cat > /srv/fvs/cleanup_projects.sh << 'SCRIPT'
#!/bin/bash
# Remove FVS projects not accessed in the last N days
RETENTION_DAYS=${1:-60}
PROJECTS_DIR="/srv/fvs/projects"

find "$PROJECTS_DIR" -maxdepth 1 -mindepth 1 -type d -atime +${RETENTION_DAYS} -exec rm -rf {} \;
echo "$(date): Cleaned projects older than $RETENTION_DAYS days" >> /srv/fvs/logs/cleanup.log
SCRIPT

chmod +x /srv/fvs/cleanup_projects.sh

# Add to crontab (runs daily at 3 AM)
(crontab -l 2>/dev/null; echo "0 3 * * * /srv/fvs/cleanup_projects.sh 60") | crontab -
```

---

## 15. Troubleshooting

### 15.1 "Cannot open shared object file"

**Symptom:** R reports `cannot open shared object file: No such file or directory` when loading an FVS variant.

**Cause:** The `.so` file is missing or not in the expected path.

**Fix:**
```bash
# Verify the library exists
ls -la /srv/fvs/bin/FVSne.so

# If it exists but R cannot find it, check the path passed to fvsLoad()
# Also check for missing dependencies:
ldd /srv/fvs/bin/FVSne.so | grep "not found"
```

### 15.2 "Routine not found" After fvsLoad()

**Symptom:** fvsLoad() complains that routines like `CfvsSetCmdLine` are not found.

**Cause:** The shared library was compiled without the C interface wrapper functions (apisubsc.c) or without exporting all symbols.

**Fix:** Ensure the build includes `apisubsc.c` and uses `-Wl,--export-dynamic` or `--export-all-symbols` during linking.

### 15.3 Shiny App Displays Blank Page

**Symptom:** Browser shows a blank page or connection refused.

**Fix:**
```bash
# Check that R is actually listening
ss -tlnp | grep 3838

# Check R console output for errors

# Common issue: port already in use
sudo fuser -k 3838/tcp    # Kill any process on port 3838
```

### 15.4 "Permission denied" on /srv/fvs

**Symptom:** SELinux or file permissions prevent the fvs user from writing.

**Fix:**
```bash
# Check SELinux denials
sudo ausearch -m AVC -ts recent

# Fix file ownership
sudo chown -R fvs:fvs /srv/fvs

# If SELinux is blocking, set permissive mode temporarily to diagnose:
sudo setenforce 0    # Temporary; reverts on reboot
# Then set proper contexts once you know what is needed
```

### 15.5 R Package Installation Fails

**Symptom:** `install.packages()` fails with compilation errors.

**Fix:**
```bash
# Install development libraries
sudo dnf install cairo-devel libcurl-devel openssl-devel libxml2-devel

# For rgl specifically:
sudo dnf install mesa-libGLU-devel libX11-devel libXt-devel

# Retry installation in R
```

### 15.6 Access Database Import Fails

**Symptom:** FVSDataConvert cannot read .mdb or .accdb files.

**Fix:**
```bash
# Verify mdbtools is installed
which mdb-schema
which mdb-export

# If not:
sudo dnf install mdbtools
```

### 15.7 Email Notifications Not Sending

**Symptom:** Project Builder does not send email links.

**Fix:** For local testing, email is not essential. For production, configure `mailx` or `ssmtp`:
```bash
# Test mail from command line
echo "Test" | mail -s "FVS Test" your@email.com

# If mail command not found:
sudo dnf install mailx
```

---

## 16. Architecture Reference

### 16.1 Component Diagram

```
Browser
  |
  |  HTTP (port 80/443)
  v
nginx (reverse proxy)
  |
  |--- /FVSPrjBldr/  --> Shiny (port 3839): Project Builder
  |--- /FVSwork/*     --> Shiny (port 3838): FVS-Online
  |--- /FVSDataConvert/ --> Shiny (port 3840): Data Converter
  |--- /              --> Static files (/srv/fvs/www/)
```

### 16.2 Data Flow

```
User creates project (FVSPrjBldr)
  --> generates UUID directory in /srv/fvs/projects/{uuid}/
  --> writes app.R, projectId.txt
  --> sends email with project URL

User opens project (fvsOL)
  --> loads rFVS, which calls dyn.load() on FVS{variant}.so
  --> reads FVS_Data.db (SQLite) for stand/tree data
  --> user configures simulation via Shiny UI
  --> fvsRun() calls Fortran .Fortran("fvs", ...) for simulation
  --> results written to FVSOut.db (SQLite)
  --> Shiny renders ggplot2/leaflet/rgl visualizations
```

### 16.3 R Package Dependencies

```
fvsOL
  |-- rFVS (calls Fortran via .Fortran() and .C())
  |-- shiny (web framework)
  |-- RSQLite (database I/O)
  |-- ggplot2 (2D plots)
  |-- rgl (3D stand visualization)
  |-- leaflet (maps)
  |-- Cairo (graphics device)
  |-- rhandsontable (editable data grids)
  |-- openxlsx (Excel import)
  |-- shinyFiles (file browser)
  |-- zip (backup/restore)
  |-- colourpicker (UI color selection)
  |-- plyr, dplyr (data manipulation)
```

### 16.4 File Types

| Extension | Purpose | Tool |
|-----------|---------|------|
| `.so` | FVS Fortran shared library | gfortran + linker |
| `.db` | SQLite database (input/output) | RSQLite |
| `.kwd` | FVS keyword files (simulation config) | fvsOL/parms/ |
| `.mdb`, `.accdb` | Microsoft Access databases | mdbtools |
| `.xlsx` | Excel spreadsheets (data import) | openxlsx |

---

## Quick Start Summary

For the impatient, here is the minimum set of commands to get FVS-Online running locally on Fedora:

```bash
# 1. Install system packages
sudo dnf install gcc-gfortran gcc gcc-c++ make R R-devel \
    cairo-devel libcurl-devel openssl-devel libxml2-devel sqlite-devel \
    mesa-libGLU-devel libXt-devel libX11-devel mdbtools sqlite

# 2. Create directories
mkdir -p /srv/fvs/{bin,config,projects,interface,patches,R-libs}

# 3. Build FVS libraries (from USDA source)
cd /path/to/ForestVegetationSimulator-main/bin
make all CC=gcc FC=gfortran
cp ../FVS*.so /srv/fvs/bin/

# 4. Install R packages
R -e 'install.packages(c("shiny","Cairo","rhandsontable","ggplot2","RSQLite","plyr","dplyr","colourpicker","rgl","leaflet","zip","openxlsx","shinyFiles","yaml"), lib="/srv/fvs/R-libs")'

# 5. Install rFVS and fvsOL
cp -r /path/to/ForestVegetationSimulator-Interface-main/* /srv/fvs/interface/
R CMD INSTALL --library=/srv/fvs/R-libs /srv/fvs/interface/rFVS
R CMD INSTALL --library=/srv/fvs/R-libs /srv/fvs/interface/fvsOL

# 6. Configure
cp /path/to/fvs-modern/deployment/config/* /srv/fvs/config/
# Edit /srv/fvs/config/fvsol_config.yml with your paths

# 7. Launch
export R_LIBS_USER=/srv/fvs/R-libs
cd /srv/fvs/projects
Rscript -e '.libPaths("/srv/fvs/R-libs"); library(rFVS); library(fvsOL); options(shiny.port=3838L, shiny.host="127.0.0.1"); fvsOL(fvsBin="/srv/fvs/bin")'

# 8. Open browser to http://localhost:3838
```
