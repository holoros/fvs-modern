# =============================================================================
# FVS-Online Docker Image
# Base: Ubuntu 24.04 LTS
#
# Builds all 22 US FVS variant shared libraries, installs rFVS + fvsOL,
# and runs the Shiny web interface on port 3838.
#
# Build:
#   docker build -t fvs-online \
#     --build-arg FVS_REPO=https://github.com/USER/ForestVegetationSimulator.git \
#     --build-arg IFACE_REPO=https://github.com/USER/ForestVegetationSimulator-Interface.git \
#     .
#
# Or with local source (see docker-compose.yml):
#   docker build -t fvs-online .
#
# Run:
#   docker run -p 3838:3838 -v fvs-projects:/srv/fvs/projects fvs-online
#
# =============================================================================

FROM ubuntu:24.04 AS builder

ENV DEBIAN_FRONTEND=noninteractive
ENV TZ=UTC

# -------------------------------------------------------------------------
# System dependencies
# -------------------------------------------------------------------------
RUN apt-get update && apt-get install -y --no-install-recommends \
    gfortran gcc g++ make cmake \
    git ca-certificates \
    r-base r-base-dev \
    libcairo2-dev libcurl4-openssl-dev libssl-dev libxml2-dev \
    libsqlite3-dev libglu1-mesa-dev libxt-dev libx11-dev \
    libfreetype6-dev libharfbuzz-dev libfribidi-dev \
    libtiff-dev libjpeg-turbo8-dev libpng-dev \
    libfontconfig1-dev \
    mdbtools sqlite3 \
    uuid-dev \
    && rm -rf /var/lib/apt/lists/*

# -------------------------------------------------------------------------
# Copy FVS source (expects context to include both repos)
# -------------------------------------------------------------------------
COPY ForestVegetationSimulator-main /build/fvs-src
COPY ForestVegetationSimulator-Interface-main /build/iface-src

# -------------------------------------------------------------------------
# Fix known FORMAT bugs in vbase/initre.f
# -------------------------------------------------------------------------
RUN cd /build/fvs-src && \
    sed -i "s/A8'/A8,'/g" vbase/initre.f && \
    sed -i "s/A10'; STAND ORIGIN='/A10, '; STAND ORIGIN='/g" vbase/initre.f && \
    sed -i "s/I3' TARGETED/I3, ' TARGETED/" vbase/initre.f

# -------------------------------------------------------------------------
# Build all 22 US variant shared libraries
# -------------------------------------------------------------------------
RUN cd /build/fvs-src/bin && \
    OSTYPE=linux-gnu make -j$(nproc) US 2>&1 | tail -5 && \
    echo "Built $(ls /build/fvs-src/bin/FVS*.so | wc -l) variant libraries"

# -------------------------------------------------------------------------
# Install R package dependencies
# -------------------------------------------------------------------------
RUN R --no-save -e " \
    install.packages(c( \
        'shiny', 'Cairo', 'rhandsontable', 'ggplot2', 'RSQLite', \
        'plyr', 'colourpicker', 'rgl', 'leaflet', 'zip', \
        'openxlsx', 'shinyFiles', 'nlme', 'yaml' \
    ), repos = 'https://cloud.r-project.org', Ncpus = $(nproc)) \
"

# -------------------------------------------------------------------------
# Generate NAMESPACE files and install rFVS
# -------------------------------------------------------------------------
COPY deployment/config/rFVS_NAMESPACE /build/iface-src/rFVS/NAMESPACE
RUN mkdir -p /build/iface-src/rFVS/man && \
    printf '\\name{rFVS-package}\n\\alias{rFVS}\n\\docType{package}\n\\title{rFVS}\n\\description{R interface to FVS.}\n\\author{Nicholas Crookston}\n' \
    > /build/iface-src/rFVS/man/rFVS-package.Rd && \
    R CMD INSTALL /build/iface-src/rFVS

# -------------------------------------------------------------------------
# Generate NAMESPACE files and install fvsOL
# -------------------------------------------------------------------------
COPY deployment/config/fvsOL_NAMESPACE /build/iface-src/fvsOL/NAMESPACE
COPY deployment/config/fvsOL_DESCRIPTION /build/iface-src/fvsOL/DESCRIPTION
RUN mkdir -p /build/iface-src/fvsOL/man && \
    printf '\\name{fvsOL-package}\n\\alias{fvsOL}\n\\docType{package}\n\\title{fvsOL}\n\\description{FVS-Online Shiny interface.}\n\\author{FVS Staff}\n' \
    > /build/iface-src/fvsOL/man/fvsOL-package.Rd && \
    R CMD INSTALL /build/iface-src/fvsOL

# =========================================================================
# Production image (multi-stage for smaller size)
# =========================================================================
FROM ubuntu:24.04

ENV DEBIAN_FRONTEND=noninteractive
ENV TZ=UTC

RUN apt-get update && apt-get install -y --no-install-recommends \
    r-base \
    libcairo2 libcurl4t64 libssl3t64 libxml2 \
    libsqlite3-0 libglu1-mesa libxt6t64 libx11-6 \
    libfreetype6 libharfbuzz0b libfribidi0 \
    libtiff6 libjpeg-turbo8 libpng16-16t64 \
    libfontconfig1 libgfortran5 \
    mdbtools sqlite3 \
    && rm -rf /var/lib/apt/lists/*

# Create directory structure
RUN mkdir -p /srv/fvs/{bin,projects,config,logs}

# Copy built libraries from builder
COPY --from=builder /build/fvs-src/bin/FVS*.so /srv/fvs/bin/

# Copy installed R packages from builder
COPY --from=builder /usr/local/lib/R/site-library /usr/local/lib/R/site-library

# Copy test data for verification
COPY --from=builder /build/fvs-src/tests/APIviaR /srv/fvs/tests/APIviaR
COPY --from=builder /build/fvs-src/tests/FVSne /srv/fvs/tests/FVSne

# Copy interface data files
COPY --from=builder /build/iface-src/fvsOL/data /srv/fvs/data
COPY --from=builder /build/iface-src/fvsOL/parms /srv/fvs/parms

# Copy configuration
COPY deployment/config/fvsol_config.yml /srv/fvs/config/
COPY deployment/config/fvsol_config.R /srv/fvs/config/

# Set up non-root user
RUN useradd -r -m -s /bin/bash fvs && \
    chown -R fvs:fvs /srv/fvs

# Launcher script
COPY deployment/docker/entrypoint.sh /entrypoint.sh
RUN chmod +x /entrypoint.sh

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=30s --retries=3 \
    CMD curl -f http://localhost:3838/ || exit 1

USER fvs
WORKDIR /srv/fvs

EXPOSE 3838

ENTRYPOINT ["/entrypoint.sh"]
CMD ["fvsonline"]
