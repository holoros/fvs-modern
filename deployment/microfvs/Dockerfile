# =============================================================================
# Dockerfile for microFVS + fvs-modern integration
#
# Builds FVS from fvs-modern's modernized Fortran source and serves the
# microFVS REST API (FastAPI) on port 80.
#
# Usage:
#   docker build -t fvs-modern-api -f deployment/microfvs/Dockerfile .
#   docker run -p 8000:80 fvs-modern-api
#
# API docs available at http://localhost:8000/docs (Swagger UI)
# =============================================================================

# --- Stage 1: Build FVS from modernized source ---
FROM ubuntu:24.04 AS builder

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        build-essential gfortran make && \
    rm -rf /var/lib/apt/lists/*

COPY src-converted/ /build/src-converted/

WORKDIR /build/src-converted/bin

# Build all 24 variants (22 US + 2 Canadian)
# The makefile expects OSTYPE to detect Linux
ENV OSTYPE=linux-gnu
RUN make US 2>/dev/null || true
# Canadian variants need the build script approach
COPY deployment/scripts/build_fvs_libraries.sh /build/build.sh
RUN bash /build/build.sh /build/src-converted /build/output bc on 2>/dev/null || true


# --- Stage 2: Python runtime with microFVS API ---
FROM python:3.11-slim AS runtime

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        libgfortran5 libquadmath0 && \
    rm -rf /var/lib/apt/lists/*

# Copy FVS shared libraries and executables from builder
COPY --from=builder /build/src-converted/bin/FVS??.so /usr/local/lib/
COPY --from=builder /build/src-converted/bin/FVS?? /usr/local/bin/
COPY --from=builder /build/output/FVS??.so /usr/local/lib/
# Make executables available (symlink shared libs as executables too)
RUN ldconfig

# Install Python dependencies
WORKDIR /app
COPY deployment/microfvs/requirements.txt /app/requirements.txt
RUN pip install --no-cache-dir --upgrade pip && \
    pip install --no-cache-dir -r /app/requirements.txt

# Copy the microFVS application
COPY deployment/microfvs/microfvs/ /app/microfvs/

EXPOSE 80
CMD ["uvicorn", "microfvs.main:app", "--host", "0.0.0.0", "--port", "80"]
