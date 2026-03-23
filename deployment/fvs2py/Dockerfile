# =============================================================================
# Dockerfile for fvs2py + fvs-modern integration
#
# Builds FVS from fvs-modern's modernized Fortran 90 source and provides the
# fvs2py Python wrapper for programmatic access to FVS via ctypes.
#
# Based on Vibrant Planet's fvs2py (CC-BY-NC-SA-4.0) and docker_fvs projects.
# Extended with all 24 variants (22 US + 2 Canadian).
#
# Usage:
#   docker build -t fvs-modern-fvs2py -f deployment/fvs2py/Dockerfile .
#   docker run -it fvs-modern-fvs2py python3
#
# Inside the container:
#   >>> from fvs2py import FVS
#   >>> fvs = FVS("/usr/local/lib/FVSne.so")
#   >>> fvs.load_keyfile("my_stand.key")
#   >>> fvs.run()
#   >>> print(fvs.summary)
# =============================================================================

# --- Stage 1: Build FVS from modernized source ---
FROM ubuntu:24.04 AS builder

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        build-essential gfortran make && \
    rm -rf /var/lib/apt/lists/*

COPY src-converted/ /build/src-converted/

WORKDIR /build/src-converted/bin

# Build all US variants via makefile
ENV OSTYPE=linux-gnu
RUN make US 2>/dev/null || true

# Canadian variants need the build script approach
COPY deployment/scripts/build_fvs_libraries.sh /build/build.sh
RUN bash /build/build.sh /build/src-converted /build/output bc on 2>/dev/null || true


# --- Stage 2: Python runtime with fvs2py ---
FROM python:3.11-slim AS runtime

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        libgfortran5 libquadmath0 && \
    rm -rf /var/lib/apt/lists/*

# Copy FVS shared libraries from builder
COPY --from=builder /build/src-converted/bin/FVS??.so /usr/local/lib/
COPY --from=builder /build/output/FVS??.so /usr/local/lib/
RUN ldconfig

# Install Python dependencies
WORKDIR /code
COPY deployment/fvs2py/requirements.txt /code/requirements.txt
RUN pip install --no-cache-dir --upgrade pip && \
    pip install --no-cache-dir -r /code/requirements.txt

# Copy the fvs2py package
COPY deployment/fvs2py/fvs2py/ /code/fvs2py/
COPY deployment/fvs2py/pyproject.toml /code/pyproject.toml

# Install fvs2py in development mode
RUN pip install --no-cache-dir -e .

# Copy test keyfiles
COPY deployment/fvs2py/fvs2py/tests/ /code/fvs2py/tests/

EXPOSE 8888
CMD ["python3"]
