# Base image
FROM ubuntu:22.04

# Build arguments for FTP credentials
ARG FTP_USER
ARG FTP_PASSWORD
ARG VERSION=R16.1.1
ARG VERSION_SHORT=R16_1_1
ARG PRECISION=smp_d

# Validate build arguments
RUN if [ -z "$FTP_USER" ] || [ -z "$FTP_PASSWORD" ]; then \
        echo "Error: FTP_USER and FTP_PASSWORD must be provided as build arguments"; \
        echo "Usage: docker build --build-arg FTP_USER=user --build-arg FTP_PASSWORD=pass ."; \
        exit 1; \
    fi

# Environment variables for runtime (optional)
ENV LSTC_LICENSE="ansys"
ENV LSTC_LICENSE_SERVER=""
ENV ANSYSLMD_LICENSE_FILE=""

# Metadata
# Prevent interactive prompts
ENV DEBIAN_FRONTEND=noninteractive
 
# Install dependencies
RUN apt-get update && apt-get install -y \
    wget \
    curl \
    tar \
    gzip \
    bash \
    file \
    python3 \
    python3-pip \
    python3-venv \
    git \
    && rm -rf /var/lib/apt/lists/*

# Install uv for faster Python package management
RUN pip3 install uv
 
# Working directory
WORKDIR /opt/Dyna
 
# Download LS-DYNA solver
RUN wget --user "${FTP_USER}" --password "${FTP_PASSWORD}" \
    https://ftp.lstc.com/user/ls-dyna/${VERSION}/linx.64/lsdyna/SMP/ls-dyna_${PRECISION}_${VERSION_SHORT}_x64_centos79_ifort190_sse2.tgz_extractor.sh
# Make the extractor script executable
RUN chmod +x ls-dyna_${PRECISION}_${VERSION_SHORT}_x64_centos79_ifort190_sse2.tgz_extractor.sh

# Extract the LS-DYNA solver (auto-accept license)
RUN echo "y" | ./ls-dyna_${PRECISION}_${VERSION_SHORT}_x64_centos79_ifort190_sse2.tgz_extractor.sh

# Verify extraction succeeded
RUN ls -la && \
    if [ -d "ls-dyna_${PRECISION}_${VERSION_SHORT}_x64_centos79_ifort190_sse2" ]; then \
        echo " LS-DYNA extracted successfully"; \
    else \
        echo "LS-DYNA extraction failed"; \
        ls -la; \
        exit 1; \
    fi

COPY . .


# # Create LSTC_FILE for license configuration
RUN echo "#LICENSE_TYPE: ansys" > /opt/Dyna/ls-dyna_${PRECISION}_${VERSION_SHORT}_x64_centos79_ifort190_sse2/LSTC_FILE && \
    echo "#LICENSE_SERVER: \${LSTC_LICENSE_SERVER:-localhost}" >> /opt/Dyna/ls-dyna_${PRECISION}_${VERSION_SHORT}_x64_centos79_ifort190_sse2/LSTC_FILE

# # Set environment variables for LSTC network licensing
# ENV LSTC_LICENSE=network

# Add LS-DYNA to PATH for easier access
ENV PATH="/opt/Dyna/ls-dyna_${PRECISION}_${VERSION_SHORT}_x64_centos79_ifort190_sse2:${PATH}"

# Set up working directory to match DockerRunner expectations
WORKDIR /run
