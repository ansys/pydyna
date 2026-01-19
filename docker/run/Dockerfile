# Base image
FROM ubuntu:22.04

# Build arguments for FTP credentials
ARG FTP_USER
ARG FTP_PASSWORD

# Validate build arguments
RUN if [ -z "$FTP_USER" ] || [ -z "$FTP_PASSWORD" ]; then \
        echo "Error: FTP_USER and FTP_PASSWORD must be provided as build arguments"; \
        echo "Usage: docker build --build-arg FTP_USER=user --build-arg FTP_PASSWORD=pass ."; \
        exit 1; \
    fi

# Environment variables for runtime (optional)
ENV LSTC_LICENSE=""
ENV LSTC_LICENSE_SERVER=""

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
    && rm -rf /var/lib/apt/lists/*
 
# Working directory
WORKDIR /opt/Dyna
 
# Download LS-DYNA solver
RUN wget --user "${FTP_USER}" --password "${FTP_PASSWORD}" \
    https://ftp.lstc.com/user/ls-dyna/R16.1.1/linx.64/lsdyna/SMP/ls-dyna_smp_s_R16_1_1_x64_centos79_ifort190_sse2.tgz_extractor.sh

# Make the extractor script executable
RUN chmod +x ls-dyna_smp_s_R16_1_1_x64_centos79_ifort190_sse2.tgz_extractor.sh

# Extract the LS-DYNA solver (auto-accept license)
RUN echo "y" | ./ls-dyna_smp_s_R16_1_1_x64_centos79_ifort190_sse2.tgz_extractor.sh

# Verify extraction succeeded
RUN ls -la && \
    if [ -d "ls-dyna_smp_s_R16_1_1_x64_centos79_ifort190_sse2" ]; then \
        echo " LS-DYNA extracted successfully"; \
    else \
        echo "LS-DYNA extraction failed"; \
        ls -la; \
        exit 1; \
    fi

# entrypoint
ENTRYPOINT ["/opt/Dyna/ls-dyna_smp_s_R16_1_1_x64_centos79_ifort190_sse2/ls-dyna_smp_s_R16_1_1_x64_centos79_ifort190_sse2"]

