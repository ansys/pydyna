FROM ubuntu:22.04

LABEL description="LS-DYNA runtime image"
LABEL email="pyansys.core@nsys.com"

# --------------------
# Build arguments
# --------------------
ARG FTP_USER
ARG FTP_LOGIN

# Relative path from https://ftp.lstc.com/user/
# Example:
#   ls-dyna/R16.1.1/linx.64/lsdyna/SMP/
#   mpp-dyna/R16.1.1/linux/x86-64/ifort_190_sse2/MPP/
ARG DYNA_URL="ls-dyna/R16.1.1/linx.64/lsdyna/SMP/"

# Extractor script filename
ARG DYNA_FILE="ls-dyna_smp_d_R16_1_1_x64_centos79_ifort190_sse2.tgz_extractor.sh"

# Folder created after extraction
ARG DYNA_PATH="lsdyna_smp_d_R16_1_1_x64_centos79_ifort190_sse2"

# --------------------
# Environment
# --------------------
ENV DEBIAN_FRONTEND=noninteractive
ENV INSTALL_DIR=/opt/dyna

ENV LSTC_LICENSE=ansys
ENV LSTC_LICENSE_SERVER=""
ENV ANSYSLMD_LICENSE_FILE=""

# --------------------
# System dependencies
# --------------------
RUN apt-get update && apt-get install -y \
    wget \
    curl \
    tar \
    gzip \
    bash \
    python3 \
    python3-pip \
    git \
    && rm -rf /var/lib/apt/lists/*

RUN pip3 install --no-cache-dir uv

# --------------------
# Install LS-DYNA
# --------------------
WORKDIR ${INSTALL_DIR}

# Validate required args
RUN test -n "$FTP_USER" && \
    test -n "$FTP_LOGIN" && \
    test -n "$DYNA_URL" && \
    test -n "$DYNA_FILE" && \
    test -n "$DYNA_PATH"

# Download, extract, cleanup
RUN wget --user "$FTP_USER" --password "$FTP_LOGIN" \
      "https://ftp.lstc.com/user/${DYNA_URL}/${DYNA_FILE}" && \
    chmod +x "${DYNA_FILE}" && \
    echo "y" | "./${DYNA_FILE}" && \
    rm "${DYNA_FILE}"

# --------------------
# Verify installation   
# --------------------
RUN test -d "${INSTALL_DIR}/${DYNA_PATH}"

# --------------------
# License config
# --------------------
RUN echo "#LICENSE_TYPE: ansys" > ${INSTALL_DIR}/${DYNA_PATH}/LSTC_FILE && \
    echo "#LICENSE_SERVER: \${LSTC_LICENSE_SERVER:-localhost}" >> ${INSTALL_DIR}/${DYNA_PATH}/LSTC_FILE

# --------------------
# PATH
# --------------------
ENV PATH="${INSTALL_DIR}/${DYNA_PATH}:${PATH}"

# --------------------
# Runtime dir
# --------------------
WORKDIR /run
