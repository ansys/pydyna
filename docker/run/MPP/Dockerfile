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
# ARG DYNA_URL="ls-dyna/R16.1.1/linx.64/lsdyna/SMP/"
ARG DYNA_URL="mpp-dyna/R16.1.1/linux/x86-64/ifort_190_sse2/MPP/"

# Extractor script filename
# ARG DYNA_FILE="ls-dyna_smp_d_R16_1_1_x64_centos79_ifort190_sse2.tgz_extractor.sh"
ARG DYNA_FILE="ls-dyna_mpp_d_R16_1_1_x64_centos79_ifort190_sse2_intelmpi-2018.tgz_extractor.sh"

# Folder created after extraction
# ARG DYNA_PATH="ls-dyna_smp_d_R16_1_1_x64_centos79_ifort190_sse2"
ARG DYNA_PATH="ls-dyna_mpp_d_R16_1_1_x64_centos79_ifort190_sse2_intelmpi-2018"

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
    gnupg \
    && rm -rf /var/lib/apt/lists/*


# --------------------
# Install dir
# --------------------
RUN mkdir -p ${INSTALL_DIR}
WORKDIR ${INSTALL_DIR}


# --------------------
# Extract LS-DYNA
# --------------------
RUN wget --user "$FTP_USER" --password "$FTP_LOGIN" \
      "https://ftp.lstc.com/user/${DYNA_URL}/${DYNA_FILE}" && \
    chmod +x "${DYNA_FILE}" && \
    yes | "./${DYNA_FILE}" && \
    rm "${DYNA_FILE}"


# --------------------
# License config
# --------------------
RUN echo "#LICENSE_TYPE: ansys" > ${INSTALL_DIR}/${DYNA_PATH}/LSTC_FILE && \
    echo "#LICENSE_SERVER: ${LSTC_LICENSE_SERVER:-localhost}" >> ${INSTALL_DIR}/${DYNA_PATH}/LSTC_FILE

# --------------------
# PATH
# --------------------
ENV PATH="${INSTALL_DIR}/${DYNA_PATH}:${PATH}"

# --------------------
# Runtime dir
# --------------------
WORKDIR /run
