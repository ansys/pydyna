FROM rockylinux:8

LABEL description="LS-DYNA MPP R16.1.1"
LABEL email="pyansys.core@nsys.com"

# --------------------
# Build arguments
# --------------------

ARG FTP_USER=""
ARG FTP_LOGIN=""
ARG DYNA_URL="mpp-dyna/R16.1.1/linux/x86-64/ifort_190_sse2/MPP/"
ARG DYNA_FILE="ls-dyna_mpp_d_R16_1_1_x64_centos79_ifort190_sse2_openmpi405_sharelib.tgz_extractor.sh"
ARG DYNA_PATH="ls-dyna_mpp_d_R16_1_1_x64_centos79_ifort190_sse2_openmpi405_sharelib"

# --------------------
# Environment
# --------------------
ENV INSTALL_DIR=/opt/dyna
ENV LSTC_LICENSE=ansys
ENV LSTC_LICENSE_SERVER=""
ENV ANSYSLMD_LICENSE_FILE=""
ENV DEBIAN_FRONTEND=noninteractive

# --------------------
# System dependencies
# --------------------
RUN dnf -y update && \
    dnf -y install \
        wget \
        curl \
        tar \
        gzip \
        bash \
        python3 \
        python3-pip \
        git \
        which \
        numactl \
        libfabric \
        openssh-clients \
        hostname \
        procps-ng \
    && dnf clean all

# --------------------
# Install directory
# --------------------
RUN mkdir -p ${INSTALL_DIR}
WORKDIR ${INSTALL_DIR}

# --------------------
# Download & Extract LS-DYNA
# --------------------
RUN wget --user "$FTP_USER" --password "$FTP_LOGIN" \
      "https://ftp.lstc.com/user/${DYNA_URL}/${DYNA_FILE}" && \
    chmod +x "${DYNA_FILE}" && \
    yes | "./${DYNA_FILE}" && \
    rm -f "${DYNA_FILE}"

# --------------------
# License configuration
# --------------------
RUN echo "#LICENSE_TYPE: ansys" > ${INSTALL_DIR}/${DYNA_PATH}/LSTC_FILE && \
    echo "#LICENSE_SERVER: ${LSTC_LICENSE_SERVER:-localhost}" >> ${INSTALL_DIR}/${DYNA_PATH}/LSTC_FILE

# --------------------
# Add LS-DYNA to PATH
# --------------------
ENV PATH=${INSTALL_DIR}/${DYNA_PATH}:${PATH}

# --------------------
# Runtime working directory
# --------------------
WORKDIR /run

CMD ["/bin/bash"]
