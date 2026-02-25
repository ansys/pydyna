FROM ubuntu:22.04

LABEL description="LS-DYNA runtime image with both SMP and MPP executables"
LABEL email="pyansys.core@nsys.com"

# --------------------
# Build arguments
# --------------------
ARG FTP_USER
ARG FTP_LOGIN

# SMP executable
ARG SMP_URL="ls-dyna/R16.1.1/linx.64/lsdyna/SMP/"
ARG SMP_FILE="ls-dyna_smp_d_R16_1_1_x64_centos79_ifort190_sse2.tgz_extractor.sh"
ARG SMP_PATH="ls-dyna_smp_d_R16_1_1_x64_centos79_ifort190_sse2"

# MPP executable
ARG MPP_URL="mpp-dyna/R16.1.1/linux/x86-64/ifort_190_sse2/MPP/"
ARG MPP_FILE="ls-dyna_mpp_d_R16_1_1_x64_centos79_ifort190_sse2_openmpi405_sharelib.tgz_extractor.sh"
ARG MPP_PATH="ls-dyna_mpp_d_R16_1_1_x64_centos79_ifort190_sse2_openmpi405_sharelib"

# --------------------
# Environment
# --------------------
ENV INSTALL_DIR=/opt/dyna
ENV LSTC_LICENSE=ansys
ENV LSTC_LICENSE_SERVER=""
ENV ANSYSLMD_LICENSE_FILE=""
ENV DEBIAN_FRONTEND=noninteractive
ENV OMPI_ALLOW_RUN_AS_ROOT=1
ENV OMPI_ALLOW_RUN_AS_ROOT_CONFIRM=1

# --------------------
# System dependencies (includes OpenMPI for MPP)
# --------------------
RUN apt-get update && apt-get install -y \
    wget \
    tar \
    gzip \
    bash \
    libgomp1 \
    libatomic1 \
    libquadmath0 \
    libnuma1 \
    libgfortran5 \
    libstdc++6 \
    openmpi-bin \
    libopenmpi-dev \
    && rm -rf /var/lib/apt/lists/*

# --------------------
# Install LS-DYNA (SMP)
# --------------------
WORKDIR ${INSTALL_DIR}

# Validate required args
RUN test -n "$FTP_USER" && \
    test -n "$FTP_LOGIN"

# Download SMP executable
RUN wget --user "$FTP_USER" --password "$FTP_LOGIN" \
      "https://ftp.lstc.com/user/${SMP_URL}/${SMP_FILE}" && \
    chmod +x "${SMP_FILE}" && \
    echo "y" | "./${SMP_FILE}" && \
    rm "${SMP_FILE}"

# Verify SMP installation
RUN test -d "${INSTALL_DIR}/${SMP_PATH}" && \
    chmod -R +x "${INSTALL_DIR}/${SMP_PATH}/"

# Configure SMP license
RUN echo "#LICENSE_TYPE: ansys" > ${INSTALL_DIR}/${SMP_PATH}/LSTC_FILE && \
    echo "#LICENSE_SERVER: \${LSTC_LICENSE_SERVER:-localhost}" >> ${INSTALL_DIR}/${SMP_PATH}/LSTC_FILE

# --------------------
# Install LS-DYNA (MPP)  
# --------------------

# Download MPP executable
RUN wget --user "$FTP_USER" --password "$FTP_LOGIN" \
      "https://ftp.lstc.com/user/${MPP_URL}/${MPP_FILE}" && \
    chmod +x "${MPP_FILE}" && \
    echo "y" | "./${MPP_FILE}" && \
    rm "${MPP_FILE}"

# Verify MPP installation
RUN test -d "${INSTALL_DIR}/${MPP_PATH}" && \
    chmod -R +x "${INSTALL_DIR}/${MPP_PATH}/"

# Create placeholder license file (actual config via runtime env vars)
RUN touch ${INSTALL_DIR}/${MPP_PATH}/LSTC_FILE

# --------------------
# PATH (add both executables)
# --------------------
ENV PATH="${INSTALL_DIR}/${SMP_PATH}:${INSTALL_DIR}/${MPP_PATH}:${PATH}"
ENV LD_LIBRARY_PATH="${INSTALL_DIR}/${SMP_PATH}:${INSTALL_DIR}/${MPP_PATH}:${LD_LIBRARY_PATH}"

# --------------------
# Runtime dir
# --------------------
WORKDIR /run
