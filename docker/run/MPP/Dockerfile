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

RUN apt-get update && apt-get install -y gpg-agent wget && \
    wget -O- https://apt.repos.intel.com/intel-gpg-keys/GPG-PUB-KEY-INTEL-SW-PRODUCTS.PUB | gpg --dearmor | tee /usr/share/keyrings/oneapi-archive-keyring.gpg > /dev/null && \
    echo "deb [signed-by=/usr/share/keyrings/oneapi-archive-keyring.gpg] https://apt.repos.intel.com/oneapi all main" | tee /etc/apt/sources.list.d/oneAPI.list && \
    apt-get update && \
    apt-get install -y intel-oneapi-mpi-devel

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
# Intel MPI Environment
# --------------------
# Source Intel oneAPI MPI environment
ENV I_MPI_ROOT=/opt/intel/oneapi/mpi/latest
ENV MPIROOT=${I_MPI_ROOT}
ENV PATH="${I_MPI_ROOT}/bin:${PATH}"
ENV LD_LIBRARY_PATH="${I_MPI_ROOT}/lib:${I_MPI_ROOT}/lib/release:${LD_LIBRARY_PATH}"
ENV MANPATH="${I_MPI_ROOT}/share/man:${MANPATH}"
ENV FI_PROVIDER_PATH="${I_MPI_ROOT}/lib/prov:${FI_PROVIDER_PATH}"

# Source Intel oneAPI MPI environment and add to startup
RUN echo 'source /opt/intel/oneapi/mpi/latest/env/vars.sh' >> /etc/bash.bashrc
# Additional Intel MPI environment variables for proper initialization
ENV I_MPI_FABRICS=shm
ENV I_MPI_DEVICE=rdma:OpenIB-cma
ENV I_MPI_FALLBACK=1

# --------------------
# PATH
# --------------------
ENV PATH="${INSTALL_DIR}/${DYNA_PATH}:${PATH}"

# --------------------
# Runtime dir
# --------------------
WORKDIR /run
