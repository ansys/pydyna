FROM ghcr.io/ansys/mechanical:24.1.0 AS pymechanical

FROM ubuntu:20.04

LABEL description="lsdyna solver"

# install Intel-MPI

ENV TZ=America/New_York
ENV DEBIAN_FRONTEND=noninteractive

RUN apt update -y
RUN apt install -y wget

# This isn't directly needed, but for some reason if I don't install this package the installation of intel-mpi is broken because of a key issue
# Maybe this won't be needed in more recent pymechanical images...
RUN apt install -y libgtk2.0-dev

RUN wget -O- https://apt.repos.intel.com/intel-gpg-keys/GPG-PUB-KEY-INTEL-SW-PRODUCTS.PUB | gpg --dearmor | tee /usr/share/keyrings/intel-mpi-archive-keyring.gpg > /dev/null
RUN echo "deb [signed-by=/usr/share/keyrings/intel-mpi-archive-keyring.gpg] https://apt.repos.intel.com/mpi all main" | tee /etc/apt/sources.list.d/intel-mpi.list
RUN apt update -y
RUN apt install -y intel-mpi-rt-2019.9-304

ARG MAJORVER=24
ARG MINORVER=1
ARG VER=$MAJORVER$MINORVER

RUN mkdir -p /install/ansys_inc/shared_files
RUN mkdir -p /install/ansys_inc/v$VER/ansys/bin/linx64
RUN mkdir -p /install/ansys_inc/v$VER/mpi

COPY --from=pymechanical /install/ansys_inc/v$VER/ansys/bin/linx64/lsdyna_dp.e /install/ansys_inc/v$VER/ansys/bin/linx64/lsdyna_dp.e
COPY --from=pymechanical /install/ansys_inc/v$VER/ansys/bin/linx64/lsdyna_dp_mpp.e /install/ansys_inc/v$VER/ansys/bin/linx64/lsdyna_dp_mpp.e
COPY --from=pymechanical /install/ansys_inc/v$VER/ansys/bin/linx64/lsdyna_sp.e /install/ansys_inc/v$VER/ansys/bin/linx64/lsdyna_sp.e
COPY --from=pymechanical /install/ansys_inc/v$VER/ansys/bin/linx64/lsdyna_sp_mpp.e /install/ansys_inc/v$VER/ansys/bin/linx64/lsdyna_sp_mpp.e
COPY --from=pymechanical /install/ansys_inc/v$VER/licensingclient /install/ansys_inc/v$VER/licensingclient
COPY --from=pymechanical /install/ansys_inc/shared_files /install/ansys_inc/shared_files
COPY --from=pymechanical /install/ansys_inc/v$VER/builddate.txt /install/ansys_inc/v$VER/builddate.txt
COPY --from=pymechanical /install/ansys_inc/v$VER/commonfiles/MPI/Intel/2021.10.0/linx64/bin /install/ansys_inc/v$VER/mpi

# Set environment variables pointing to Intel-MPI and LS-DYNA

ENV MPIVARS=/opt/intel/impi/2019.9.304/intel64/bin/mpivars.sh
ENV MPIRUN=/install/ansys_inc/v$VER/mpi/mpirun
ENV LSDYNA_DP=/install/ansys_inc/v$VER/ansys/bin/linx64/lsdyna_dp.e
ENV LSDYNA_DP_MPP=/install/ansys_inc/v$VER/ansys/bin/linx64/lsdyna_dp_mpp.e
ENV LSDYNA_SP=/install/ansys_inc/v$VER/ansys/bin/linx64/lsdyna_sp.e
ENV LSDYNA_SP_MPP=/install/ansys_inc/v$VER/ansys/bin/linx64/lsdyna_sp_mpp.e

# OPTIONAL
# pass licensing environment variables into the build
ARG LSTC_LICENSE
ENV LSTC_LICENSE=$LSTC_LICENSE

ARG ANSYSLI_SERVERS
ENV ANSYSLI_SERVERS=$ANSYSLI_SERVERS

ARG ANSYSLMD_LICENSE_FILE
ENV ANSYSLMD_LICENSE_FILE=$ANSYSLMD_LICENSE_FILE

COPY ./file.sh /

RUN ["chmod", "+x", "/file.sh"]

ENTRYPOINT ["/file.sh"]
