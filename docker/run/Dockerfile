FROM ghcr.io/ansys/mechanical:24.1.0

#TODO - update to a more recent mechanical imaged
#TODO - reduce the size of this image by pruning unnecessary files from the Mechanical image

LABEL description="lsdyna solver"

# install Intel-MPI

RUN apt update -y
RUN apt install -y wget

# This isn't directly needed, but for some reason if I don't install this package the installation of intel-mpi is broken because of a key issue
# Maybe this won't be needed in more recent pymechanical images...
RUN apt install -y libgtk2.0-dev

RUN wget -O- https://apt.repos.intel.com/intel-gpg-keys/GPG-PUB-KEY-INTEL-SW-PRODUCTS.PUB | gpg --dearmor | tee /usr/share/keyrings/intel-mpi-archive-keyring.gpg > /dev/null
RUN echo "deb [signed-by=/usr/share/keyrings/intel-mpi-archive-keyring.gpg] https://apt.repos.intel.com/mpi all main" | tee /etc/apt/sources.list.d/intel-mpi.list
RUN apt update -y
RUN apt install -y intel-mpi-rt-2019.9-304

# Set environment variables pointing to Intel-MPI and LS-DYNA

ENV MPIVARS=/opt/intel/impi/2019.9.304/intel64/bin/mpivars.sh
ENV MPIRUN=/install/ansys_inc/v241/commonfiles/MPI/Intel/2021.10.0/linx64/bin/mpirun
ENV LSDYNA_DP=/install/ansys_inc/v241/ansys/bin/linx64/lsdyna_dp.e
ENV LSDYNA_DP_MPP=/install/ansys_inc/v241/ansys/bin/linx64/lsdyna_dp_mpp.e
ENV LSDYNA_SP=/install/ansys_inc/v241/ansys/bin/linx64/lsdyna_sp.e
ENV LSDYNA_SP_MPP=/install/ansys_inc/v241/ansys/bin/linx64/lsdyna_sp_mpp.e

COPY ./file.sh /

ENTRYPOINT ["/file.sh"]
