FROM centos:7
#
# The order of the lines in here should be (as much as possible) in
# an order that will create the same series of intermediate images
# no mater what version of dyna we are using.  This will speed up
# experiments when creating containers over and over.  Ordering the
# commands this way makes the Dockerfile a bit more confusing, because
# for example we make all the directories up top, but don't put things
# in them until further down.  And some environment variables are near
# the top, but others near the bottom

LABEL "com.ansys.description"="Ansys MPPDYNA with gRPC server"

# install some missing stuff we want/need

RUN yum -y install openssh-clients openssh-server bind-utils sudo python3 unzip &>/dev/null && yum clean all
RUN python3 -m pip install --upgrade pip && \ 
python3 -m pip install grpcio && \
python3 -m pip install grpcio-tools && \ 
python3 -m pip install protobuf==4.21.0
#
ENV USER mpirun
ENV HOME /home/mpirun
ENV LD_LIBRARY_PATH /ansys_inc/lib
ENV LSTC_LICENSE network
ENV LSTC_LICENSE_SERVER license
ENV SSHDIR ${HOME}/.ssh


RUN groupadd ${USER} && useradd -g ${USER} -d ${HOME} ${USER} && \
    echo "${USER} ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers
#
RUN mkdir -p /ansys_inc/lib && \
mkdir -p /rundir && \
mkdir /var/run/sshd && \
mkdir -p ${SSHDIR} && \
chown -R ${USER}:${USER} /rundir && \
chmod -R 755 /rundir
WORKDIR /rundir

# set up the runtime environment variables for
# dyna

RUN echo "export LD_LIBRARY_PATH=/ansys_inc/lib:/opt/openmpi/lib" > ${HOME}/.bashrc
# Adding these causes problems because they can't be overridden
# via the "environment" attribute in a docker-compose file.
# The ENV commands above come through as defaults, but can be
# overridden (to use the ANSYS license, or a different license server)
#
# RUN echo "export LSTC_LICENSE=network" >> ${HOME}/.bashrc
# RUN echo "export LSTC_LICENSE_SERVER=license" >> ${HOME}/.bashrc
#
# Get ssh properly set up
#
RUN sed -i 's/PermitRootLogin without-password/PermitRootLogin yes/' /etc/ssh/sshd_config
# SSH login fix. Otherwise user is kicked off after login
RUN sed 's@session\s*required\s*pam_loginuid.so@session optional pam_loginuid.so@g' -i /etc/pam.d/sshd
#
EXPOSE 22
CMD ["/usr/sbin/sshd", "-D"]
#
# Everything above here should not depend on the version of DYNA or the MPI we are using
# and so should behave nicely as regards the Docker image cache.  Below here not so much
#
ENV PATH /opt/openmpi/bin:${PATH}
RUN echo "export PATH=/opt/openmpi/bin:\${PATH}" >> ${HOME}/.bashrc

#ADD ssh/config ${SSHDIR}/config
#ADD ssh/id_rsa ${SSHDIR}/id_rsa
#ADD ssh/id_rsa.pub ${SSHDIR}/id_rsa.pub
#ADD ssh/id_rsa.pub ${SSHDIR}/authorized_keys
#RUN ssh-keygen -A


RUN ssh-keygen -q -b 2048 -f ${SSHDIR}/id_rsa -N "" && \
chmod 700 ${SSHDIR} && chmod -R 600 ${SSHDIR}/id_rsa && \
echo "Host *" > ${SSHDIR}/config && \
echo "  StrictHostKeyChecking no" >> ${SSHDIR}/config && \
cp ${SSHDIR}/id_rsa.pub ${SSHDIR}/authorized_keys && \
ssh-keygen -A

RUN chmod -R 600 ${SSHDIR}* && \
    chown -R ${USER}:${USER} ${SSHDIR} && \
    chown -R ${USER}:${USER} ${HOME}

# Copy the openmpi libraries

COPY mppdyna_docker_centos7.zip .
RUN unzip -qu mppdyna_docker_centos7.zip && \
rm mppdyna_docker_centos7.zip && \
mkdir -p /opt/openmpi && \
cp -r mpi/* /opt/openmpi && \
chmod -R 777 /opt/openmpi && \

# The executable will go in /ansys_inc, and the
# ifort runtime libraries in /ansys_inc/lib

cp -rpu docker_dir/* /ansys_inc/ && \
chmod -R 777 /ansys_inc/
