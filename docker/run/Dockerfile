# Base image
FROM ubuntu:22.04 as BUILDER


ARG FTP_USER
ARG FTP_PASSWORD
## env variables
ENV FTP_USER=${FTP_USER}
ENV FTP_PASSWORD=${FTP_PASSWORD}
ENV LSTC_LICENSE=""
ENV LSTC_LICENSE_SERVER=""


# Metadat
# Prevent interactive prompts
ENV DEBIAN_FRONTEND=noninteractive
 
# Install dependencies
RUN apt-get update && apt-get install -y \
    wget \
    curl \
    tar \
    gzip \
    bash \
    && rm -rf /var/lib/apt/lists/*
 
# Working directory
WORKDIR /opt/Dyna
 
# Download LS-DYNA solver
RUN wget --user ${FTP_USER} --password ${FTP_PASSWORD} \
    https://ftp.lstc.com/user/ls-dyna/R16.1.1/linx.64/lsdyna/SMP//ls-dyna_smp_s_R16_1_1_x64_centos79_ifort190_sse2.tgz_extractor.sh

# Make the extractor script executable
RUN chmod +x ls-dyna_smp_s_R16_1_1_x64_centos79_ifort190_sse2.tgz_extractor.sh

# Extract the LS-DYNA solver
RUN ./ls-dyna_smp_s_R16_1_1_x64_centos79_ifort190_sse2.tgz_extractor.sh

# entrypoint
ENTRYPOINT ["/opt/Dyna/ls-dyna_smp_s_R16_1_1_x64_centos79_ifort190_sse2/ls-dyna"]

