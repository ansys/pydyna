# Pydyna-pre service Linux-based Dockerfile

FROM python:3.8-slim-buster

# Define the working directory
WORKDIR /server

# Install unzip
RUN apt-get update && apt-get install -y unzip

# Add the binary files from the latest release
COPY linux-binaries.zip .
RUN unzip -qu linux-binaries.zip && rm linux-binaries.zip

RUN pip3 install grpcio grpcio-tools protobuf

RUN python -m grpc_tools.protoc --python_out=./linux-binaries --grpc_python_out=./linux-binaries -I./linux-binaries ./linux-binaries/kwprocess.proto

EXPOSE 50051

CMD ["python3", "./linux-binaries/kwserver.py"]
