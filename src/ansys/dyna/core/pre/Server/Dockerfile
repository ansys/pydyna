ARG PYTHON_VERSION
FROM python:${PYTHON_VERSION}
WORKDIR /server
COPY . .
RUN pip3 install grpcio protobuf
CMD ["python3", "kwserver.py"]
