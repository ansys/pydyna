FROM python:3.6
RUN pip install grpcio protobuf
COPY . /server
CMD ["python", "/server/kwserver.py"]
