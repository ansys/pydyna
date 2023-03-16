***How to build docker image***
1. Create a empty folder
2. Copy Dockerfile kwserver.py kwprocess_pb2.py kwprocess_pb2_grpc.py keywordreader.so to the folder
3. Enter the folder, run 'docker build --build-arg PYTHON_VERSION=3.8-slim-buster -t ls-pre .'
   note:set PYTHON_VERSION as the python version you used

***Check the image***
1. Run 'docker images'

***Run kwserver image***
1. Run 'docker run -d -p 50051:50051 ls-pre'
or
   Run 'docker exec -it <container_id> /bin/bash'


***Check container***
1. Run 'docker ps'  (running container)
2. Run 'docker ps -a'   (running container and histroy run container)

***Stop container***
1. Run 'docker stop <container_id>

***Remove container***
1. Run 'docker rm <container_id>

***Remove local image***
1. Run 'docker rmi <image_id>


***Check docker server generated files***
1. Run 'docker inspect <container_id>'
2. Search text about 'Graphics upperdir', will find the generated files path
