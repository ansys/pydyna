# pyDyna-pre

The PyDyna-pre provide the ability to create keyword inputdeck by grpc.

## Starting the Server

### Starting the server on Linux:

Keep these files in the same folder
```
Server/
kwserver.py
keywordreader.so
kwprocess_pb2.py
kwprocess_pb2_grpc.py
```
Start server:
```
:~/pyDyna/ansys/dyna/pre/Server> python kwserver.py
```

### Starting the Server on Windows:

Here is the minimal content in Server folder 
```
Server/
kwserver.py
keywordreader.pyd
kwprocess_pb2.py
kwprocess_pb2_grpc.py
```
Start server by running command:
```
:pyDyna\ansys\dyna\pre\Server> python kwserver.py
```

## Running pyDyna-pre client

See the examples in the examples folder for some basic example.  More will be added later.

### Brief Demo

Go to pyDyna/examples/pre,and run sale_efp.py:

```
pyDyna\examples\pre> python sale_efp.py
```

After running this file,the result file will be save in pyDyna/ansys/dyna/pre/Server/output
