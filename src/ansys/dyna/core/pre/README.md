# PyDYNA ``pre`` service

The PyDYNA ``pre`` service provide the ability to create keyword input decks through the gRPC framework. 

## Run the ``pre`` service in a virtual environment

A virtual environment is a CLI tool that must have a Python interpreter to run.

Install virtualenv:

```
pip install virtualenv
```

Create a Python virtual environment of the same version as ``virtualenv``, installed in the subdirectory ``venv``:

```
virtualenv venv
```

On Windows, activate the ``venv`` environment by running these commands: 

```
cd venv
./Scripts/activate
```

On Linux, activate the ``venv`` environment by running these commands: 

```
cd venv
source bin/activate
```

Copy the PyDYNA package into the ``venv`` folder:

```
venv/
    pyDyna/
	      ansys
		       /dyna/pre/Server
		  doc
		  examples
		  .flake8
		  LICENSE
		  ...
	
```

## Requirements

Go to ``pyDyna/requirements/python[xx]``, where ``[xx]`` is your Python version.
Then, run these commands:

```
cd pyDyna/requirement/pythonxx
pip install -r requirements.txt
```

## Start the server

Python 3.6 through 3.8 can be used to start the server. Validate by running
these commands:

```
(venv) C:\pyDyna\examples\pre> C:\python36\python.exe --version
Python 3.6.8
```

Here is the minimal content in the ``Server`` folder: 
```
Server/
      kwserver.py
      kwprocess_pb2.py
      kwprocess_pb2_grpc.py
	  lib/
	     linux/
		       keywordreader.so
         windows/
               	cp36/keywordreader.pyd	
                cp38/keywordreader.pyd
                cp39/keywordreader.pyd				
```

### Start the server on Windows

On Windows, start the server by running these commands:

```shell
(venv) C:\pyDyna\ansys\dyna\pre\Server> python .\kwserver.py
kwgrpc Server listening on: localhost:50051
```

### Start the server on Linux

On Linux, start the server by running these commands:

```
(venv) :~/pyDyna/ansys/dyna/pre/Server> python kwserver.py
```

## Run the PyDYNA ``pre`` service

See the ``Examples`` folder for basic usage examples.

### S-ALE (Structure ALE) demo

Go to the ``pyDyna/examples/pre`` directory and run the ``sale_efp.py`` file:

```
(venv) C:\pyDyna\examples\pre> python .\sale_efp.py
2022-07-05 08:19:42 :  Connected to kwServer...
2022-07-05 08:19:42 :  C:\pyDyna\examples\pre\input\sale\input\efpcase.k uploaded to server...
2022-07-05 08:19:43 :  Setup Analysis...
2022-07-05 08:19:43 :  Material air Created...
2022-07-05 08:19:43 :  Material air Created...
2022-07-05 08:19:43 :  Material HE Created...
2022-07-05 08:19:43 :  Material HE Created...
2022-07-05 08:19:43 :  Material liner Created...
2022-07-05 08:19:43 :  Material liner Created...
2022-07-05 08:19:43 :  Material vacuum Created...
2022-07-05 08:19:43 :  Material vacuum Created...
2022-07-05 08:19:43 :  ALE Structured mesh 1 Created...
2022-07-05 08:19:43 :  Material air filled in Mesh 1...
2022-07-05 08:19:43 :  Material HE filled in Mesh 1...
2022-07-05 08:19:43 :  Material liner filled in Mesh 1...
2022-07-05 08:19:43 :  Location of high explosive detonation Defined...
2022-07-05 08:19:43 :  Output Setting...
2022-07-05 08:19:43 :  efpcase.k is outputed...
```

Corresponding information is printed on the server side:

```
Load model: C:\pyDyna\ansys\dyna\pre\Server\input\efpcase.k
*DATABASE_SALE Created...
Termination Created...
DB Binary Created...
*CONTROL_ALE Created...
*EOS_LINEAR_POLYNOMIAL Created...
*MAT_NULL Created...
*ALE_STRUCTURED_MULTI-MATERIAL_GROUP Created...
*MAT_HIGH_EXPLOSIVE_BURN Created...
*EOS_JWL Created...
*ALE_STRUCTURED_MULTI-MATERIAL_GROUP Created...
*MAT_JOHNSON_COOK Created...
*EOS_GRUNEISEN Created...
*ALE_STRUCTURED_MULTI-MATERIAL_GROUP Created...
*MAT_VACUUM Created...
*ALE_STRUCTURED_MULTI-MATERIAL_GROUP Created...
*ALE_STRUCTURED_MESH_CONTROL_POINTS Created...
*ALE_STRUCTURED_MESH_CONTROL_POINTS Created...
*ALE_STRUCTURED_MESH_CONTROL_POINTS Created...
*ALE_STRUCTURED_MESH Created...
*ALE_STRUCTURED_MESH_VOLUME_FILLING Created...
*ALE_STRUCTURED_MESH_VOLUME_FILLING Created...
*ALE_STRUCTURED_MESH_VOLUME_FILLING Created...
*INITIAL_DETONATION Created...
Database MATSUMCreated...
Database GLSTATCreated...
Saved Successfully!
```

After running the ``sale_efp.py`` file, the result file is saved in the
``pyDyna/ansys/dyna/pre/Server/output`` directory.

```
(venv) C:\pyDyna\ansys\dyna\pre\Server> cd .\output\
(venv) C:\pyDyna\ansys\dyna\pre\Server\output> ls


    目录: C:\pyDyna\ansys\dyna\pre\Server\output


Mode                 LastWriteTime         Length Name
----                 -------------         ------ ----
-a----          2022/7/5      8:19        2436058 efpcase.k
```

Submit the result file, ``efpcase.k``, to Ansys LS-DYNA for simulation results.
