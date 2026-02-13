PyDyna-pre
==========

The PyDyna-pre provide the ability to create keyword ``inputdeck`` through gRPC framework. 

Running pyDyna-pre in ``virtualenv``
------------------------------------

``virtualenv`` is a CLI tool that needs a Python interpreter to run.

Install ``virtualenv``
~~~~~~~~~~~~~~~~~~~~~~

.. code:: python

    pip install virtualenv

Create a Python virtual environment of the same version as ``virtualenv``, installed into the subdirectory ``venv``.

.. code:: python

    virtualenv venv

Activate ``venv`` on Windows by running command: 

.. code:: python

    cd venv
    ./Scripts/activate

Activate ``venv`` on Linux by running command: 

.. code:: python

    cd venv
    source bin/activate

Copy pyDyna package in ``venv`` folder
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: python

    venv/
        pyDyna/
	          ansys
		           /dyna/pre/Server
		      doc
		      examples
		      .flake8
		      LICENSE
		      ...
	

Requirements
------------

Go to ``pyDyna/requirements/pythonxx`` according to your Python version.
Then running command:

.. code:: python

    cd pyDyna/requirement/pythonxx
    pip install -r requirements.txt

Starting the server
-------------------

So far, Python 3.8/3.9 is used to start server, make sure this like below:

.. code:: python

    (venv) C:\pyDyna\examples\pre> C:\python38\python.exe --version
    Python 3.8.10

Here is the minimal content in server folder 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: python

    Server/
          kwserver.py
          kwprocess_pb2.py
          kwprocess_pb2_grpc.py
	      lib/
	         linux/
		           keywordreader.so
             windows/	
                   cp38/keywordreader.pyd
                   cp39/keywordreader.pyd				


Starting the server on Windows
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Start server by running command:

.. code:: python

    shell
    (venv) C:\pyDyna\ansys\dyna\pre\Server> python .\kwserver.py
    kwgrpc Server listening on: localhost:50051

Starting the server on Linux
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Start server by running command:

.. code:: python

    (venv) :~/pyDyna/ansys/dyna/pre/Server> python kwserver.py

Running pyDyna-pre client
-------------------------

See the examples in the examples folder for some basic example. More to be added later.

Structure ALE demo
~~~~~~~~~~~~~~~~~~

Go to ``pyDyna/examples/pre``,and run ``sale_efp.py``:

.. code:: python

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

At the same time, corresponding information is printed in the server side.

.. code:: bash

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


After running this file, the result file is saved in ``pyDyna/ansys/dyna/pre/Server/output``.

.. code:: bash

    (venv) C:\pyDyna\ansys\dyna\pre\Server> cd .\output\
    (venv) C:\pyDyna\ansys\dyna\pre\Server\output> ls


        Directory: C:\pyDyna\ansys\dyna\pre\Server\output


    Mode                 LastWriteTime         Length Name
    ----                 -------------         ------ ----
    -a----          2022/7/5      8:19        2436058 efpcase.k


Submit the result file, ``efpcase.k``, to LS-Dyna for simulation results.