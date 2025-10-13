





:class:`ControllerPlant`
========================


.. py:class:: controller_plant.ControllerPlant(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROLLER_PLANT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControllerPlant

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~plntid`
            - Get or set the Plant ID
          * - :py:attr:`~nin`
            - Get or set the Number of input DOFs, such as nodal force or voltage. If all nodes within a set share a single input variable, together they account for one DOF. For example, all nodes within a set share a single input voltage for a piezo actuator.
          * - :py:attr:`~nout`
            - Get or set the Number of output DOFs, such as nodal displacement or voltage. Note that the same node velocity will be automatically exported as well
          * - :py:attr:`~nmode`
            - Get or set the Number of modes for the modal truncation, or number of base vectors for the Krylov method. If zero, all active DOFs will be used (not recommended). The reduced system will have a dimension of 2NMODE for the modal truncation method, and NMODE for the Krylov method
          * - :py:attr:`~mtxq`
            - Get or set the Q matrix for linear-quadratic-regular (LQR) method (unused currently)
          * - :py:attr:`~mtxr`
            - Get or set the R matrix for linear-quadratic-regular (LQR) method (unused currently)
          * - :py:attr:`~mopt`
            - Get or set the Modal order reduction method (see Remark 1):
          * - :py:attr:`~fscilab`
            - Get or set the File name in LSDYNA format “SCI”. If specified, the reduced matrices will be written accordingly. If left blank, no such file will be generated.
          * - :py:attr:`~flsdyna`
            - Get or set the File name in LS-DYNA format .k. If specified, the reduced matrices will be written accordingly. If left blank, no such file will be generated
          * - :py:attr:`~fmatlab`
            - Get or set the File name in MATLAB format .m. If specified, the reduced matrices will be written accordingly. If left blank, no such file will be generated.
          * - :py:attr:`~nodi1`
            - Get or set the Node or node set index for the input channel.
          * - :py:attr:`~dofi1`
            - Get or set the Degree-of-freedom for input:
          * - :py:attr:`~nodi2`
            - Get or set the Node or node set index for the input channel.
          * - :py:attr:`~dofi2`
            - Get or set the Degree-of-freedom for input:
          * - :py:attr:`~nodi3`
            - Get or set the Node or node set index for the input channel.
          * - :py:attr:`~dofi3`
            - Get or set the Degree-of-freedom for input:
          * - :py:attr:`~nodi4`
            - Get or set the Node or node set index for the input channel.
          * - :py:attr:`~dofi4`
            - Get or set the Degree-of-freedom for input:
          * - :py:attr:`~nodo1`
            - Get or set the Node index for output
          * - :py:attr:`~dofo1`
            - Get or set the Degree-of-freedom for output:
          * - :py:attr:`~nodo2`
            - Get or set the Node index for output
          * - :py:attr:`~dofo2`
            - Get or set the Degree-of-freedom for output:
          * - :py:attr:`~nodo3`
            - Get or set the Node index for output
          * - :py:attr:`~dofo3`
            - Get or set the Degree-of-freedom for output:
          * - :py:attr:`~nodo4`
            - Get or set the Node index for output
          * - :py:attr:`~dofo4`
            - Get or set the Degree-of-freedom for output:
          * - :py:attr:`~nfeq`
            - Get or set the Number of shifted frequencies to generate the Krylov base vectors. In most cases, a single frequency at zero rad/s works. For the modal truncation method, just leave as it is.
          * - :py:attr:`~deftol`
            - Get or set the Deflation tolerance for the Krylov method. The default value of 10E-9 works in most cases. For the modal truncation method, just leave as it is.
          * - :py:attr:`~mod1`
            - Get or set the List all NMODE mode indexes for the modal truncation method, or NFEQ shifting frequencies (unit: rad/s) for the Krylov method. The default setting of a single frequency at 0 rad/s works in most Krylov cases.For the model truncation method, a negative MODx triggers mode generation between MODx-1 and -MODx, meaning all modes between MODx-1 and -MODx will be considered.
          * - :py:attr:`~mod2`
            - Get or set the List all NMODE mode indexes for the modal truncation method, or NFEQ shifting frequencies (unit: rad/s) for the Krylov method. The default setting of a single frequency at 0 rad/s works in most Krylov cases.For the model truncation method, a negative MODx triggers mode generation between MODx-1 and -MODx, meaning all modes between MODx-1 and -MODx will be considered.
          * - :py:attr:`~mod3`
            - Get or set the List all NMODE mode indexes for the modal truncation method, or NFEQ shifting frequencies (unit: rad/s) for the Krylov method. The default setting of a single frequency at 0 rad/s works in most Krylov cases.For the model truncation method, a negative MODx triggers mode generation between MODx-1 and -MODx, meaning all modes between MODx-1 and -MODx will be considered.
          * - :py:attr:`~mod4`
            - Get or set the List all NMODE mode indexes for the modal truncation method, or NFEQ shifting frequencies (unit: rad/s) for the Krylov method. The default setting of a single frequency at 0 rad/s works in most Krylov cases.For the model truncation method, a negative MODx triggers mode generation between MODx-1 and -MODx, meaning all modes between MODx-1 and -MODx will be considered.
          * - :py:attr:`~mod5`
            - Get or set the List all NMODE mode indexes for the modal truncation method, or NFEQ shifting frequencies (unit: rad/s) for the Krylov method. The default setting of a single frequency at 0 rad/s works in most Krylov cases.For the model truncation method, a negative MODx triggers mode generation between MODx-1 and -MODx, meaning all modes between MODx-1 and -MODx will be considered.
          * - :py:attr:`~mod6`
            - Get or set the List all NMODE mode indexes for the modal truncation method, or NFEQ shifting frequencies (unit: rad/s) for the Krylov method. The default setting of a single frequency at 0 rad/s works in most Krylov cases.For the model truncation method, a negative MODx triggers mode generation between MODx-1 and -MODx, meaning all modes between MODx-1 and -MODx will be considered.
          * - :py:attr:`~mod7`
            - Get or set the List all NMODE mode indexes for the modal truncation method, or NFEQ shifting frequencies (unit: rad/s) for the Krylov method. The default setting of a single frequency at 0 rad/s works in most Krylov cases.For the model truncation method, a negative MODx triggers mode generation between MODx-1 and -MODx, meaning all modes between MODx-1 and -MODx will be considered.
          * - :py:attr:`~mod8`
            - Get or set the List all NMODE mode indexes for the modal truncation method, or NFEQ shifting frequencies (unit: rad/s) for the Krylov method. The default setting of a single frequency at 0 rad/s works in most Krylov cases.For the model truncation method, a negative MODx triggers mode generation between MODx-1 and -MODx, meaning all modes between MODx-1 and -MODx will be considered.


   .. tab-item:: Attributes

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~keyword`
            - 
          * - :py:attr:`~subkeyword`
            - 






Import detail
-------------

.. code-block:: python

    from controller_plant import ControllerPlant

Property detail
---------------

.. py:property:: plntid
   :type: Optional[int]


   
   Get or set the Plant ID
















   ..
       !! processed by numpydoc !!

.. py:property:: nin
   :type: Optional[int]


   
   Get or set the Number of input DOFs, such as nodal force or voltage. If all nodes within a set share a single input variable, together they account for one DOF. For example, all nodes within a set share a single input voltage for a piezo actuator.
















   ..
       !! processed by numpydoc !!

.. py:property:: nout
   :type: Optional[int]


   
   Get or set the Number of output DOFs, such as nodal displacement or voltage. Note that the same node velocity will be automatically exported as well
















   ..
       !! processed by numpydoc !!

.. py:property:: nmode
   :type: Optional[int]


   
   Get or set the Number of modes for the modal truncation, or number of base vectors for the Krylov method. If zero, all active DOFs will be used (not recommended). The reduced system will have a dimension of 2NMODE for the modal truncation method, and NMODE for the Krylov method
















   ..
       !! processed by numpydoc !!

.. py:property:: mtxq
   :type: Optional[int]


   
   Get or set the Q matrix for linear-quadratic-regular (LQR) method (unused currently)
















   ..
       !! processed by numpydoc !!

.. py:property:: mtxr
   :type: Optional[int]


   
   Get or set the R matrix for linear-quadratic-regular (LQR) method (unused currently)
















   ..
       !! processed by numpydoc !!

.. py:property:: mopt
   :type: Optional[int]


   
   Get or set the Modal order reduction method (see Remark 1):
   EQ.0:   Modal truncation method
   EQ.1 : Krylov subspace method
















   ..
       !! processed by numpydoc !!

.. py:property:: fscilab
   :type: Optional[str]


   
   Get or set the File name in LSDYNA format “SCI”. If specified, the reduced matrices will be written accordingly. If left blank, no such file will be generated.
















   ..
       !! processed by numpydoc !!

.. py:property:: flsdyna
   :type: Optional[str]


   
   Get or set the File name in LS-DYNA format .k. If specified, the reduced matrices will be written accordingly. If left blank, no such file will be generated
















   ..
       !! processed by numpydoc !!

.. py:property:: fmatlab
   :type: Optional[str]


   
   Get or set the File name in MATLAB format .m. If specified, the reduced matrices will be written accordingly. If left blank, no such file will be generated.
















   ..
       !! processed by numpydoc !!

.. py:property:: nodi1
   :type: Optional[int]


   
   Get or set the Node or node set index for the input channel.
   GT.0:Nnode index
   LT.0:Node set index, within which all nodes share the same input variable, e.g., force, voltage, see Remark 2 below.
















   ..
       !! processed by numpydoc !!

.. py:property:: dofi1
   :type: int


   
   Get or set the Degree-of-freedom for input:
   EQ.1:   Nodal force in the x - direction, f_x
   EQ.2 : Nodal force in the y - direction, f_y
   EQ.3 : Nodal force in the z - direction, f_z
   EQ.7 : Voltage if piezoelectric materials are defined.See Remark 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: nodi2
   :type: Optional[int]


   
   Get or set the Node or node set index for the input channel.
   GT.0:Nnode index
   LT.0:Node set index, within which all nodes share the same input variable, e.g., force, voltage, see Remark 2 below.
















   ..
       !! processed by numpydoc !!

.. py:property:: dofi2
   :type: int


   
   Get or set the Degree-of-freedom for input:
   EQ.1:   Nodal force in the x - direction, f_x
   EQ.2 : Nodal force in the y - direction, f_y
   EQ.3 : Nodal force in the z - direction, f_z
   EQ.7 : Voltage if piezoelectric materials are defined.See Remark 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: nodi3
   :type: Optional[int]


   
   Get or set the Node or node set index for the input channel.
   GT.0:Nnode index
   LT.0:Node set index, within which all nodes share the same input variable, e.g., force, voltage, see Remark 2 below.
















   ..
       !! processed by numpydoc !!

.. py:property:: dofi3
   :type: int


   
   Get or set the Degree-of-freedom for input:
   EQ.1:   Nodal force in the x - direction, f_x
   EQ.2 : Nodal force in the y - direction, f_y
   EQ.3 : Nodal force in the z - direction, f_z
   EQ.7 : Voltage if piezoelectric materials are defined.See Remark 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: nodi4
   :type: Optional[int]


   
   Get or set the Node or node set index for the input channel.
   GT.0:Nnode index
   LT.0:Node set index, within which all nodes share the same input variable, e.g., force, voltage, see Remark 2 below.
















   ..
       !! processed by numpydoc !!

.. py:property:: dofi4
   :type: int


   
   Get or set the Degree-of-freedom for input:
   EQ.1:   Nodal force in the x - direction, f_x
   EQ.2 : Nodal force in the y - direction, f_y
   EQ.3 : Nodal force in the z - direction, f_z
   EQ.7 : Voltage if piezoelectric materials are defined.See Remark 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: nodo1
   :type: Optional[int]


   
   Get or set the Node index for output
















   ..
       !! processed by numpydoc !!

.. py:property:: dofo1
   :type: int


   
   Get or set the Degree-of-freedom for output:
   EQ.1:   Displacement along the x - direction
   EQ.2 : Displacement along the y - direction
   EQ.3 : Displacement along the z - direction
   EQ.7 : Voltage output if piezoelectric materials are defined.
















   ..
       !! processed by numpydoc !!

.. py:property:: nodo2
   :type: Optional[int]


   
   Get or set the Node index for output
















   ..
       !! processed by numpydoc !!

.. py:property:: dofo2
   :type: int


   
   Get or set the Degree-of-freedom for output:
   EQ.1:   Displacement along the x - direction
   EQ.2 : Displacement along the y - direction
   EQ.3 : Displacement along the z - direction
   EQ.7 : Voltage output if piezoelectric materials are defined.
















   ..
       !! processed by numpydoc !!

.. py:property:: nodo3
   :type: Optional[int]


   
   Get or set the Node index for output
















   ..
       !! processed by numpydoc !!

.. py:property:: dofo3
   :type: int


   
   Get or set the Degree-of-freedom for output:
   EQ.1:   Displacement along the x - direction
   EQ.2 : Displacement along the y - direction
   EQ.3 : Displacement along the z - direction
   EQ.7 : Voltage output if piezoelectric materials are defined.
















   ..
       !! processed by numpydoc !!

.. py:property:: nodo4
   :type: Optional[int]


   
   Get or set the Node index for output
















   ..
       !! processed by numpydoc !!

.. py:property:: dofo4
   :type: int


   
   Get or set the Degree-of-freedom for output:
   EQ.1:   Displacement along the x - direction
   EQ.2 : Displacement along the y - direction
   EQ.3 : Displacement along the z - direction
   EQ.7 : Voltage output if piezoelectric materials are defined.
















   ..
       !! processed by numpydoc !!

.. py:property:: nfeq
   :type: int


   
   Get or set the Number of shifted frequencies to generate the Krylov base vectors. In most cases, a single frequency at zero rad/s works. For the modal truncation method, just leave as it is.
















   ..
       !! processed by numpydoc !!

.. py:property:: deftol
   :type: float


   
   Get or set the Deflation tolerance for the Krylov method. The default value of 10E-9 works in most cases. For the modal truncation method, just leave as it is.
















   ..
       !! processed by numpydoc !!

.. py:property:: mod1
   :type: Optional[int]


   
   Get or set the List all NMODE mode indexes for the modal truncation method, or NFEQ shifting frequencies (unit: rad/s) for the Krylov method. The default setting of a single frequency at 0 rad/s works in most Krylov cases.For the model truncation method, a negative MODx triggers mode generation between MODx-1 and -MODx, meaning all modes between MODx-1 and -MODx will be considered.
















   ..
       !! processed by numpydoc !!

.. py:property:: mod2
   :type: Optional[int]


   
   Get or set the List all NMODE mode indexes for the modal truncation method, or NFEQ shifting frequencies (unit: rad/s) for the Krylov method. The default setting of a single frequency at 0 rad/s works in most Krylov cases.For the model truncation method, a negative MODx triggers mode generation between MODx-1 and -MODx, meaning all modes between MODx-1 and -MODx will be considered.
















   ..
       !! processed by numpydoc !!

.. py:property:: mod3
   :type: Optional[int]


   
   Get or set the List all NMODE mode indexes for the modal truncation method, or NFEQ shifting frequencies (unit: rad/s) for the Krylov method. The default setting of a single frequency at 0 rad/s works in most Krylov cases.For the model truncation method, a negative MODx triggers mode generation between MODx-1 and -MODx, meaning all modes between MODx-1 and -MODx will be considered.
















   ..
       !! processed by numpydoc !!

.. py:property:: mod4
   :type: Optional[int]


   
   Get or set the List all NMODE mode indexes for the modal truncation method, or NFEQ shifting frequencies (unit: rad/s) for the Krylov method. The default setting of a single frequency at 0 rad/s works in most Krylov cases.For the model truncation method, a negative MODx triggers mode generation between MODx-1 and -MODx, meaning all modes between MODx-1 and -MODx will be considered.
















   ..
       !! processed by numpydoc !!

.. py:property:: mod5
   :type: Optional[int]


   
   Get or set the List all NMODE mode indexes for the modal truncation method, or NFEQ shifting frequencies (unit: rad/s) for the Krylov method. The default setting of a single frequency at 0 rad/s works in most Krylov cases.For the model truncation method, a negative MODx triggers mode generation between MODx-1 and -MODx, meaning all modes between MODx-1 and -MODx will be considered.
















   ..
       !! processed by numpydoc !!

.. py:property:: mod6
   :type: Optional[int]


   
   Get or set the List all NMODE mode indexes for the modal truncation method, or NFEQ shifting frequencies (unit: rad/s) for the Krylov method. The default setting of a single frequency at 0 rad/s works in most Krylov cases.For the model truncation method, a negative MODx triggers mode generation between MODx-1 and -MODx, meaning all modes between MODx-1 and -MODx will be considered.
















   ..
       !! processed by numpydoc !!

.. py:property:: mod7
   :type: Optional[int]


   
   Get or set the List all NMODE mode indexes for the modal truncation method, or NFEQ shifting frequencies (unit: rad/s) for the Krylov method. The default setting of a single frequency at 0 rad/s works in most Krylov cases.For the model truncation method, a negative MODx triggers mode generation between MODx-1 and -MODx, meaning all modes between MODx-1 and -MODx will be considered.
















   ..
       !! processed by numpydoc !!

.. py:property:: mod8
   :type: Optional[int]


   
   Get or set the List all NMODE mode indexes for the modal truncation method, or NFEQ shifting frequencies (unit: rad/s) for the Krylov method. The default setting of a single frequency at 0 rad/s works in most Krylov cases.For the model truncation method, a negative MODx triggers mode generation between MODx-1 and -MODx, meaning all modes between MODx-1 and -MODx will be considered.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROLLER'


.. py:attribute:: subkeyword
   :value: 'PLANT'






