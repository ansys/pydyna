





:class:`DatabaseTracerGeneral`
==============================


.. py:class:: database_tracer_general.DatabaseTracerGeneral(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DATABASE_TRACER_GENERAL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DatabaseTracerGeneral

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~node`
            - Get or set the Node ID that locates the tracer (see Remark 1)
          * - :py:attr:`~elem`
            - Get or set the Element ID that controls the tracer motion (see Remarks 1 and 2)
          * - :py:attr:`~typm`
            - Get or set the ELEM type:
          * - :py:attr:`~move`
            - Get or set the Flag to define how the tracer moves (see Remark 1):
          * - :py:attr:`~set`
            - Get or set the Element set for which the data are output by the tracer (see Remark 2)
          * - :py:attr:`~typs`
            - Get or set the SET type:
          * - :py:attr:`~dt`
            - Get or set the Interval time between outputs (See Remark 3)
          * - :py:attr:`~tbeg`
            - Get or set the Time to start the outputs.
          * - :py:attr:`~tend`
            - Get or set the Time to stop the outputs
          * - :py:attr:`~fid`
            - Get or set the Id to be append to trcrgal_binout (See Remark 3).
          * - :py:attr:`~varloc`
            - Get or set the Variable location in trcrgal_binout to be replaced with the variable specified in the VAREPL field:
          * - :py:attr:`~varepl`
            - Get or set the Data to be output to the trcrgal_binout file instead of the variable located at VARLOC.  The interpretation of VAREPL is enumerated in the following list:


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

    from database_tracer_general import DatabaseTracerGeneral

Property detail
---------------

.. py:property:: node
   :type: int


   
   Get or set the Node ID that locates the tracer (see Remark 1)
















   ..
       !! processed by numpydoc !!

.. py:property:: elem
   :type: int


   
   Get or set the Element ID that controls the tracer motion (see Remarks 1 and 2)
   GT.0: Data are output for ELEM
   LT.0: Data are not output for ELEM.
















   ..
       !! processed by numpydoc !!

.. py:property:: typm
   :type: int


   
   Get or set the ELEM type:
   EQ.1: solid
   EQ.2: beam
   EQ.3: shell
   EQ.4: tshell
















   ..
       !! processed by numpydoc !!

.. py:property:: move
   :type: int


   
   Get or set the Flag to define how the tracer moves (see Remark 1):
   EQ.0: the tracer does not move with ELEM
   EQ.1: the tracer velocity is interpolated from ELEM nodal velocities
   EQ.2: the tracer position is interpolated from ELEM nodal positions.
















   ..
       !! processed by numpydoc !!

.. py:property:: set
   :type: int


   
   Get or set the Element set for which the data are output by the tracer (see Remark 2)
















   ..
       !! processed by numpydoc !!

.. py:property:: typs
   :type: int


   
   Get or set the SET type:
   EQ.0: part
   EQ.1: solid
   EQ.2: beam
   EQ.3: shell
   EQ.4: tshell .
















   ..
       !! processed by numpydoc !!

.. py:property:: dt
   :type: float


   
   Get or set the Interval time between outputs (See Remark 3)
















   ..
       !! processed by numpydoc !!

.. py:property:: tbeg
   :type: float


   
   Get or set the Time to start the outputs.
















   ..
       !! processed by numpydoc !!

.. py:property:: tend
   :type: float


   
   Get or set the Time to stop the outputs
















   ..
       !! processed by numpydoc !!

.. py:property:: fid
   :type: int


   
   Get or set the Id to be append to trcrgal_binout (See Remark 3).
















   ..
       !! processed by numpydoc !!

.. py:property:: varloc
   :type: int


   
   Get or set the Variable location in trcrgal_binout to be replaced with the variable specified in the VAREPL field:
   EQ.4:   -velocity
   EQ.5:   -velocity
   EQ.6:   -velocity
   EQ.7:   -stress
   EQ.8:   -stress
   EQ.9:   -stress
   EQ.10:  -stress
   EQ.11:  -stress
   EQ.12:  -stress
   EQ.13:  plastic strain
   EQ.14:  nodal mass
   EQ.15:  undefined
   GE.16 and LE.30:        other auxiliary variables
















   ..
       !! processed by numpydoc !!

.. py:property:: varepl
   :type: int


   
   Get or set the Data to be output to the trcrgal_binout file instead of the variable located at VARLOC.  The interpretation of VAREPL is enumerated in the following list:
   EQ.1:   -acceleration
   EQ.2:   - acceleration
   EQ.3:   - acceleration
   EQ.4:   nodal temperature
   EQ.5:   density
   EQ.6:   compression ratio
   EQ.7:   pressure.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DATABASE'


.. py:attribute:: subkeyword
   :value: 'TRACER_GENERAL'






