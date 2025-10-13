





:class:`DatabaseAleOperation`
=============================


.. py:class:: database_ale_operation.DatabaseAleOperation(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DATABASE_ALE_OPERATION keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DatabaseAleOperation

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~fct`
            - Get or set the *DEFINE_FUNCTION ID;
          * - :py:attr:`~hisvn`
            - Get or set the Number of the history variable replaced in d3plot
          * - :py:attr:`~wrt`
            - Get or set the File output flag. WRT must be a two digit number:WRT = L + M×10
          * - :py:attr:`~dt`
            - Get or set the Time interval between computed function values included in the .xy file
          * - :py:attr:`~setid`
            - Get or set the ALE element set ID.See Remarks 3 and 4. If the model is 2D(*SECTION_ALE2D), the set should be a shell set(see* SET_SHELL).If the model is 3D(*SECTION_SOLID), the set should be a solid set(see* SET_SOLID
          * - :py:attr:`~var`
            - Get or set the Arguments that can be included in function FCT (see Remark 1):


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

    from database_ale_operation import DatabaseAleOperation

Property detail
---------------

.. py:property:: fct
   :type: Optional[int]


   
   Get or set the *DEFINE_FUNCTION ID;
















   ..
       !! processed by numpydoc !!

.. py:property:: hisvn
   :type: Optional[int]


   
   Get or set the Number of the history variable replaced in d3plot
















   ..
       !! processed by numpydoc !!

.. py:property:: wrt
   :type: int


   
   Get or set the File output flag. WRT must be a two digit number:WRT = L + M×10
   The 1 digit controls the replacement of the history variable number HISVN in d3plot :
   L.EQ.1 : For each ALE element in the mesh, replace the values of the history variable with values computed by the function FCT
   L.EQ.0 : Do not modify d3plot.
   The 10s digit controls the history output of values computed by the function FCT :
   M.EQ.1 : For each ALE element in the set SETID, write.xy file that stores values computed by FCT at a frequency DT. (See Remarks 3 and 4.)
   M.EQ.0 : Do not output this history file.
















   ..
       !! processed by numpydoc !!

.. py:property:: dt
   :type: Optional[int]


   
   Get or set the Time interval between computed function values included in the .xy file
















   ..
       !! processed by numpydoc !!

.. py:property:: setid
   :type: Optional[int]


   
   Get or set the ALE element set ID.See Remarks 3 and 4. If the model is 2D(*SECTION_ALE2D), the set should be a shell set(see* SET_SHELL).If the model is 3D(*SECTION_SOLID), the set should be a solid set(see* SET_SOLID
















   ..
       !! processed by numpydoc !!

.. py:property:: var
   :type: int


   
   Get or set the Arguments that can be included in function FCT (see Remark 1):
   EQ.1:   xx - stress
   EQ.2 : yy - stress
   EQ.3 : zz - stress
   EQ.4 : xy - stress
   EQ.5 : yz - stress
   EQ.6 : zx - stress
   EQ.7 : Plastic strain
   EQ.8 : Internal energy
   EQ.9 : Bulk viscosity
   EQ.10 : Previous volume
   EQ.11 : Mass
   EQ.12 : Volume
   EQ.13 : Nodal x - positions
   EQ.14 : Nodal y - positions
   EQ.15 : Nodal z - positions
   EQ.16 : Nodal x - velocities
   EQ.17 : Nodal y - velocities
   EQ.18 : Nodal z - velocities
   EQ.19 : Nodal x - accelerations
   EQ.20 : Nodal y - accelerations
   EQ.21 : Nodal z - accelerations
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DATABASE'


.. py:attribute:: subkeyword
   :value: 'ALE_OPERATION'






