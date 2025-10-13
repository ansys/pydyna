





:class:`InitialHydrostaticAle`
==============================


.. py:class:: initial_hydrostatic_ale.InitialHydrostaticAle(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA INITIAL_HYDROSTATIC_ALE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: InitialHydrostaticAle

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~alesid`
            - Get or set the Set ID.
          * - :py:attr:`~stype`
            - Get or set the Set type for the SID above:  EQ.0:  SID is a part set ID ; EQ.1:  SID is a part ID.
          * - :py:attr:`~vecid`
            - Get or set the A vector ID defining the direction of gravitational acceleration.
          * - :py:attr:`~grav`
            - Get or set the Magnitude of the gravitational acceleration.
          * - :py:attr:`~pbase`
            - Get or set the The "base" pressure of each fluid layer.  This is the ambient pressure at the top of each ALE material (fluid) layer to be initialized.  Each layer must be represented by one ALE multi-material group ID (AMMG).
          * - :py:attr:`~nid`
            - Get or set the Node ID defining the top location of a material/fluid layer.
          * - :py:attr:`~mmgblo`
            - Get or set the AMMG ID of the fluid layer immediately below this NID. Each node is defined in association with one AMMG layer below it. See Remark 3.  In case of S-ALE, AMMG name (AMMGNM) could be also used in place of AMMGID.


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

    from initial_hydrostatic_ale import InitialHydrostaticAle

Property detail
---------------

.. py:property:: alesid
   :type: Optional[int]


   
   Get or set the Set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: stype
   :type: int


   
   Get or set the Set type for the SID above:  EQ.0:  SID is a part set ID ; EQ.1:  SID is a part ID.
   EQ.2: Solid set ID (SSID).
















   ..
       !! processed by numpydoc !!

.. py:property:: vecid
   :type: Optional[int]


   
   Get or set the A vector ID defining the direction of gravitational acceleration.
















   ..
       !! processed by numpydoc !!

.. py:property:: grav
   :type: Optional[float]


   
   Get or set the Magnitude of the gravitational acceleration.
















   ..
       !! processed by numpydoc !!

.. py:property:: pbase
   :type: float


   
   Get or set the The "base" pressure of each fluid layer.  This is the ambient pressure at the top of each ALE material (fluid) layer to be initialized.  Each layer must be represented by one ALE multi-material group ID (AMMG).
















   ..
       !! processed by numpydoc !!

.. py:property:: nid
   :type: Optional[int]


   
   Get or set the Node ID defining the top location of a material/fluid layer.
















   ..
       !! processed by numpydoc !!

.. py:property:: mmgblo
   :type: Optional[int]


   
   Get or set the AMMG ID of the fluid layer immediately below this NID. Each node is defined in association with one AMMG layer below it. See Remark 3.  In case of S-ALE, AMMG name (AMMGNM) could be also used in place of AMMGID.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'INITIAL'


.. py:attribute:: subkeyword
   :value: 'HYDROSTATIC_ALE'






