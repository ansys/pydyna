





:class:`DefineDeToSurfaceTied`
==============================


.. py:class:: define_de_to_surface_tied.DefineDeToSurfaceTied(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_DE_TO_SURFACE_TIED keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineDeToSurfaceTied

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~slave`
            - Get or set the DES nodes.
          * - :py:attr:`~master`
            - Get or set the Shell set.
          * - :py:attr:`~stype`
            - Get or set the EQ.0: Slave node set
          * - :py:attr:`~mtype`
            - Get or set the EQ.0: Part set
          * - :py:attr:`~nflf`
            - Get or set the Normal failure force. Only tensile failure, i.e., tensile normal forces, will be considered in the failure criterion.
          * - :py:attr:`~sflf`
            - Get or set the Shear failure force.
          * - :py:attr:`~nen`
            - Get or set the Exponent for normal force.
          * - :py:attr:`~mes`
            - Get or set the Exponent for shear force. Failure criterion.
          * - :py:attr:`~lcid`
            - Get or set the Optional curve ID defining a scale factor vs. time.  The scale factor is applied to NFLF and SFLF, making the failure forces time-dependent.
          * - :py:attr:`~nsort`
            - Get or set the Number of cycle between bucket sort.
          * - :py:attr:`~maxgap`
            - Get or set the Maximum gap between DES and master surface:
          * - :py:attr:`~title`
            - Get or set the Additional title line


   .. tab-item:: Attributes

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~keyword`
            - 
          * - :py:attr:`~subkeyword`
            - 
          * - :py:attr:`~option_specs`
            - Get the card format type.






Import detail
-------------

.. code-block:: python

    from define_de_to_surface_tied import DefineDeToSurfaceTied

Property detail
---------------

.. py:property:: slave
   :type: int


   
   Get or set the DES nodes.
















   ..
       !! processed by numpydoc !!

.. py:property:: master
   :type: int


   
   Get or set the Shell set.
















   ..
       !! processed by numpydoc !!

.. py:property:: stype
   :type: int


   
   Get or set the EQ.0: Slave node set
   EQ.1: Slave node
   EQ.2: Slave part set
   EQ.3: Slave part.
















   ..
       !! processed by numpydoc !!

.. py:property:: mtype
   :type: int


   
   Get or set the EQ.0: Part set
   EQ.1: Part.
















   ..
       !! processed by numpydoc !!

.. py:property:: nflf
   :type: Optional[float]


   
   Get or set the Normal failure force. Only tensile failure, i.e., tensile normal forces, will be considered in the failure criterion.
















   ..
       !! processed by numpydoc !!

.. py:property:: sflf
   :type: Optional[float]


   
   Get or set the Shear failure force.
















   ..
       !! processed by numpydoc !!

.. py:property:: nen
   :type: float


   
   Get or set the Exponent for normal force.
















   ..
       !! processed by numpydoc !!

.. py:property:: mes
   :type: float


   
   Get or set the Exponent for shear force. Failure criterion.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Optional curve ID defining a scale factor vs. time.  The scale factor is applied to NFLF and SFLF, making the failure forces time-dependent.
















   ..
       !! processed by numpydoc !!

.. py:property:: nsort
   :type: int


   
   Get or set the Number of cycle between bucket sort.
















   ..
       !! processed by numpydoc !!

.. py:property:: maxgap
   :type: Optional[float]


   
   Get or set the Maximum gap between DES and master surface:
   GT.0.0: defines the ratio of the DES radius as the maximum gap, that is, MAXGAP × r_DES
   LT.0.0 : absolute value is used as the maximum gap
















   ..
       !! processed by numpydoc !!

.. py:property:: title
   :type: Optional[str]


   
   Get or set the Additional title line
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DEFINE'


.. py:attribute:: subkeyword
   :value: 'DE_TO_SURFACE_TIED'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





