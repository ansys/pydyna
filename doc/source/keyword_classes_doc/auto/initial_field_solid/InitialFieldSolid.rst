





:class:`InitialFieldSolid`
==========================


.. py:class:: initial_field_solid.InitialFieldSolid(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA INITIAL_FIELD_SOLID keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: InitialFieldSolid

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~eid`
            - Get or set the Element ID.
          * - :py:attr:`~nint`
            - Get or set the Number of integration points (should correspond to the solid element formulation).
          * - :py:attr:`~nhisv`
            - Get or set the Number of field variables. If NHISV exceeds the number of
          * - :py:attr:`~fld1`
            - Get or set the Data for the nth field (history) variable. NOTE that *MAT_TISSUE_DISPERSED only use FLD1 to FLD3 since NHISV = 3.
          * - :py:attr:`~fld2`
            - Get or set the Data for the nth field (history) variable. NOTE that *MAT_TISSUE_DISPERSED only use FLD1 to FLD3 since NHISV = 3.
          * - :py:attr:`~fld3`
            - Get or set the Data for the nth field (history) variable. NOTE that *MAT_TISSUE_DISPERSED only use FLD1 to FLD3 since NHISV = 3.
          * - :py:attr:`~fld4`
            - Get or set the Data for the nth field (history) variable. NOTE that *MAT_TISSUE_DISPERSED only use FLD1 to FLD3 since NHISV = 3.
          * - :py:attr:`~fld5`
            - Get or set the Data for the nth field (history) variable. NOTE that *MAT_TISSUE_DISPERSED only use FLD1 to FLD3 since NHISV = 3.
          * - :py:attr:`~fld6`
            - Get or set the Data for the nth field (history) variable. NOTE that *MAT_TISSUE_DISPERSED only use FLD1 to FLD3 since NHISV = 3.
          * - :py:attr:`~fld7`
            - Get or set the Data for the nth field (history) variable. NOTE that *MAT_TISSUE_DISPERSED only use FLD1 to FLD3 since NHISV = 3.
          * - :py:attr:`~fld8`
            - Get or set the Data for the nth field (history) variable. NOTE that *MAT_TISSUE_DISPERSED only use FLD1 to FLD3 since NHISV = 3.


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

    from initial_field_solid import InitialFieldSolid

Property detail
---------------

.. py:property:: eid
   :type: Optional[int]


   
   Get or set the Element ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: nint
   :type: Optional[int]


   
   Get or set the Number of integration points (should correspond to the solid element formulation).
















   ..
       !! processed by numpydoc !!

.. py:property:: nhisv
   :type: Optional[int]


   
   Get or set the Number of field variables. If NHISV exceeds the number of
   integration point field variables required by the constitutive model,
   only the number required is output; therefore, if in doubt, set NHISV to a large number.
















   ..
       !! processed by numpydoc !!

.. py:property:: fld1
   :type: Optional[int]


   
   Get or set the Data for the nth field (history) variable. NOTE that *MAT_TISSUE_DISPERSED only use FLD1 to FLD3 since NHISV = 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: fld2
   :type: Optional[int]


   
   Get or set the Data for the nth field (history) variable. NOTE that *MAT_TISSUE_DISPERSED only use FLD1 to FLD3 since NHISV = 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: fld3
   :type: Optional[int]


   
   Get or set the Data for the nth field (history) variable. NOTE that *MAT_TISSUE_DISPERSED only use FLD1 to FLD3 since NHISV = 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: fld4
   :type: Optional[int]


   
   Get or set the Data for the nth field (history) variable. NOTE that *MAT_TISSUE_DISPERSED only use FLD1 to FLD3 since NHISV = 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: fld5
   :type: Optional[int]


   
   Get or set the Data for the nth field (history) variable. NOTE that *MAT_TISSUE_DISPERSED only use FLD1 to FLD3 since NHISV = 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: fld6
   :type: Optional[int]


   
   Get or set the Data for the nth field (history) variable. NOTE that *MAT_TISSUE_DISPERSED only use FLD1 to FLD3 since NHISV = 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: fld7
   :type: Optional[int]


   
   Get or set the Data for the nth field (history) variable. NOTE that *MAT_TISSUE_DISPERSED only use FLD1 to FLD3 since NHISV = 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: fld8
   :type: Optional[int]


   
   Get or set the Data for the nth field (history) variable. NOTE that *MAT_TISSUE_DISPERSED only use FLD1 to FLD3 since NHISV = 3.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'INITIAL'


.. py:attribute:: subkeyword
   :value: 'FIELD_SOLID'






