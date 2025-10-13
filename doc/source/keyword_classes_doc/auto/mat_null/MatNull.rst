





:class:`MatNull`
================


.. py:class:: mat_null.MatNull(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_NULL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatNull

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification. A unique number has to be used.
          * - :py:attr:`~ro`
            - Get or set the Mass density.
          * - :py:attr:`~pc`
            - Get or set the Pressure cutoff (<= 0.0).
          * - :py:attr:`~mu`
            - Get or set the Viscosity coefficient (optional).
          * - :py:attr:`~terod`
            - Get or set the Relative volume. V/V0, for erosion in tension. Typically, use values greater than unity. If zero, erosion in tension is inactive.
          * - :py:attr:`~cerod`
            - Get or set the Relative volume, V/V0, for erosion in compression. Typically, use values less than unity. If zero, erosion in compression is inactive.
          * - :py:attr:`~ym`
            - Get or set the Young's modulus (used for null beams and shells only).
          * - :py:attr:`~pr`
            - Get or set the Poisson's ratio (used for null beams and shells only).


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

    from mat_null import MatNull

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification. A unique number has to be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Mass density.
















   ..
       !! processed by numpydoc !!

.. py:property:: pc
   :type: Optional[float]


   
   Get or set the Pressure cutoff (<= 0.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: mu
   :type: Optional[float]


   
   Get or set the Viscosity coefficient (optional).
















   ..
       !! processed by numpydoc !!

.. py:property:: terod
   :type: Optional[float]


   
   Get or set the Relative volume. V/V0, for erosion in tension. Typically, use values greater than unity. If zero, erosion in tension is inactive.
















   ..
       !! processed by numpydoc !!

.. py:property:: cerod
   :type: Optional[float]


   
   Get or set the Relative volume, V/V0, for erosion in compression. Typically, use values less than unity. If zero, erosion in compression is inactive.
















   ..
       !! processed by numpydoc !!

.. py:property:: ym
   :type: Optional[float]


   
   Get or set the Young's modulus (used for null beams and shells only).
















   ..
       !! processed by numpydoc !!

.. py:property:: pr
   :type: Optional[float]


   
   Get or set the Poisson's ratio (used for null beams and shells only).
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'MAT'


.. py:attribute:: subkeyword
   :value: 'NULL'






