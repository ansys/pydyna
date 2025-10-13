





:class:`BoundaryElementMethodControl`
=====================================


.. py:class:: boundary_element_method_control.BoundaryElementMethodControl(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BOUNDARY_ELEMENT_METHOD_CONTROL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BoundaryElementMethodControl

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~lwake`
            - Get or set the Number of elements in the wake of lifting surfaces. Wakes must be defined for all lifting surfaces.
          * - :py:attr:`~dtbem`
            - Get or set the Time increment between calls to the boundary element method. The fluid pressures computed during the previous call to the BEM will continue to be used for subsequent LS-DYNA iterations until a time increment of DTBEM has elapsed.
          * - :py:attr:`~iupbem`
            - Get or set the The number of times the BEM routines are called before the matrix of influence coefficients is recomputed and refactored.
          * - :py:attr:`~farbem`
            - Get or set the Nondimensional boundary between near-field and far-field calculation of influence coefficients.


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

    from boundary_element_method_control import BoundaryElementMethodControl

Property detail
---------------

.. py:property:: lwake
   :type: int


   
   Get or set the Number of elements in the wake of lifting surfaces. Wakes must be defined for all lifting surfaces.
















   ..
       !! processed by numpydoc !!

.. py:property:: dtbem
   :type: float


   
   Get or set the Time increment between calls to the boundary element method. The fluid pressures computed during the previous call to the BEM will continue to be used for subsequent LS-DYNA iterations until a time increment of DTBEM has elapsed.
















   ..
       !! processed by numpydoc !!

.. py:property:: iupbem
   :type: int


   
   Get or set the The number of times the BEM routines are called before the matrix of influence coefficients is recomputed and refactored.
















   ..
       !! processed by numpydoc !!

.. py:property:: farbem
   :type: float


   
   Get or set the Nondimensional boundary between near-field and far-field calculation of influence coefficients.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'BOUNDARY'


.. py:attribute:: subkeyword
   :value: 'ELEMENT_METHOD_CONTROL'






