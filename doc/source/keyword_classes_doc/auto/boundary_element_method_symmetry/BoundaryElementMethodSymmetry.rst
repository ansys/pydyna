





:class:`BoundaryElementMethodSymmetry`
======================================


.. py:class:: boundary_element_method_symmetry.BoundaryElementMethodSymmetry(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BOUNDARY_ELEMENT_METHOD_SYMMETRY keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BoundaryElementMethodSymmetry

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~bemsym`
            - Get or set the Defines a symmetry plane for boundary element method.


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

    from boundary_element_method_symmetry import BoundaryElementMethodSymmetry

Property detail
---------------

.. py:property:: bemsym
   :type: int


   
   Get or set the Defines a symmetry plane for boundary element method.
   EQ.0: no symmetry plane is defined,
   EQ.1: x=0 is a symmetry plane,
   EQ.2: y=0 is a symmetry plane,
   EQ.3: z=0 is a symmetry plane.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'BOUNDARY'


.. py:attribute:: subkeyword
   :value: 'ELEMENT_METHOD_SYMMETRY'






