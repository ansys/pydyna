





:class:`BoundaryElementMethodWake`
==================================


.. py:class:: boundary_element_method_wake.BoundaryElementMethodWake(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BOUNDARY_ELEMENT_METHOD_WAKE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BoundaryElementMethodWake

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~nelem`
            - Get or set the Element number to which a wake is attached.
          * - :py:attr:`~nside`
            - Get or set the The side of NELEM to which the wake is attached. This should be the downstream side of NELEM.


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

    from boundary_element_method_wake import BoundaryElementMethodWake

Property detail
---------------

.. py:property:: nelem
   :type: Optional[int]


   
   Get or set the Element number to which a wake is attached.
















   ..
       !! processed by numpydoc !!

.. py:property:: nside
   :type: Optional[int]


   
   Get or set the The side of NELEM to which the wake is attached. This should be the downstream side of NELEM.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'BOUNDARY'


.. py:attribute:: subkeyword
   :value: 'ELEMENT_METHOD_WAKE'






