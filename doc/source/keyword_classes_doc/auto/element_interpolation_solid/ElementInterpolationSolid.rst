





:class:`ElementInterpolationSolid`
==================================


.. py:class:: element_interpolation_solid.ElementInterpolationSolid(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ELEMENT_INTERPOLATION_SOLID keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ElementInterpolationSolid

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~eids`
            - Get or set the Element ID of the interpolation solid. This needs to coincide with a
          * - :py:attr:`~eidgs`
            - Get or set the Element ID of the master element defined in *ELEMENT_GENERALIZED_SOLID..
          * - :py:attr:`~ngp`
            - Get or set the Number of integration points of the master element.
          * - :py:attr:`~ip1`
            - Get or set the Integration point number (1 to NGP) in the order how they were defined in *DEFINE_ELEMENT_GENERALIZED_SOLID..
          * - :py:attr:`~w1`
            - Get or set the Interpolation weight of integration point i.
          * - :py:attr:`~ip2`
            - Get or set the Integration point number (1 to NGP) in the order how they were defined in *DEFINE_ELEMENT_GENERALIZED_SOLID.
          * - :py:attr:`~w2`
            - Get or set the Interpolation weight of integration point i.
          * - :py:attr:`~ip3`
            - Get or set the Integration point number (1 to NGP) in the order how they were defined in *DEFINE_ELEMENT_GENERALIZED_SOLID
          * - :py:attr:`~w3`
            - Get or set the Interpolation weight of integration point i.
          * - :py:attr:`~ip4`
            - Get or set the Integration point number (1 to NGP) in the order how they were defined in *DEFINE_ELEMENT_GENERALIZED_SOLID
          * - :py:attr:`~w4`
            - Get or set the Interpolation weight of integration point i.


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

    from element_interpolation_solid import ElementInterpolationSolid

Property detail
---------------

.. py:property:: eids
   :type: Optional[int]


   
   Get or set the Element ID of the interpolation solid. This needs to coincide with a
   proper definition of a 8-noded solid element (*ELEMENT_SOLID)
   using interpolation nodes (*CONSTRAINED_NODE_INTERPOLATION).
















   ..
       !! processed by numpydoc !!

.. py:property:: eidgs
   :type: Optional[int]


   
   Get or set the Element ID of the master element defined in *ELEMENT_GENERALIZED_SOLID..
















   ..
       !! processed by numpydoc !!

.. py:property:: ngp
   :type: Optional[int]


   
   Get or set the Number of integration points of the master element.
















   ..
       !! processed by numpydoc !!

.. py:property:: ip1
   :type: Optional[int]


   
   Get or set the Integration point number (1 to NGP) in the order how they were defined in *DEFINE_ELEMENT_GENERALIZED_SOLID..
















   ..
       !! processed by numpydoc !!

.. py:property:: w1
   :type: Optional[float]


   
   Get or set the Interpolation weight of integration point i.
















   ..
       !! processed by numpydoc !!

.. py:property:: ip2
   :type: Optional[int]


   
   Get or set the Integration point number (1 to NGP) in the order how they were defined in *DEFINE_ELEMENT_GENERALIZED_SOLID.
















   ..
       !! processed by numpydoc !!

.. py:property:: w2
   :type: Optional[float]


   
   Get or set the Interpolation weight of integration point i.
















   ..
       !! processed by numpydoc !!

.. py:property:: ip3
   :type: Optional[int]


   
   Get or set the Integration point number (1 to NGP) in the order how they were defined in *DEFINE_ELEMENT_GENERALIZED_SOLID
















   ..
       !! processed by numpydoc !!

.. py:property:: w3
   :type: Optional[float]


   
   Get or set the Interpolation weight of integration point i.
















   ..
       !! processed by numpydoc !!

.. py:property:: ip4
   :type: Optional[int]


   
   Get or set the Integration point number (1 to NGP) in the order how they were defined in *DEFINE_ELEMENT_GENERALIZED_SOLID
















   ..
       !! processed by numpydoc !!

.. py:property:: w4
   :type: Optional[float]


   
   Get or set the Interpolation weight of integration point i.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ELEMENT'


.. py:attribute:: subkeyword
   :value: 'INTERPOLATION_SOLID'






