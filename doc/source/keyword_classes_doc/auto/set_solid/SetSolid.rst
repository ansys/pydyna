





:class:`SetSolid`
=================


.. py:class:: set_solid.SetSolid(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA SET_SOLID keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: SetSolid

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~sid`
            - Get or set the Solid element set ID. All shell sets should have a unique set ID.
          * - :py:attr:`~solver`
            - Get or set the EQ.MECH: mechanics.
          * - :py:attr:`~k1`
            - Get or set the First solid element ID of the set.
          * - :py:attr:`~k2`
            - Get or set the Second solid element ID of the set.
          * - :py:attr:`~k3`
            - Get or set the Third solid element ID of the set.
          * - :py:attr:`~k4`
            - Get or set the Fourth solid element ID of the set.
          * - :py:attr:`~k5`
            - Get or set the Fifth solid element ID of the set.
          * - :py:attr:`~k6`
            - Get or set the Sixth solid element ID of the set.
          * - :py:attr:`~k7`
            - Get or set the Seventh solid element ID of the set.
          * - :py:attr:`~k8`
            - Get or set the Eighth solid element ID of the set.
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

    from set_solid import SetSolid

Property detail
---------------

.. py:property:: sid
   :type: Optional[int]


   
   Get or set the Solid element set ID. All shell sets should have a unique set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: solver
   :type: str


   
   Get or set the EQ.MECH: mechanics.
   EQ.CESE: CE/SE compressible fluid flow solver.
   EQ.ICFD: Incompressible fluid flow solver.
















   ..
       !! processed by numpydoc !!

.. py:property:: k1
   :type: Optional[int]


   
   Get or set the First solid element ID of the set.
















   ..
       !! processed by numpydoc !!

.. py:property:: k2
   :type: Optional[int]


   
   Get or set the Second solid element ID of the set.
















   ..
       !! processed by numpydoc !!

.. py:property:: k3
   :type: Optional[int]


   
   Get or set the Third solid element ID of the set.
















   ..
       !! processed by numpydoc !!

.. py:property:: k4
   :type: Optional[int]


   
   Get or set the Fourth solid element ID of the set.
















   ..
       !! processed by numpydoc !!

.. py:property:: k5
   :type: Optional[int]


   
   Get or set the Fifth solid element ID of the set.
















   ..
       !! processed by numpydoc !!

.. py:property:: k6
   :type: Optional[int]


   
   Get or set the Sixth solid element ID of the set.
















   ..
       !! processed by numpydoc !!

.. py:property:: k7
   :type: Optional[int]


   
   Get or set the Seventh solid element ID of the set.
















   ..
       !! processed by numpydoc !!

.. py:property:: k8
   :type: Optional[int]


   
   Get or set the Eighth solid element ID of the set.
















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
   :value: 'SET'


.. py:attribute:: subkeyword
   :value: 'SOLID'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





