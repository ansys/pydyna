





:class:`ElementShellOffsetComposite`
====================================


.. py:class:: element_shell_offset_composite.ElementShellOffsetComposite(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ELEMENT_SHELL_OFFSET_COMPOSITE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ElementShellOffsetComposite

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~eid`
            - Get or set the Element ID. A unique number has to be used.
          * - :py:attr:`~pid`
            - Get or set the Part ID, see *PART.
          * - :py:attr:`~n1`
            - Get or set the Nodal point 1.
          * - :py:attr:`~n2`
            - Get or set the Nodal point 2.
          * - :py:attr:`~n3`
            - Get or set the Nodal point 3.
          * - :py:attr:`~n4`
            - Get or set the Nodal point 4.
          * - :py:attr:`~n5`
            - Get or set the Mid side nodal point 5.
          * - :py:attr:`~n6`
            - Get or set the Mid side nodal point 6.
          * - :py:attr:`~n7`
            - Get or set the Mid side nodal point 7.
          * - :py:attr:`~n8`
            - Get or set the Mid side nodal point 8.
          * - :py:attr:`~offset`
            - Get or set the The offset distance from the nodal points to the reference surface of the shell in the direction of the normal vector to the shell
          * - :py:attr:`~mid1`
            - Get or set the Material ID of integration point i, see *MAT_ . Section.
          * - :py:attr:`~thick1`
            - Get or set the Thickness of integration point i.
          * - :py:attr:`~b1`
            - Get or set the Material angle of integration point i.
          * - :py:attr:`~mid2`
            - Get or set the Material ID of integration point i, see *MAT_ . Section.
          * - :py:attr:`~thick2`
            - Get or set the Thickness of integration point i.
          * - :py:attr:`~b2`
            - Get or set the Material angle of integration point i.


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

    from element_shell_offset_composite import ElementShellOffsetComposite

Property detail
---------------

.. py:property:: eid
   :type: Optional[int]


   
   Get or set the Element ID. A unique number has to be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID, see *PART.
















   ..
       !! processed by numpydoc !!

.. py:property:: n1
   :type: Optional[int]


   
   Get or set the Nodal point 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: n2
   :type: Optional[int]


   
   Get or set the Nodal point 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: n3
   :type: Optional[int]


   
   Get or set the Nodal point 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: n4
   :type: Optional[int]


   
   Get or set the Nodal point 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: n5
   :type: Optional[int]


   
   Get or set the Mid side nodal point 5.
















   ..
       !! processed by numpydoc !!

.. py:property:: n6
   :type: Optional[int]


   
   Get or set the Mid side nodal point 6.
















   ..
       !! processed by numpydoc !!

.. py:property:: n7
   :type: Optional[int]


   
   Get or set the Mid side nodal point 7.
















   ..
       !! processed by numpydoc !!

.. py:property:: n8
   :type: Optional[int]


   
   Get or set the Mid side nodal point 8.
















   ..
       !! processed by numpydoc !!

.. py:property:: offset
   :type: float


   
   Get or set the The offset distance from the nodal points to the reference surface of the shell in the direction of the normal vector to the shell
















   ..
       !! processed by numpydoc !!

.. py:property:: mid1
   :type: Optional[int]


   
   Get or set the Material ID of integration point i, see *MAT_ . Section.
















   ..
       !! processed by numpydoc !!

.. py:property:: thick1
   :type: Optional[float]


   
   Get or set the Thickness of integration point i.
















   ..
       !! processed by numpydoc !!

.. py:property:: b1
   :type: Optional[float]


   
   Get or set the Material angle of integration point i.
















   ..
       !! processed by numpydoc !!

.. py:property:: mid2
   :type: Optional[int]


   
   Get or set the Material ID of integration point i, see *MAT_ . Section.
















   ..
       !! processed by numpydoc !!

.. py:property:: thick2
   :type: Optional[float]


   
   Get or set the Thickness of integration point i.
















   ..
       !! processed by numpydoc !!

.. py:property:: b2
   :type: Optional[float]


   
   Get or set the Material angle of integration point i.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ELEMENT'


.. py:attribute:: subkeyword
   :value: 'SHELL_OFFSET_COMPOSITE'






