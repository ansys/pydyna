





:class:`ElementShellDof`
========================


.. py:class:: element_shell_dof.ElementShellDof(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ELEMENT_SHELL_DOF keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ElementShellDof

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
          * - :py:attr:`~ns1`
            - Get or set the Scalar node 1, parameter NDOF on the *NODE_SCALAR is normally set to 2.  If the thickness is constrained, set NDOF=0.
          * - :py:attr:`~ns2`
            - Get or set the Scalar node 2
          * - :py:attr:`~ns3`
            - Get or set the Scalar node 3.
          * - :py:attr:`~ns4`
            - Get or set the Scalar node 4.


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

    from element_shell_dof import ElementShellDof

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

.. py:property:: ns1
   :type: int


   
   Get or set the Scalar node 1, parameter NDOF on the *NODE_SCALAR is normally set to 2.  If the thickness is constrained, set NDOF=0.
















   ..
       !! processed by numpydoc !!

.. py:property:: ns2
   :type: int


   
   Get or set the Scalar node 2
















   ..
       !! processed by numpydoc !!

.. py:property:: ns3
   :type: int


   
   Get or set the Scalar node 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: ns4
   :type: int


   
   Get or set the Scalar node 4.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ELEMENT'


.. py:attribute:: subkeyword
   :value: 'SHELL_DOF'






