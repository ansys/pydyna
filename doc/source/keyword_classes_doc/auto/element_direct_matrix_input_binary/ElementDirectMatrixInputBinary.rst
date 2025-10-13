





:class:`ElementDirectMatrixInputBinary`
=======================================


.. py:class:: element_direct_matrix_input_binary.ElementDirectMatrixInputBinary(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ELEMENT_DIRECT_MATRIX_INPUT_BINARY keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ElementDirectMatrixInputBinary

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~eid`
            - Get or set the Super element ID.
          * - :py:attr:`~ifrmt`
            - Get or set the Format:
          * - :py:attr:`~filename`
            - Get or set the Name of file that has the element matrices
          * - :py:attr:`~mass`
            - Get or set the Name of mass matrix in the file defined by FILENAME. This filename should be no more than eight characters to be compatible with NASTRAN.
          * - :py:attr:`~damp`
            - Get or set the Name of damping matrix in the file defined by FILENAME. This filename should be no more than eight characters to be compatible with NASTRAN.
          * - :py:attr:`~stif`
            - Get or set the Name of stiffness matrix in the file defined by FILENAME. This filename should be no more than eight characters to be compatible with NASTRAN.
          * - :py:attr:`~inert`
            - Get or set the Name of inertia matrix in the file defined by FILENAME.  This filename should be no more than eight characters to be compatible with NASTRAN.  This file must be present when *LOAD_BODY is used to put gravitational forces on the model..


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

    from element_direct_matrix_input_binary import ElementDirectMatrixInputBinary

Property detail
---------------

.. py:property:: eid
   :type: Optional[int]


   
   Get or set the Super element ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: ifrmt
   :type: Optional[int]


   
   Get or set the Format:
   EQ.0: standard format
   NE.0:  extended precision format
















   ..
       !! processed by numpydoc !!

.. py:property:: filename
   :type: Optional[str]


   
   Get or set the Name of file that has the element matrices
















   ..
       !! processed by numpydoc !!

.. py:property:: mass
   :type: Optional[str]


   
   Get or set the Name of mass matrix in the file defined by FILENAME. This filename should be no more than eight characters to be compatible with NASTRAN.
















   ..
       !! processed by numpydoc !!

.. py:property:: damp
   :type: Optional[str]


   
   Get or set the Name of damping matrix in the file defined by FILENAME. This filename should be no more than eight characters to be compatible with NASTRAN.
















   ..
       !! processed by numpydoc !!

.. py:property:: stif
   :type: Optional[str]


   
   Get or set the Name of stiffness matrix in the file defined by FILENAME. This filename should be no more than eight characters to be compatible with NASTRAN.
















   ..
       !! processed by numpydoc !!

.. py:property:: inert
   :type: Optional[str]


   
   Get or set the Name of inertia matrix in the file defined by FILENAME.  This filename should be no more than eight characters to be compatible with NASTRAN.  This file must be present when *LOAD_BODY is used to put gravitational forces on the model..
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ELEMENT'


.. py:attribute:: subkeyword
   :value: 'DIRECT_MATRIX_INPUT_BINARY'






