





:class:`SetIgaFaceXyzListGenerateIncrementCollect`
==================================================


.. py:class:: set_iga_face_xyz_list_generate_increment_collect.SetIgaFaceXyzListGenerateIncrementCollect(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA SET_IGA_FACE_XYZ_LIST_GENERATE_INCREMENT_COLLECT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: SetIgaFaceXyzListGenerateIncrementCollect

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~sid`
            - Get or set the Set ID. A unique number must be chosen.
          * - :py:attr:`~da1`
            - Get or set the First attribute default value; see Remark 1.
          * - :py:attr:`~da2`
            - Get or set the Second attribute default value.
          * - :py:attr:`~da3`
            - Get or set the Third attribute default value0.
          * - :py:attr:`~da4`
            - Get or set the Fourth attribute default value.
          * - :py:attr:`~solver`
            - Get or set the Name of solver using this set (MECH, CESE, etc.). See Remark 2.
          * - :py:attr:`~bbeg`
            - Get or set the First physical face ID in block.
          * - :py:attr:`~bend`
            - Get or set the Last physical face ID in block..
          * - :py:attr:`~incr`
            - Get or set the physical face ID increment. Parametric/physical face IDs
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

    from set_iga_face_xyz_list_generate_increment_collect import SetIgaFaceXyzListGenerateIncrementCollect

Property detail
---------------

.. py:property:: sid
   :type: Optional[int]


   
   Get or set the Set ID. A unique number must be chosen.
















   ..
       !! processed by numpydoc !!

.. py:property:: da1
   :type: float


   
   Get or set the First attribute default value; see Remark 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: da2
   :type: float


   
   Get or set the Second attribute default value.
















   ..
       !! processed by numpydoc !!

.. py:property:: da3
   :type: float


   
   Get or set the Third attribute default value0.
















   ..
       !! processed by numpydoc !!

.. py:property:: da4
   :type: float


   
   Get or set the Fourth attribute default value.
















   ..
       !! processed by numpydoc !!

.. py:property:: solver
   :type: str


   
   Get or set the Name of solver using this set (MECH, CESE, etc.). See Remark 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: bbeg
   :type: Optional[int]


   
   Get or set the First physical face ID in block.
















   ..
       !! processed by numpydoc !!

.. py:property:: bend
   :type: Optional[int]


   
   Get or set the Last physical face ID in block..
















   ..
       !! processed by numpydoc !!

.. py:property:: incr
   :type: Optional[int]


   
   Get or set the physical face ID increment. Parametric/physical face IDs
   BBEG, BBEG + INCR, BBEG + 2 * INCR,and so on through BEND are added to the set.
















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
   :value: 'IGA_FACE_XYZ_LIST_GENERATE_INCREMENT_COLLECT'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





