





:class:`SetIgaFaceXyzListGenerate`
==================================


.. py:class:: set_iga_face_xyz_list_generate.SetIgaFaceXyzListGenerate(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA SET_IGA_FACE_XYZ_LIST_GENERATE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: SetIgaFaceXyzListGenerate

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
          * - :py:attr:`~b1beg`
            - Get or set the First physical face ID in block N.
          * - :py:attr:`~b1end`
            - Get or set the Last physical face ID in block N. All defined ID's between and
          * - :py:attr:`~b2beg`
            - Get or set the First physical face ID in block N.
          * - :py:attr:`~b2end`
            - Get or set the Last physical face ID in block N. All defined ID's between and
          * - :py:attr:`~b3beg`
            - Get or set the First physical face ID in block N.
          * - :py:attr:`~b3end`
            - Get or set the Last physical face ID in block N. All defined ID's between and
          * - :py:attr:`~b4beg`
            - Get or set the First physical face ID in block N.
          * - :py:attr:`~b4end`
            - Get or set the Last physical face ID in block N. All defined ID's between and
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

    from set_iga_face_xyz_list_generate import SetIgaFaceXyzListGenerate

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

.. py:property:: b1beg
   :type: Optional[int]


   
   Get or set the First physical face ID in block N.
















   ..
       !! processed by numpydoc !!

.. py:property:: b1end
   :type: Optional[int]


   
   Get or set the Last physical face ID in block N. All defined ID's between and
   including B[N]BEG to B[N]END are added to the set.These sets are
   generated after all input is read so that gaps in the physical face
   numbering are not a problem.B[N]BEG and B[N]END may simply be
   limits on the IDs and not physical face ID's.
















   ..
       !! processed by numpydoc !!

.. py:property:: b2beg
   :type: Optional[int]


   
   Get or set the First physical face ID in block N.
















   ..
       !! processed by numpydoc !!

.. py:property:: b2end
   :type: Optional[int]


   
   Get or set the Last physical face ID in block N. All defined ID's between and
   including B[N]BEG to B[N]END are added to the set.These sets are
   generated after all input is read so that gaps in the physical face
   numbering are not a problem.B[N]BEG and B[N]END may simply be
   limits on the IDs and not physical face ID's.
















   ..
       !! processed by numpydoc !!

.. py:property:: b3beg
   :type: Optional[int]


   
   Get or set the First physical face ID in block N.
















   ..
       !! processed by numpydoc !!

.. py:property:: b3end
   :type: Optional[int]


   
   Get or set the Last physical face ID in block N. All defined ID's between and
   including B[N]BEG to B[N]END are added to the set.These sets are
   generated after all input is read so that gaps in the physical face
   numbering are not a problem.B[N]BEG and B[N]END may simply be
   limits on the IDs and not physical face ID's.
















   ..
       !! processed by numpydoc !!

.. py:property:: b4beg
   :type: Optional[int]


   
   Get or set the First physical face ID in block N.
















   ..
       !! processed by numpydoc !!

.. py:property:: b4end
   :type: Optional[int]


   
   Get or set the Last physical face ID in block N. All defined ID's between and
   including B[N]BEG to B[N]END are added to the set.These sets are
   generated after all input is read so that gaps in the physical face
   numbering are not a problem.B[N]BEG and B[N]END may simply be
   limits on the IDs and not physical face ID's.
















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
   :value: 'IGA_FACE_XYZ_LIST_GENERATE'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





