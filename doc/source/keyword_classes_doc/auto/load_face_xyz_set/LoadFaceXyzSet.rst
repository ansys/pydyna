





:class:`LoadFaceXyzSet`
=======================


.. py:class:: load_face_xyz_set.LoadFaceXyzSet(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA LOAD_FACE_XYZ_SET keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: LoadFaceXyzSet

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~fxyzsid`
            - Get or set the Physical face set ID; see *SET_IGA_FACE_XYZ
          * - :py:attr:`~lcid`
            - Get or set the Load curve ID (see *DEFINE_CURVE). The load curve must provide pressure as a function of time
          * - :py:attr:`~sf`
            - Get or set the Load curve scale factor
          * - :py:attr:`~at`
            - Get or set the Arrival time for pressure or birth time of pressure


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

    from load_face_xyz_set import LoadFaceXyzSet

Property detail
---------------

.. py:property:: fxyzsid
   :type: Optional[int]


   
   Get or set the Physical face set ID; see *SET_IGA_FACE_XYZ
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve ID (see *DEFINE_CURVE). The load curve must provide pressure as a function of time
















   ..
       !! processed by numpydoc !!

.. py:property:: sf
   :type: float


   
   Get or set the Load curve scale factor
















   ..
       !! processed by numpydoc !!

.. py:property:: at
   :type: float


   
   Get or set the Arrival time for pressure or birth time of pressure
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'LOAD'


.. py:attribute:: subkeyword
   :value: 'FACE_XYZ_SET'






