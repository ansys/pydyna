





:class:`LoadFaceUvwSet`
=======================


.. py:class:: load_face_uvw_set.LoadFaceUvwSet(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA LOAD_FACE_UVW_SET keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: LoadFaceUvwSet

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~fid`
            - Get or set the Parametric face set ID for the SET keyword option; see *SET_IGA_FACE_UVW (see Remark 1)
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

    from load_face_uvw_set import LoadFaceUvwSet

Property detail
---------------

.. py:property:: fid
   :type: Optional[int]


   
   Get or set the Parametric face set ID for the SET keyword option; see *SET_IGA_FACE_UVW (see Remark 1)
















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
   :value: 'FACE_UVW_SET'






