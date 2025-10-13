





:class:`LoadFaceUvw`
====================


.. py:class:: load_face_uvw.LoadFaceUvw(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA LOAD_FACE_UVW keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: LoadFaceUvw

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~luid`
            - Get or set the Parametric face ID; see *IGA_FACE_UVW
          * - :py:attr:`~lcid`
            - Get or set the Load curve ID (see *DEFINE_CURVE). The load curve must provide pressure as a function of time
          * - :py:attr:`~sf`
            - Get or set the Load curve scale factor
          * - :py:attr:`~at`
            - Get or set the Arrival or birth time of pressure


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

    from load_face_uvw import LoadFaceUvw

Property detail
---------------

.. py:property:: luid
   :type: Optional[int]


   
   Get or set the Parametric face ID; see *IGA_FACE_UVW
















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


   
   Get or set the Arrival or birth time of pressure
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'LOAD'


.. py:attribute:: subkeyword
   :value: 'FACE_UVW'






