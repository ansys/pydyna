





:class:`IgaPointUvw`
====================


.. py:class:: iga_point_uvw.IgaPointUvw(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA IGA_POINT_UVW keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IgaPointUvw

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Parametric point ID. A unique number must be chosen.
          * - :py:attr:`~nid`
            - Get or set the Node IDs, see *NODE, see Remark 3.
          * - :py:attr:`~u`
            - Get or set the Coordinates in the parametric u-direction.
          * - :py:attr:`~v`
            - Get or set the Coordinates in the parametric v-direction.
          * - :py:attr:`~w`
            - Get or set the Coordinates in the parametric w-direction.


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

    from iga_point_uvw import IgaPointUvw

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Parametric point ID. A unique number must be chosen.
















   ..
       !! processed by numpydoc !!

.. py:property:: nid
   :type: Optional[int]


   
   Get or set the Node IDs, see *NODE, see Remark 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: u
   :type: float


   
   Get or set the Coordinates in the parametric u-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: v
   :type: float


   
   Get or set the Coordinates in the parametric v-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: w
   :type: float


   
   Get or set the Coordinates in the parametric w-direction.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'IGA'


.. py:attribute:: subkeyword
   :value: 'POINT_UVW'






