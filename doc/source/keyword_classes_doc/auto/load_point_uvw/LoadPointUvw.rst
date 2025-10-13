





:class:`LoadPointUvw`
=====================


.. py:class:: load_point_uvw.LoadPointUvw(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA LOAD_POINT_UVW keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: LoadPointUvw

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Parametric point ID; see *IGA_POINT_UVW
          * - :py:attr:`~dof`
            - Get or set the Applicable degrees-of-freedom:
          * - :py:attr:`~lcid`
            - Get or set the Load curve
          * - :py:attr:`~sf`
            - Get or set the Load curve scale factor


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

    from load_point_uvw import LoadPointUvw

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Parametric point ID; see *IGA_POINT_UVW
















   ..
       !! processed by numpydoc !!

.. py:property:: dof
   :type: Optional[int]


   
   Get or set the Applicable degrees-of-freedom:
   EQ.1:   x - direction of load action,
   EQ.2 : y - direction of load action,
   EQ.3 : z - direction of load action,
   EQ.5 : Moment about the x - axis,
   EQ.6 : Moment about the y - axis axis,
   EQ.7 : Moment about the z - axis axis
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve
















   ..
       !! processed by numpydoc !!

.. py:property:: sf
   :type: float


   
   Get or set the Load curve scale factor
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'LOAD'


.. py:attribute:: subkeyword
   :value: 'POINT_UVW'






