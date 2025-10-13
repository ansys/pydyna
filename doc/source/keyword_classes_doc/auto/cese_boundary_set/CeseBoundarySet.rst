





:class:`CeseBoundarySet`
========================


.. py:class:: cese_boundary_set.CeseBoundarySet(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CESE_BOUNDARY_SET keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: CeseBoundarySet

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ssid`
            - Get or set the Segment set  ID.
          * - :py:attr:`~dof_`
            - Get or set the Applicable degrees-of-freedom:
          * - :py:attr:`~lcid`
            - Get or set the Load curve ID to describe the variable value versus time, see *DEFINE_ CURVE.
          * - :py:attr:`~sf`
            - Get or set the Load curve scale factor.  (default=1.0).


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

    from cese_boundary_set import CeseBoundarySet

Property detail
---------------

.. py:property:: ssid
   :type: Optional[int]


   
   Get or set the Segment set  ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: dof_
   :type: int


   
   Get or set the Applicable degrees-of-freedom:
   EQ.101:  x-velocity,
   EQ.102:  y-velocity,
   EQ.103:  z-velocity,
   EQ.104:  density,
   EQ.105:  pressure ,
   EQ.106:  temperature,
   EQ.201:  x, y & z-velocity,
   EQ.202: x & y-velocity,
   EQ.203:  x & z-velocity,
   EQ.204:  y & z-velocity.
   .
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve ID to describe the variable value versus time, see *DEFINE_ CURVE.
















   ..
       !! processed by numpydoc !!

.. py:property:: sf
   :type: float


   
   Get or set the Load curve scale factor.  (default=1.0).
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CESE'


.. py:attribute:: subkeyword
   :value: 'BOUNDARY_SET'






