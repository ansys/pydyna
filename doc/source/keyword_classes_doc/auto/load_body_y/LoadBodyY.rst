





:class:`LoadBodyY`
==================


.. py:class:: load_body_y.LoadBodyY(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA LOAD_BODY_Y keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: LoadBodyY

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~lcid`
            - Get or set the Load curve ID, see *DEFINE_CURVE.
          * - :py:attr:`~sf`
            - Get or set the Load curve scale factori which applies to both LCID and LCIDDR.
          * - :py:attr:`~lciddr`
            - Get or set the Load curve ID for dynamic relaxation phase. Only if dynamic relaxation is defined.
          * - :py:attr:`~xc`
            - Get or set the X-center of rotation, define for angular velocities.
          * - :py:attr:`~yc`
            - Get or set the Y-center of rotation, define for angular velocities.
          * - :py:attr:`~zc`
            - Get or set the Z-center of rotation, define for angular velocities.
          * - :py:attr:`~cid`
            - Get or set the Coordinate system ID to define acceleration in local coordinate system.


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

    from load_body_y import LoadBodyY

Property detail
---------------

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve ID, see *DEFINE_CURVE.
















   ..
       !! processed by numpydoc !!

.. py:property:: sf
   :type: float


   
   Get or set the Load curve scale factori which applies to both LCID and LCIDDR.
















   ..
       !! processed by numpydoc !!

.. py:property:: lciddr
   :type: int


   
   Get or set the Load curve ID for dynamic relaxation phase. Only if dynamic relaxation is defined.
















   ..
       !! processed by numpydoc !!

.. py:property:: xc
   :type: float


   
   Get or set the X-center of rotation, define for angular velocities.
















   ..
       !! processed by numpydoc !!

.. py:property:: yc
   :type: float


   
   Get or set the Y-center of rotation, define for angular velocities.
















   ..
       !! processed by numpydoc !!

.. py:property:: zc
   :type: float


   
   Get or set the Z-center of rotation, define for angular velocities.
















   ..
       !! processed by numpydoc !!

.. py:property:: cid
   :type: int


   
   Get or set the Coordinate system ID to define acceleration in local coordinate system.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'LOAD'


.. py:attribute:: subkeyword
   :value: 'BODY_Y'






