





:class:`LoadBeamElement`
========================


.. py:class:: load_beam_element.LoadBeamElement(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA LOAD_BEAM_ELEMENT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: LoadBeamElement

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~eid`
            - Get or set the Beam element ID, see *ELEMENT_BEAM.
          * - :py:attr:`~dal`
            - Get or set the Direction of applied load:
          * - :py:attr:`~lcid`
            - Get or set the Load curve ID, see *DEFINE_CURVE or *DEFINE_FUNCTION.
          * - :py:attr:`~sf`
            - Get or set the Load curve scale factor. This is for a simple modification of the function values of the load curve.


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

    from load_beam_element import LoadBeamElement

Property detail
---------------

.. py:property:: eid
   :type: Optional[int]


   
   Get or set the Beam element ID, see *ELEMENT_BEAM.
















   ..
       !! processed by numpydoc !!

.. py:property:: dal
   :type: int


   
   Get or set the Direction of applied load:
   EQ.1: along r-axis of beam,
   EQ.2: along s-axis of beam,
   EQ.3: along t-axis of beam.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve ID, see *DEFINE_CURVE or *DEFINE_FUNCTION.
















   ..
       !! processed by numpydoc !!

.. py:property:: sf
   :type: float


   
   Get or set the Load curve scale factor. This is for a simple modification of the function values of the load curve.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'LOAD'


.. py:attribute:: subkeyword
   :value: 'BEAM_ELEMENT'






