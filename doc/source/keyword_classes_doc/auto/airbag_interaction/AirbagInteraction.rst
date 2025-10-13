





:class:`AirbagInteraction`
==========================


.. py:class:: airbag_interaction.AirbagInteraction(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA AIRBAG_INTERACTION keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: AirbagInteraction

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ab1`
            - Get or set the First airbag ID, as defined on *AIRBAG card.
          * - :py:attr:`~ab2`
            - Get or set the Second airbag ID, as defined on *AIRBAG card.
          * - :py:attr:`~area`
            - Get or set the Orifice area between connected bags.
          * - :py:attr:`~sf`
            - Get or set the Shape factor.
          * - :py:attr:`~pid`
            - Get or set the Optional part ID of the partition between the interacting control volumes. AREA is based on this part ID.
          * - :py:attr:`~lcid`
            - Get or set the Load curve ID defining mass flow rate versus pressure difference, see *DEFINE_CURVE. If LCID is defined AREA, SF and PID are ignored.
          * - :py:attr:`~iflow`
            - Get or set the Flow direction.


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

    from airbag_interaction import AirbagInteraction

Property detail
---------------

.. py:property:: ab1
   :type: Optional[int]


   
   Get or set the First airbag ID, as defined on *AIRBAG card.
















   ..
       !! processed by numpydoc !!

.. py:property:: ab2
   :type: Optional[int]


   
   Get or set the Second airbag ID, as defined on *AIRBAG card.
















   ..
       !! processed by numpydoc !!

.. py:property:: area
   :type: Optional[float]


   
   Get or set the Orifice area between connected bags.
   LT.0.0: |AREA| is the load curve ID defining the orifice area as a function of absolute pressure,
   EQ.0.0: AREA is taken as the surface area of the part ID defined below.
















   ..
       !! processed by numpydoc !!

.. py:property:: sf
   :type: Optional[float]


   
   Get or set the Shape factor.
   LT.0.0: |SF| is the load curve ID defining vent orifice coefficient as a function of relative time.
















   ..
       !! processed by numpydoc !!

.. py:property:: pid
   :type: int


   
   Get or set the Optional part ID of the partition between the interacting control volumes. AREA is based on this part ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: int


   
   Get or set the Load curve ID defining mass flow rate versus pressure difference, see *DEFINE_CURVE. If LCID is defined AREA, SF and PID are ignored.
















   ..
       !! processed by numpydoc !!

.. py:property:: iflow
   :type: int


   
   Get or set the Flow direction.
   LT.0: One-way flow from AB1 to AB2 only,
   EQ.0: Two-way flow between AB1 and AB2,
   GT.0: One-way flow from AB2 to AB1 only.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'AIRBAG'


.. py:attribute:: subkeyword
   :value: 'INTERACTION'






