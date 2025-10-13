





:class:`MatWinfrithConcreteReinforcement`
=========================================


.. py:class:: mat_winfrith_concrete_reinforcement.MatWinfrithConcreteReinforcement(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_WINFRITH_CONCRETE_REINFORCEMENT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatWinfrithConcreteReinforcement

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~eid1_unused`
            - Get or set the First element ID in group. Left blank to active card of option 2.
          * - :py:attr:`~eid2_pid`
            - Get or set the Last element ID in group.
          * - :py:attr:`~inc_axis`
            - Get or set the Element increment for generation.
          * - :py:attr:`~xr_coor`
            - Get or set the X-reinforcement quantity (for bars running parallel to global x-axis).
          * - :py:attr:`~yr_rqa`
            - Get or set the Y-reinforcement quantity (for bars running parallel to global y-axis).
          * - :py:attr:`~zr_rqb`
            - Get or set the Z-reinforcement quantity (for bars running parallel to global z-axis).
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

    from mat_winfrith_concrete_reinforcement import MatWinfrithConcreteReinforcement

Property detail
---------------

.. py:property:: eid1_unused
   :type: Optional[int]


   
   Get or set the First element ID in group. Left blank to active card of option 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: eid2_pid
   :type: Optional[int]


   
   Get or set the Last element ID in group.
   PID: Part ID of reinforced elements
















   ..
       !! processed by numpydoc !!

.. py:property:: inc_axis
   :type: Optional[int]


   
   Get or set the Element increment for generation.
   AXIS: Axis normal to layer:
   EQ.1: A and B are parallel to global Y and Z, respectively (default),
   EQ.2: A and B are parallel to global Z and X, respectively,
   EQ.3: A and B are parallel to global X and Y, respectively.
















   ..
       !! processed by numpydoc !!

.. py:property:: xr_coor
   :type: Optional[float]


   
   Get or set the X-reinforcement quantity (for bars running parallel to global x-axis).
   COOR: Coordinate location of layer
   If AXIS.EQ.1: X-coordinate ,
   If AXIS.EQ.2: Y-coordinate,
   If AXIS.EQ.3: Z-coordinate
















   ..
       !! processed by numpydoc !!

.. py:property:: yr_rqa
   :type: Optional[float]


   
   Get or set the Y-reinforcement quantity (for bars running parallel to global y-axis).
   RQA: Reinforcement quantity (A).
















   ..
       !! processed by numpydoc !!

.. py:property:: zr_rqb
   :type: Optional[float]


   
   Get or set the Z-reinforcement quantity (for bars running parallel to global z-axis).
   RQB: Reinforcement quantity (B).
















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
   :value: 'MAT'


.. py:attribute:: subkeyword
   :value: 'WINFRITH_CONCRETE_REINFORCEMENT'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





