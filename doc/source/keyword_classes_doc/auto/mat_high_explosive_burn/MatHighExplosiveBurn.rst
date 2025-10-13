





:class:`MatHighExplosiveBurn`
=============================


.. py:class:: mat_high_explosive_burn.MatHighExplosiveBurn(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_HIGH_EXPLOSIVE_BURN keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatHighExplosiveBurn

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification. A unique number has to be used.
          * - :py:attr:`~ro`
            - Get or set the Mass density.
          * - :py:attr:`~d`
            - Get or set the Detonation velocity.
          * - :py:attr:`~pcj`
            - Get or set the Chapman-Jouget pressure.
          * - :py:attr:`~beta`
            - Get or set the Beta burn flag, BETA:
          * - :py:attr:`~k`
            - Get or set the Bulk modulus (BETA=2.0 only).
          * - :py:attr:`~g`
            - Get or set the Shear modulus (BETA=2.0 only).
          * - :py:attr:`~sigy`
            - Get or set the Yield stress (BETA=2.0 only).
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

    from mat_high_explosive_burn import MatHighExplosiveBurn

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification. A unique number has to be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Mass density.
















   ..
       !! processed by numpydoc !!

.. py:property:: d
   :type: Optional[float]


   
   Get or set the Detonation velocity.
















   ..
       !! processed by numpydoc !!

.. py:property:: pcj
   :type: Optional[float]


   
   Get or set the Chapman-Jouget pressure.
















   ..
       !! processed by numpydoc !!

.. py:property:: beta
   :type: float


   
   Get or set the Beta burn flag, BETA:
   EQ.0.0: beta + programmed burn (default),
   EQ.1.0: beta burn only,
   EQ.2.0: programmed burn only.
















   ..
       !! processed by numpydoc !!

.. py:property:: k
   :type: Optional[float]


   
   Get or set the Bulk modulus (BETA=2.0 only).
















   ..
       !! processed by numpydoc !!

.. py:property:: g
   :type: Optional[float]


   
   Get or set the Shear modulus (BETA=2.0 only).
















   ..
       !! processed by numpydoc !!

.. py:property:: sigy
   :type: Optional[float]


   
   Get or set the Yield stress (BETA=2.0 only).
















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
   :value: 'HIGH_EXPLOSIVE_BURN'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





