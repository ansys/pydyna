





:class:`LoadBlast`
==================


.. py:class:: load_blast.LoadBlast(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA LOAD_BLAST keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: LoadBlast

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~wgt`
            - Get or set the Equivalent mass of TNT.
          * - :py:attr:`~xbo`
            - Get or set the x-coordinate of point of explosion.
          * - :py:attr:`~ybo`
            - Get or set the y-coordinate of point of explosion.
          * - :py:attr:`~zbo`
            - Get or set the z-coordinate of point of explosion.
          * - :py:attr:`~tbo`
            - Get or set the Time-zero of explosion.
          * - :py:attr:`~iunit`
            - Get or set the Unit conversion flag:
          * - :py:attr:`~isurf`
            - Get or set the Type of burst:,
          * - :py:attr:`~cfm`
            - Get or set the Conversion factor - pounds per LS-DYNA mass unit.
          * - :py:attr:`~cfl`
            - Get or set the Conversion factor - feet per LS-DYNA length units.
          * - :py:attr:`~cft`
            - Get or set the Conversion factor - milliseconds per LS-DYNA time unit.
          * - :py:attr:`~cfp`
            - Get or set the Conversion factor - psi per LS-DYNA pressure unit.
          * - :py:attr:`~death`
            - Get or set the Death time. Blast pressures are deactivated at this time.


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

    from load_blast import LoadBlast

Property detail
---------------

.. py:property:: wgt
   :type: Optional[float]


   
   Get or set the Equivalent mass of TNT.
















   ..
       !! processed by numpydoc !!

.. py:property:: xbo
   :type: float


   
   Get or set the x-coordinate of point of explosion.
















   ..
       !! processed by numpydoc !!

.. py:property:: ybo
   :type: float


   
   Get or set the y-coordinate of point of explosion.
















   ..
       !! processed by numpydoc !!

.. py:property:: zbo
   :type: float


   
   Get or set the z-coordinate of point of explosion.
















   ..
       !! processed by numpydoc !!

.. py:property:: tbo
   :type: float


   
   Get or set the Time-zero of explosion.
















   ..
       !! processed by numpydoc !!

.. py:property:: iunit
   :type: int


   
   Get or set the Unit conversion flag:
   EQ.1: feet, pounds, seconds, psi,
   EQ.2: meters, kilograms, seconds, Pascals (default),
   EQ.3: inch, dozens of slugs, seconds, psi,
   EQ.4: centimeters, grams, microseconds, Megabars,
   EQ.5: user conversions will be supplied (see Card 2).
















   ..
       !! processed by numpydoc !!

.. py:property:: isurf
   :type: int


   
   Get or set the Type of burst:,
   EQ.1: surface burst - hemispherical charge situated on the surface,
   EQ.2: air burst - spherical charge at least one charge diameter away from the surface (default).
















   ..
       !! processed by numpydoc !!

.. py:property:: cfm
   :type: float


   
   Get or set the Conversion factor - pounds per LS-DYNA mass unit.
















   ..
       !! processed by numpydoc !!

.. py:property:: cfl
   :type: float


   
   Get or set the Conversion factor - feet per LS-DYNA length units.
















   ..
       !! processed by numpydoc !!

.. py:property:: cft
   :type: float


   
   Get or set the Conversion factor - milliseconds per LS-DYNA time unit.
















   ..
       !! processed by numpydoc !!

.. py:property:: cfp
   :type: float


   
   Get or set the Conversion factor - psi per LS-DYNA pressure unit.
















   ..
       !! processed by numpydoc !!

.. py:property:: death
   :type: float


   
   Get or set the Death time. Blast pressures are deactivated at this time.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'LOAD'


.. py:attribute:: subkeyword
   :value: 'BLAST'






