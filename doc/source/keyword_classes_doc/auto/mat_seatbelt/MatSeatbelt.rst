





:class:`MatSeatbelt`
====================


.. py:class:: mat_seatbelt.MatSeatbelt(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_SEATBELT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatSeatbelt

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Belt material number. A unique number has to be used.
          * - :py:attr:`~mpul`
            - Get or set the Mass per unit length.
          * - :py:attr:`~llcid`
            - Get or set the Load curve identification for loading (strain/force with engineering strain).
          * - :py:attr:`~ulcid`
            - Get or set the Load curve identification for unloading (strain/force with engineering strain).
          * - :py:attr:`~lmin`
            - Get or set the Minimum length (for elements connected to slip rings and retractors).
          * - :py:attr:`~cse`
            - Get or set the Optional compressive stress elimination option which applies to shell elements only (default 0.0):
          * - :py:attr:`~damp`
            - Get or set the Optional Rayleigh damping coefficient, which applies to shell elements only.  A coefficient value of 0.10 is the default corresponding to 10% of critical damping.  Sometimes smaller or larger values work better.
          * - :py:attr:`~e`
            - Get or set the Young's modulus for bending/compression stiffness, when positive the optional card is invoked.
          * - :py:attr:`~a`
            - Get or set the Cross sectional area for bending/compression stiffness
          * - :py:attr:`~i`
            - Get or set the Area moment of inertia for bending/compression stiffness
          * - :py:attr:`~j`
            - Get or set the Torsional constant for bending/compression stiffness
          * - :py:attr:`~as_`
            - Get or set the Shear area for bending/compression stiffness
          * - :py:attr:`~f`
            - Get or set the Maximum force in compression/tension.
          * - :py:attr:`~m`
            - Get or set the Maximum torque
          * - :py:attr:`~r`
            - Get or set the Rotational mass scaling factor
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

    from mat_seatbelt import MatSeatbelt

Property detail
---------------

.. py:property:: mid
   :type: int


   
   Get or set the Belt material number. A unique number has to be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: mpul
   :type: Optional[float]


   
   Get or set the Mass per unit length.
















   ..
       !! processed by numpydoc !!

.. py:property:: llcid
   :type: int


   
   Get or set the Load curve identification for loading (strain/force with engineering strain).
















   ..
       !! processed by numpydoc !!

.. py:property:: ulcid
   :type: int


   
   Get or set the Load curve identification for unloading (strain/force with engineering strain).
















   ..
       !! processed by numpydoc !!

.. py:property:: lmin
   :type: Optional[float]


   
   Get or set the Minimum length (for elements connected to slip rings and retractors).
















   ..
       !! processed by numpydoc !!

.. py:property:: cse
   :type: float


   
   Get or set the Optional compressive stress elimination option which applies to shell elements only (default 0.0):
   EQ.0.0: eliminate compressive stresses in shell fabric
   EQ.1.0: don't eliminate compressive stresses.  This option should not be used if retractors and sliprings are present in the model.
   EQ.2.0: whether or not compressive stress is eliminated is decided by ls-dyna automatically, recommended for shell belt.
















   ..
       !! processed by numpydoc !!

.. py:property:: damp
   :type: Optional[float]


   
   Get or set the Optional Rayleigh damping coefficient, which applies to shell elements only.  A coefficient value of 0.10 is the default corresponding to 10% of critical damping.  Sometimes smaller or larger values work better.
















   ..
       !! processed by numpydoc !!

.. py:property:: e
   :type: Optional[float]


   
   Get or set the Young's modulus for bending/compression stiffness, when positive the optional card is invoked.
















   ..
       !! processed by numpydoc !!

.. py:property:: a
   :type: Optional[float]


   
   Get or set the Cross sectional area for bending/compression stiffness
















   ..
       !! processed by numpydoc !!

.. py:property:: i
   :type: Optional[float]


   
   Get or set the Area moment of inertia for bending/compression stiffness
















   ..
       !! processed by numpydoc !!

.. py:property:: j
   :type: Optional[float]


   
   Get or set the Torsional constant for bending/compression stiffness
















   ..
       !! processed by numpydoc !!

.. py:property:: as_
   :type: Optional[float]


   
   Get or set the Shear area for bending/compression stiffness
















   ..
       !! processed by numpydoc !!

.. py:property:: f
   :type: float


   
   Get or set the Maximum force in compression/tension.
















   ..
       !! processed by numpydoc !!

.. py:property:: m
   :type: float


   
   Get or set the Maximum torque
















   ..
       !! processed by numpydoc !!

.. py:property:: r
   :type: float


   
   Get or set the Rotational mass scaling factor
















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
   :value: 'SEATBELT'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





