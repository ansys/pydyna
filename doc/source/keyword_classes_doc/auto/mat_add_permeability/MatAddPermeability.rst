





:class:`MatAddPermeability`
===========================


.. py:class:: mat_add_permeability.MatAddPermeability(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_ADD_PERMEABILITY keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatAddPermeability

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification - must be same as the structural material.
          * - :py:attr:`~perm`
            - Get or set the Permeability or load curve ID defining permeability, depending on the definition of PMTYP below. If PERMY and PERMZ are nonzero, then PERM gives the permeability in the global X direction.  See Remark 3.
          * - :py:attr:`~permy`
            - Get or set the Optional permeability or load curve ID defining permeability in the global Y direction, depending on the definition of PMTYP below
          * - :py:attr:`~permz`
            - Get or set the Optional permeability or load curve ID defining permeability in the global Z direction, depending on the definition of PMTYP below
          * - :py:attr:`~thexp`
            - Get or set the Undrained volumetric thermal expansion coefficient (see Remark 2):
          * - :py:attr:`~lckz`
            - Get or set the Load curve giving factor on PERM as a function of z-coordinate
          * - :py:attr:`~pmtyp`
            - Get or set the Permeability definition type:
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

    from mat_add_permeability import MatAddPermeability

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification - must be same as the structural material.
















   ..
       !! processed by numpydoc !!

.. py:property:: perm
   :type: Optional[float]


   
   Get or set the Permeability or load curve ID defining permeability, depending on the definition of PMTYP below. If PERMY and PERMZ are nonzero, then PERM gives the permeability in the global X direction.  See Remark 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: permy
   :type: Optional[int]


   
   Get or set the Optional permeability or load curve ID defining permeability in the global Y direction, depending on the definition of PMTYP below
















   ..
       !! processed by numpydoc !!

.. py:property:: permz
   :type: Optional[int]


   
   Get or set the Optional permeability or load curve ID defining permeability in the global Z direction, depending on the definition of PMTYP below
















   ..
       !! processed by numpydoc !!

.. py:property:: thexp
   :type: Optional[float]


   
   Get or set the Undrained volumetric thermal expansion coefficient (see Remark 2):
   GE.0.0: Constant undrained volumetric thermal expansion coefficient
   LT.0.0 : | THEXP | is the ID of a load curve giving the thermal expansion coefficient(y - axis) as a function of temperature(x - axis).
















   ..
       !! processed by numpydoc !!

.. py:property:: lckz
   :type: Optional[int]


   
   Get or set the Load curve giving factor on PERM as a function of z-coordinate
















   ..
       !! processed by numpydoc !!

.. py:property:: pmtyp
   :type: int


   
   Get or set the Permeability definition type:
   EQ.0:   PERM is a constant.
   EQ.1 : PERM is a load curve ID giving permeability(y - axis) as a function of the volume ratio of current volume to volume in the stress free state(x - axis).
   EQ.2 : PERM is a load curve ID giving permeability(y - axis) as a function of effective plastic strain(x - axis) of materials other than MAT_072R3.For MAT_072R3, the x - axis is the output selector specified by NOUT; see* MAT_072R3.
   EQ.3:   PERM is a load curve ID giving permeability(y - axis) as a function of effective pressure(x - axis) which is positive when in compression.
















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
   :value: 'ADD_PERMEABILITY'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





