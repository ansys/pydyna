





:class:`MatKinematicHardeningTransverselyAnisotropic`
=====================================================


.. py:class:: mat_kinematic_hardening_transversely_anisotropic.MatKinematicHardeningTransverselyAnisotropic(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_KINEMATIC_HARDENING_TRANSVERSELY_ANISOTROPIC keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatKinematicHardeningTransverselyAnisotropic

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification.  A unique number or label must be specified.
          * - :py:attr:`~ro`
            - Get or set the Mass density.
          * - :py:attr:`~e`
            - Get or set the Young's Modulus.
          * - :py:attr:`~pr`
            - Get or set the Poisson's ratio.
          * - :py:attr:`~r`
            - Get or set the Anisotropic hardening parameter.
          * - :py:attr:`~hclid`
            - Get or set the Load curve ID in keyword *DEFINE_CURVE, where true strain and true stress relationship is characterized. Used in conjunction with variable OPT.
          * - :py:attr:`~opt`
            - Get or set the Error calculation flag. When OPT=2, the load curve ID is the true stress-strain curve from uniaxial tension. LS-DYNA will perform error calculation based on this curve.
          * - :py:attr:`~cb`
            - Get or set the The uppercase B defined in the following equations.
          * - :py:attr:`~y`
            - Get or set the Hardening parameter as defined in the following equations.
          * - :py:attr:`~sc`
            - Get or set the The lowercase c defined in the following equations.
          * - :py:attr:`~k`
            - Get or set the Hardening parameter as defined in the following equations.
          * - :py:attr:`~rsat`
            - Get or set the Hardening parameter as defined in the following equations.
          * - :py:attr:`~sb`
            - Get or set the Anisotropic parameter associated with work-hardening stagnation
          * - :py:attr:`~h`
            - Get or set the Formulation option:
          * - :py:attr:`~sc2`
            - Get or set the Formulation option:
          * - :py:attr:`~ea`
            - Get or set the Variable controlling the change of Young's modulus,    in the following equations.
          * - :py:attr:`~coe`
            - Get or set the Variable controlling the change of Young's modulus,   in the following equations.
          * - :py:attr:`~iopt`
            - Get or set the Modified kinematic hardening rule flag:
          * - :py:attr:`~c1`
            - Get or set the Constants used to modify R:
          * - :py:attr:`~c2`
            - Get or set the Constants used to modify R:
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

    from mat_kinematic_hardening_transversely_anisotropic import MatKinematicHardeningTransverselyAnisotropic

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification.  A unique number or label must be specified.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Mass density.
















   ..
       !! processed by numpydoc !!

.. py:property:: e
   :type: Optional[float]


   
   Get or set the Young's Modulus.
















   ..
       !! processed by numpydoc !!

.. py:property:: pr
   :type: Optional[float]


   
   Get or set the Poisson's ratio.
















   ..
       !! processed by numpydoc !!

.. py:property:: r
   :type: Optional[float]


   
   Get or set the Anisotropic hardening parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: hclid
   :type: Optional[int]


   
   Get or set the Load curve ID in keyword *DEFINE_CURVE, where true strain and true stress relationship is characterized. Used in conjunction with variable OPT.
















   ..
       !! processed by numpydoc !!

.. py:property:: opt
   :type: Optional[int]


   
   Get or set the Error calculation flag. When OPT=2, the load curve ID is the true stress-strain curve from uniaxial tension. LS-DYNA will perform error calculation based on this curve.
















   ..
       !! processed by numpydoc !!

.. py:property:: cb
   :type: Optional[float]


   
   Get or set the The uppercase B defined in the following equations.
















   ..
       !! processed by numpydoc !!

.. py:property:: y
   :type: Optional[float]


   
   Get or set the Hardening parameter as defined in the following equations.
















   ..
       !! processed by numpydoc !!

.. py:property:: sc
   :type: Optional[float]


   
   Get or set the The lowercase c defined in the following equations.
















   ..
       !! processed by numpydoc !!

.. py:property:: k
   :type: Optional[float]


   
   Get or set the Hardening parameter as defined in the following equations.
















   ..
       !! processed by numpydoc !!

.. py:property:: rsat
   :type: Optional[float]


   
   Get or set the Hardening parameter as defined in the following equations.
















   ..
       !! processed by numpydoc !!

.. py:property:: sb
   :type: Optional[float]


   
   Get or set the Anisotropic parameter associated with work-hardening stagnation
















   ..
       !! processed by numpydoc !!

.. py:property:: h
   :type: Optional[float]


   
   Get or set the Formulation option:
   EQ.0.0: Maxwell (default),
   EQ.1.0: Kelvin.
















   ..
       !! processed by numpydoc !!

.. py:property:: sc2
   :type: Optional[float]


   
   Get or set the Formulation option:
   EQ.0.0: Maxwell (default),
   EQ.1.0: Kelvin.
















   ..
       !! processed by numpydoc !!

.. py:property:: ea
   :type: Optional[float]


   
   Get or set the Variable controlling the change of Young's modulus,    in the following equations.
















   ..
       !! processed by numpydoc !!

.. py:property:: coe
   :type: Optional[float]


   
   Get or set the Variable controlling the change of Young's modulus,   in the following equations.
















   ..
       !! processed by numpydoc !!

.. py:property:: iopt
   :type: int


   
   Get or set the Modified kinematic hardening rule flag:
   EQ.0:  Original Yoshida formulation,
   EQ.1:  Modified formulation.
















   ..
       !! processed by numpydoc !!

.. py:property:: c1
   :type: Optional[float]


   
   Get or set the Constants used to modify R:
















   ..
       !! processed by numpydoc !!

.. py:property:: c2
   :type: Optional[float]


   
   Get or set the Constants used to modify R:
















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
   :value: 'KINEMATIC_HARDENING_TRANSVERSELY_ANISOTROPIC'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





