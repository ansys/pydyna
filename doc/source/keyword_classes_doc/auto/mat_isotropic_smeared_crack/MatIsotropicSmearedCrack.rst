





:class:`MatIsotropicSmearedCrack`
=================================


.. py:class:: mat_isotropic_smeared_crack.MatIsotropicSmearedCrack(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_ISOTROPIC_SMEARED_CRACK keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatIsotropicSmearedCrack

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
          * - :py:attr:`~e`
            - Get or set the Young's modulus.
          * - :py:attr:`~pr`
            - Get or set the Poisson's ratio.
          * - :py:attr:`~ispl`
            - Get or set the Failure option:
          * - :py:attr:`~sigy`
            - Get or set the Yield stress.
          * - :py:attr:`~gk`
            - Get or set the Critical energy release rate
          * - :py:attr:`~sr`
            - Get or set the Strength ratio.
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

    from mat_isotropic_smeared_crack import MatIsotropicSmearedCrack

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

.. py:property:: e
   :type: Optional[float]


   
   Get or set the Young's modulus.
















   ..
       !! processed by numpydoc !!

.. py:property:: pr
   :type: Optional[float]


   
   Get or set the Poisson's ratio.
















   ..
       !! processed by numpydoc !!

.. py:property:: ispl
   :type: Optional[int]


   
   Get or set the Failure option:
   EQ.0: Maximum principal stress criterion
   EQ.5: Smeared crack model
   EQ.6: Damage model based on modified von Mises strain
















   ..
       !! processed by numpydoc !!

.. py:property:: sigy
   :type: Optional[float]


   
   Get or set the Yield stress.
















   ..
       !! processed by numpydoc !!

.. py:property:: gk
   :type: Optional[float]


   
   Get or set the Critical energy release rate
















   ..
       !! processed by numpydoc !!

.. py:property:: sr
   :type: Optional[float]


   
   Get or set the Strength ratio.
















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
   :value: 'ISOTROPIC_SMEARED_CRACK'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





