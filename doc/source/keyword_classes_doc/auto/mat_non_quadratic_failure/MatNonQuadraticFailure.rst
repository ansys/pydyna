





:class:`MatNonQuadraticFailure`
===============================


.. py:class:: mat_non_quadratic_failure.MatNonQuadraticFailure(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_NON_QUADRATIC_FAILURE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatNonQuadraticFailure

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
          * - :py:attr:`~sigy`
            - Get or set the Initial yield stress.
          * - :py:attr:`~a`
            - Get or set the Exponent of Hershey yield criterion.
          * - :py:attr:`~ksi`
            - Get or set the Coefficient governing critical strain increment for substepping.
          * - :py:attr:`~theta1`
            - Get or set the Initial hardening modulus of R_i.
          * - :py:attr:`~q1`
            - Get or set the Saturation value of R_i.
          * - :py:attr:`~theta2`
            - Get or set the Initial hardening modulus of R_i.
          * - :py:attr:`~q2`
            - Get or set the Saturation value of R_i.
          * - :py:attr:`~theta3`
            - Get or set the Initial hardening modulus of R_i.
          * - :py:attr:`~q3`
            - Get or set the Saturation value of R_i.
          * - :py:attr:`~cs`
            - Get or set the Rate sensitivity of flow stress.
          * - :py:attr:`~pdots`
            - Get or set the Reference strain rate.
          * - :py:attr:`~dcrit`
            - Get or set the Critical damage.
          * - :py:attr:`~wcb`
            - Get or set the Constant defining the damage evolution.
          * - :py:attr:`~wcl`
            - Get or set the Constant defining the damage evolution.
          * - :py:attr:`~wcs`
            - Get or set the Constant defining the damage evolution.
          * - :py:attr:`~cc`
            - Get or set the Constant defining the damage evolution.
          * - :py:attr:`~phi`
            - Get or set the Constant defining the damage evolution.
          * - :py:attr:`~gamma`
            - Get or set the Constant defining the damage evolution.
          * - :py:attr:`~thick`
            - Get or set the Element thickness if using shell formulation 16.
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

    from mat_non_quadratic_failure import MatNonQuadraticFailure

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

.. py:property:: sigy
   :type: Optional[float]


   
   Get or set the Initial yield stress.
















   ..
       !! processed by numpydoc !!

.. py:property:: a
   :type: Optional[float]


   
   Get or set the Exponent of Hershey yield criterion.
















   ..
       !! processed by numpydoc !!

.. py:property:: ksi
   :type: Optional[float]


   
   Get or set the Coefficient governing critical strain increment for substepping.
















   ..
       !! processed by numpydoc !!

.. py:property:: theta1
   :type: Optional[float]


   
   Get or set the Initial hardening modulus of R_i.
















   ..
       !! processed by numpydoc !!

.. py:property:: q1
   :type: Optional[float]


   
   Get or set the Saturation value of R_i.
















   ..
       !! processed by numpydoc !!

.. py:property:: theta2
   :type: Optional[float]


   
   Get or set the Initial hardening modulus of R_i.
















   ..
       !! processed by numpydoc !!

.. py:property:: q2
   :type: Optional[float]


   
   Get or set the Saturation value of R_i.
















   ..
       !! processed by numpydoc !!

.. py:property:: theta3
   :type: Optional[float]


   
   Get or set the Initial hardening modulus of R_i.
















   ..
       !! processed by numpydoc !!

.. py:property:: q3
   :type: Optional[float]


   
   Get or set the Saturation value of R_i.
















   ..
       !! processed by numpydoc !!

.. py:property:: cs
   :type: Optional[float]


   
   Get or set the Rate sensitivity of flow stress.
















   ..
       !! processed by numpydoc !!

.. py:property:: pdots
   :type: Optional[float]


   
   Get or set the Reference strain rate.
















   ..
       !! processed by numpydoc !!

.. py:property:: dcrit
   :type: Optional[float]


   
   Get or set the Critical damage.
















   ..
       !! processed by numpydoc !!

.. py:property:: wcb
   :type: Optional[float]


   
   Get or set the Constant defining the damage evolution.
















   ..
       !! processed by numpydoc !!

.. py:property:: wcl
   :type: Optional[float]


   
   Get or set the Constant defining the damage evolution.
















   ..
       !! processed by numpydoc !!

.. py:property:: wcs
   :type: Optional[float]


   
   Get or set the Constant defining the damage evolution.
















   ..
       !! processed by numpydoc !!

.. py:property:: cc
   :type: Optional[float]


   
   Get or set the Constant defining the damage evolution.
















   ..
       !! processed by numpydoc !!

.. py:property:: phi
   :type: Optional[float]


   
   Get or set the Constant defining the damage evolution.
















   ..
       !! processed by numpydoc !!

.. py:property:: gamma
   :type: Optional[float]


   
   Get or set the Constant defining the damage evolution.
















   ..
       !! processed by numpydoc !!

.. py:property:: thick
   :type: Optional[float]


   
   Get or set the Element thickness if using shell formulation 16.
















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
   :value: 'NON_QUADRATIC_FAILURE'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





