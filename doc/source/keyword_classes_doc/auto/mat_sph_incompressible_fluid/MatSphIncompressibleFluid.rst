





:class:`MatSphIncompressibleFluid`
==================================


.. py:class:: mat_sph_incompressible_fluid.MatSphIncompressibleFluid(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_SPH_INCOMPRESSIBLE_FLUID keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatSphIncompressibleFluid

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification. A unique number or label must be specified.
          * - :py:attr:`~ro`
            - Get or set the Mass density.
          * - :py:attr:`~mu`
            - Get or set the Dynamic viscosity.
          * - :py:attr:`~gamma1`
            - Get or set the Numerical surface tension coefficient. For water,we recommend a coefficient of γ_1=1000 m/s^2. GAMMA1 is only used if IMAT = 0 in *CONTROL_SPH_INCOMPRESSIBLE.
          * - :py:attr:`~gamma2`
            - Get or set the Numerical surface tension coefficient. For water, we recommend a coefficient of γ_2=1 m/s^2. GAMMA2 is only used if IMAT = 0 in *CONTROL_SPH_INCOMPRESSIBLE
          * - :py:attr:`~stens`
            - Get or set the Physical surface tension coefficient. Only used if IMAT=1 in *CONTROL_SPH_INCOMPRESSIBLE.
          * - :py:attr:`~cp`
            - Get or set the Fluid specific heat. Used to calculate heat transfer coefficients if IHTC=1 in *CONTROL_SPH_INCOMPRESSIBLE.
          * - :py:attr:`~lambda_`
            - Get or set the Fluid thermal conductivity. Used to calculate heat transfer coefficients if IHTC=1 in *CONTROL_SPH_INCOMPRESSIBLE.
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

    from mat_sph_incompressible_fluid import MatSphIncompressibleFluid

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification. A unique number or label must be specified.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Mass density.
















   ..
       !! processed by numpydoc !!

.. py:property:: mu
   :type: Optional[float]


   
   Get or set the Dynamic viscosity.
















   ..
       !! processed by numpydoc !!

.. py:property:: gamma1
   :type: Optional[float]


   
   Get or set the Numerical surface tension coefficient. For water,we recommend a coefficient of γ_1=1000 m/s^2. GAMMA1 is only used if IMAT = 0 in *CONTROL_SPH_INCOMPRESSIBLE.
















   ..
       !! processed by numpydoc !!

.. py:property:: gamma2
   :type: Optional[float]


   
   Get or set the Numerical surface tension coefficient. For water, we recommend a coefficient of γ_2=1 m/s^2. GAMMA2 is only used if IMAT = 0 in *CONTROL_SPH_INCOMPRESSIBLE
















   ..
       !! processed by numpydoc !!

.. py:property:: stens
   :type: Optional[float]


   
   Get or set the Physical surface tension coefficient. Only used if IMAT=1 in *CONTROL_SPH_INCOMPRESSIBLE.
















   ..
       !! processed by numpydoc !!

.. py:property:: cp
   :type: Optional[float]


   
   Get or set the Fluid specific heat. Used to calculate heat transfer coefficients if IHTC=1 in *CONTROL_SPH_INCOMPRESSIBLE.
















   ..
       !! processed by numpydoc !!

.. py:property:: lambda_
   :type: Optional[float]


   
   Get or set the Fluid thermal conductivity. Used to calculate heat transfer coefficients if IHTC=1 in *CONTROL_SPH_INCOMPRESSIBLE.
















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
   :value: 'SPH_INCOMPRESSIBLE_FLUID'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





