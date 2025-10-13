





:class:`MatSphViscous`
======================


.. py:class:: mat_sph_viscous.MatSphViscous(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_SPH_VISCOUS keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatSphViscous

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
          * - :py:attr:`~pc`
            - Get or set the Pressure cutoff (<= 0.0).
          * - :py:attr:`~mulo`
            - Get or set the There are 4 possible cases (See Remark 1):
          * - :py:attr:`~muhi`
            - Get or set the There are 2 possible cases:
          * - :py:attr:`~rk`
            - Get or set the Variable dynamic viscosity multiplier. See Remark 6..
          * - :py:attr:`~rc`
            - Get or set the Option for Cross viscosity model: See Remark 7.
          * - :py:attr:`~rn`
            - Get or set the Variable dynamic viscosity exponent. See Remark 6.
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

    from mat_sph_viscous import MatSphViscous

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

.. py:property:: pc
   :type: Optional[float]


   
   Get or set the Pressure cutoff (<= 0.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: mulo
   :type: Optional[float]


   
   Get or set the There are 4 possible cases (See Remark 1):
   1. If MULO = 0.0, then inviscid fluid is assumed.
   2. If MULO > 0.0, and MUHI = 0.0 or is not defined, then
   this is the traditional constant dynamic viscosity coefficient 𝜇.
   3. If MULO > 0.0, and MUHI > 0.0, then MULO and MUHI
   are lower and upper viscosity limit values for a powerlaw-like variable viscosity model.
   4. If MULO is negative (for example, MULO = -1), then a
   user-input data load curve (with LCID = 1) defining dynamic
   viscosity as a function of equivalent strain rate is used.
















   ..
       !! processed by numpydoc !!

.. py:property:: muhi
   :type: Optional[float]


   
   Get or set the There are 2 possible cases:
   5. If MUHI < 0.0, then the viscosity can be defined by the
   user in the file dyn21.F with a routine called f3dm9sph_userdefin.
   The file is part of the general usermat package.
   6. If MUHI > 0.0, then this is the upper dynamic viscosity
   limit (default = 0.0). This is defined only if RK and RN
   are defined for the variable viscosity case.
















   ..
       !! processed by numpydoc !!

.. py:property:: rk
   :type: Optional[float]


   
   Get or set the Variable dynamic viscosity multiplier. See Remark 6..
















   ..
       !! processed by numpydoc !!

.. py:property:: rc
   :type: Optional[float]


   
   Get or set the Option for Cross viscosity model: See Remark 7.
   RC > 0.0: Cross viscosity model will be used (overwrite all
   other options), values of MULO, MUHI, RK and RN
   will be used in the Cross viscosity model. See Remark 7.
   RC ≤ 0.0: other viscosity model (decided based on above variables) will be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: rn
   :type: Optional[float]


   
   Get or set the Variable dynamic viscosity exponent. See Remark 6.
















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
   :value: 'SPH_VISCOUS'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





