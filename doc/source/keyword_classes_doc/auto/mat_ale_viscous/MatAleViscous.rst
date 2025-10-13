





:class:`MatAleViscous`
======================


.. py:class:: mat_ale_viscous.MatAleViscous(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_ALE_VISCOUS keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatAleViscous

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
          * - :py:attr:`~pc`
            - Get or set the Pressure cutoff
          * - :py:attr:`~mulo`
            - Get or set the There are 4 possible cases (See remark 1):
          * - :py:attr:`~muhi`
            - Get or set the Upper dynamic viscosity limit (default=0.0).  This is defined only if RK and RN are defined for the variable viscosity case.
          * - :py:attr:`~rk`
            - Get or set the Variable dynamic viscosity multiplier.
          * - :py:attr:`~rn`
            - Get or set the Variable dynamic viscosity exponent.
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

    from mat_ale_viscous import MatAleViscous

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

.. py:property:: pc
   :type: Optional[float]


   
   Get or set the Pressure cutoff
















   ..
       !! processed by numpydoc !!

.. py:property:: mulo
   :type: Optional[float]


   
   Get or set the There are 4 possible cases (See remark 1):
   1) If MULO=0.0, then inviscid fluid is assumed.
   2) If MULO > 0.0, and MUHI=0.0 or is not defined, then this is the traditional constant dynamic viscosity coefficient.
   3) If MULO > 0.0, and MUHI > 0.0, then MULO and MUHI are lower and upper viscosity limit values for a power-law-like variable viscosity model.
   4) If MULO is negative (for example, MULO = -1), then a user-input data load curve (with LCID=1) defining dynamic viscosity as a function of equivalent strain rate is used
















   ..
       !! processed by numpydoc !!

.. py:property:: muhi
   :type: Optional[float]


   
   Get or set the Upper dynamic viscosity limit (default=0.0).  This is defined only if RK and RN are defined for the variable viscosity case.
















   ..
       !! processed by numpydoc !!

.. py:property:: rk
   :type: Optional[float]


   
   Get or set the Variable dynamic viscosity multiplier.
















   ..
       !! processed by numpydoc !!

.. py:property:: rn
   :type: Optional[float]


   
   Get or set the Variable dynamic viscosity exponent.
















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
   :value: 'ALE_VISCOUS'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





