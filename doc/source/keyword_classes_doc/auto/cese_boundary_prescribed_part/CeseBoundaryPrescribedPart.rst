





:class:`CeseBoundaryPrescribedPart`
===================================


.. py:class:: cese_boundary_prescribed_part.CeseBoundaryPrescribedPart(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CESE_BOUNDARY_PRESCRIBED_PART keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: CeseBoundaryPrescribedPart

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~surfprt`
            - Get or set the A surface part ID referenced in *MESH_SURFACE_ELEMENT cards
          * - :py:attr:`~idcomp`
            - Get or set the For inflow boundaries in problems involving chemical reacting flows, the chemical mixture of the fluid entering the domain as defined with a *CHEMISTRY_COMPOSITION card.
          * - :py:attr:`~lc_u`
            - Get or set the Load curve ID to describe the x-component of the velocity versus time
          * - :py:attr:`~lc_v_`
            - Get or set the Load curve ID to describe the y-component of the velocity versus time.
          * - :py:attr:`~lc_w`
            - Get or set the Load curve ID to describe the z-component of the velocity versus time.
          * - :py:attr:`~lc_rho`
            - Get or set the Load curve ID to describe the density versus time.
          * - :py:attr:`~lc_p_`
            - Get or set the Load curve ID to describe the pressure versus time.
          * - :py:attr:`~lc_t`
            - Get or set the Load curve ID to describe the temperature versus time.
          * - :py:attr:`~sf_u`
            - Get or set the Scale factor for LC_U
          * - :py:attr:`~sf_v_`
            - Get or set the Scale factor for LC_V
          * - :py:attr:`~sf_w`
            - Get or set the Scale factor for LC_W
          * - :py:attr:`~sf_rho`
            - Get or set the Scale factor for LC_RHO
          * - :py:attr:`~sf_p_`
            - Get or set the Scale factor for LC_P
          * - :py:attr:`~sf_t`
            - Get or set the Scale factor for LC_T


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

    from cese_boundary_prescribed_part import CeseBoundaryPrescribedPart

Property detail
---------------

.. py:property:: surfprt
   :type: Optional[int]


   
   Get or set the A surface part ID referenced in *MESH_SURFACE_ELEMENT cards
















   ..
       !! processed by numpydoc !!

.. py:property:: idcomp
   :type: Optional[int]


   
   Get or set the For inflow boundaries in problems involving chemical reacting flows, the chemical mixture of the fluid entering the domain as defined with a *CHEMISTRY_COMPOSITION card.
















   ..
       !! processed by numpydoc !!

.. py:property:: lc_u
   :type: Optional[int]


   
   Get or set the Load curve ID to describe the x-component of the velocity versus time
















   ..
       !! processed by numpydoc !!

.. py:property:: lc_v_
   :type: Optional[int]


   
   Get or set the Load curve ID to describe the y-component of the velocity versus time.
















   ..
       !! processed by numpydoc !!

.. py:property:: lc_w
   :type: Optional[int]


   
   Get or set the Load curve ID to describe the z-component of the velocity versus time.
















   ..
       !! processed by numpydoc !!

.. py:property:: lc_rho
   :type: Optional[int]


   
   Get or set the Load curve ID to describe the density versus time.
















   ..
       !! processed by numpydoc !!

.. py:property:: lc_p_
   :type: Optional[int]


   
   Get or set the Load curve ID to describe the pressure versus time.
















   ..
       !! processed by numpydoc !!

.. py:property:: lc_t
   :type: Optional[int]


   
   Get or set the Load curve ID to describe the temperature versus time.
















   ..
       !! processed by numpydoc !!

.. py:property:: sf_u
   :type: float


   
   Get or set the Scale factor for LC_U
















   ..
       !! processed by numpydoc !!

.. py:property:: sf_v_
   :type: float


   
   Get or set the Scale factor for LC_V
















   ..
       !! processed by numpydoc !!

.. py:property:: sf_w
   :type: float


   
   Get or set the Scale factor for LC_W
















   ..
       !! processed by numpydoc !!

.. py:property:: sf_rho
   :type: float


   
   Get or set the Scale factor for LC_RHO
















   ..
       !! processed by numpydoc !!

.. py:property:: sf_p_
   :type: float


   
   Get or set the Scale factor for LC_P
















   ..
       !! processed by numpydoc !!

.. py:property:: sf_t
   :type: float


   
   Get or set the Scale factor for LC_T
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CESE'


.. py:attribute:: subkeyword
   :value: 'BOUNDARY_PRESCRIBED_PART'






