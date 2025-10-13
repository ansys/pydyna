





:class:`DefineFrictionAutomaticSurfaceToSurfaceMortar`
======================================================


.. py:class:: define_friction_automatic_surface_to_surface_mortar.DefineFrictionAutomaticSurfaceToSurfaceMortar(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_FRICTION_AUTOMATIC_SURFACE_TO_SURFACE_MORTAR keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineFrictionAutomaticSurfaceToSurfaceMortar

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the Identification number. Only one table is allowed
          * - :py:attr:`~fs_d`
            - Get or set the Default value of the static coefficient of friction. The frictional coefficient is assumed to be dependent on the relative V of the surface in the contact. Default values are used when part pair are undefined
          * - :py:attr:`~fd_d`
            - Get or set the Default value of the dynamic coefficient of friction. The frictional coefficient is assumed to be dependent on the relative velocity V of the surfaces in contact. Default values are used when part pair are undefined
          * - :py:attr:`~dc_d`
            - Get or set the Default value of the exponential decay coefficient. The frictional coefficient is assumed to be dependent on the relative velocity V of the surfaces in contact. Default values are used when part pair are undefined
          * - :py:attr:`~vc_d`
            - Get or set the Default value of the coefficient for viscous friction. This is necessary to limit the friction force to a maximum. A limiting force is computed F=VC*Acont. Acont being the area of the segment contacted by the node in contact. The suggested value for VC is to use the yield stress in shear VC=sigma/SQRT(3.0). Where sigma is the yield stress of the contacted material.Default values are used when part pair are undefined
          * - :py:attr:`~icnep`
            - Get or set the Flag to check for non-existing parts, or part sets (PIDi, PIDj) on Card 2.
          * - :py:attr:`~pid_i`
            - Get or set the Part ID I.
          * - :py:attr:`~pid_j`
            - Get or set the Part ID J.
          * - :py:attr:`~fs_ij`
            - Get or set the Static coefficient of friction between parts I and J.
          * - :py:attr:`~fd_ij`
            - Get or set the Dynamic coefficient of friction between parts I and J.
          * - :py:attr:`~dc_ij`
            - Get or set the Exponential decay coefficient between parts I and J.
          * - :py:attr:`~vc_ij`
            - Get or set the Viscous friction between parts I and J.
          * - :py:attr:`~ptypei`
            - Get or set the EQ:"PSET" when PTYPEI or PTYPEJ refers to a set_part.
          * - :py:attr:`~ptypej`
            - Get or set the EQ:"PSET" when PTYPEI or PTYPEJ refers to a set_part.
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

    from define_friction_automatic_surface_to_surface_mortar import DefineFrictionAutomaticSurfaceToSurfaceMortar

Property detail
---------------

.. py:property:: id
   :type: int


   
   Get or set the Identification number. Only one table is allowed
















   ..
       !! processed by numpydoc !!

.. py:property:: fs_d
   :type: float


   
   Get or set the Default value of the static coefficient of friction. The frictional coefficient is assumed to be dependent on the relative V of the surface in the contact. Default values are used when part pair are undefined
















   ..
       !! processed by numpydoc !!

.. py:property:: fd_d
   :type: float


   
   Get or set the Default value of the dynamic coefficient of friction. The frictional coefficient is assumed to be dependent on the relative velocity V of the surfaces in contact. Default values are used when part pair are undefined
















   ..
       !! processed by numpydoc !!

.. py:property:: dc_d
   :type: float


   
   Get or set the Default value of the exponential decay coefficient. The frictional coefficient is assumed to be dependent on the relative velocity V of the surfaces in contact. Default values are used when part pair are undefined
















   ..
       !! processed by numpydoc !!

.. py:property:: vc_d
   :type: float


   
   Get or set the Default value of the coefficient for viscous friction. This is necessary to limit the friction force to a maximum. A limiting force is computed F=VC*Acont. Acont being the area of the segment contacted by the node in contact. The suggested value for VC is to use the yield stress in shear VC=sigma/SQRT(3.0). Where sigma is the yield stress of the contacted material.Default values are used when part pair are undefined
















   ..
       !! processed by numpydoc !!

.. py:property:: icnep
   :type: int


   
   Get or set the Flag to check for non-existing parts, or part sets (PIDi, PIDj) on Card 2.
   EQ.0:   Existence of parts or part sets is checked,and an error occurs when any is missing(default).
   EQ.1 : Existence of parts or part sets is checked and lines with non - existent parts will be ignored..
















   ..
       !! processed by numpydoc !!

.. py:property:: pid_i
   :type: Optional[int]


   
   Get or set the Part ID I.
















   ..
       !! processed by numpydoc !!

.. py:property:: pid_j
   :type: Optional[int]


   
   Get or set the Part ID J.
















   ..
       !! processed by numpydoc !!

.. py:property:: fs_ij
   :type: float


   
   Get or set the Static coefficient of friction between parts I and J.
















   ..
       !! processed by numpydoc !!

.. py:property:: fd_ij
   :type: float


   
   Get or set the Dynamic coefficient of friction between parts I and J.
















   ..
       !! processed by numpydoc !!

.. py:property:: dc_ij
   :type: float


   
   Get or set the Exponential decay coefficient between parts I and J.
















   ..
       !! processed by numpydoc !!

.. py:property:: vc_ij
   :type: float


   
   Get or set the Viscous friction between parts I and J.
















   ..
       !! processed by numpydoc !!

.. py:property:: ptypei
   :type: Optional[str]


   
   Get or set the EQ:"PSET" when PTYPEI or PTYPEJ refers to a set_part.
















   ..
       !! processed by numpydoc !!

.. py:property:: ptypej
   :type: Optional[str]


   
   Get or set the EQ:"PSET" when PTYPEI or PTYPEJ refers to a set_part.
















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
   :value: 'DEFINE'


.. py:attribute:: subkeyword
   :value: 'FRICTION_AUTOMATIC_SURFACE_TO_SURFACE_MORTAR'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





