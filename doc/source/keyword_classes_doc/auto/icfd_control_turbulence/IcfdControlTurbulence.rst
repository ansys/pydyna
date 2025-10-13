





:class:`IcfdControlTurbulence`
==============================


.. py:class:: icfd_control_turbulence.IcfdControlTurbulence(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ICFD_CONTROL_TURBULENCE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IcfdControlTurbulence

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~tmod`
            - Get or set the Indicates what turbulence model will be used.
          * - :py:attr:`~submod`
            - Get or set the Turbulence sub-model.
          * - :py:attr:`~wlaw`
            - Get or set the Law of the wall ID is RANS turbulence model selected:
          * - :py:attr:`~ks`
            - Get or set the Roughness physical height and Roughness constant. Only used if RANS turbulence model selected.
          * - :py:attr:`~cs`
            - Get or set the Roughness physical height and Roughness constant. Only used if RANS turbulence model selected.
          * - :py:attr:`~lcids1`
            - Get or set the Load curve describing user defined source term in turbulent kinetic energy equation function of time.
          * - :py:attr:`~lcids2`
            - Get or set the Load curve describing user defined source term in turbulent dissipation equation function of time.
          * - :py:attr:`~ce1`
            - Get or set the k-epsilon model constants
          * - :py:attr:`~ce2`
            - Get or set the k-epsilon model constants
          * - :py:attr:`~qe`
            - Get or set the k-epsilon model constants
          * - :py:attr:`~qk`
            - Get or set the k-epsilon model constants
          * - :py:attr:`~cu`
            - Get or set the k-epsilon model constants
          * - :py:attr:`~ccut`
            - Get or set the k-epsilon model constants
          * - :py:attr:`~r`
            - Get or set the k-omega model constants
          * - :py:attr:`~beta_01`
            - Get or set the k-omega model constants
          * - :py:attr:`~beta_w1`
            - Get or set the k-omega model constants
          * - :py:attr:`~sigma_w1`
            - Get or set the k-omega model constants
          * - :py:attr:`~sigma_k1`
            - Get or set the k-omega model constants
          * - :py:attr:`~alpha1`
            - Get or set the k-omega model constants
          * - :py:attr:`~beta_02`
            - Get or set the k-omega model constants
          * - :py:attr:`~sigma_w2`
            - Get or set the k-omega model constants
          * - :py:attr:`~sigma_k2`
            - Get or set the k-omega model constants
          * - :py:attr:`~cl`
            - Get or set the k-omega model constants
          * - :py:attr:`~cb1`
            - Get or set the Spalart-Allmaras constants
          * - :py:attr:`~cb2`
            - Get or set the Spalart-Allmaras constants
          * - :py:attr:`~sigma_v`
            - Get or set the Spalart-Allmaras constants
          * - :py:attr:`~cv1`
            - Get or set the Spalart-Allmaras constants
          * - :py:attr:`~cw1`
            - Get or set the Spalart-Allmaras constants
          * - :py:attr:`~cw2`
            - Get or set the Spalart-Allmaras constants


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

    from icfd_control_turbulence import IcfdControlTurbulence

Property detail
---------------

.. py:property:: tmod
   :type: int


   
   Get or set the Indicates what turbulence model will be used.
   EQ.0: Turbulence model based on a variational multiscale approach is used by default.
   EQ.1: RANS k-epsilon approach.
   EQ.2: LES Smagorinsky sub-grid scale model.
   EQ.3: LES Wall adapting local eddy-viscosity (WALE) model.
   EQ.4: RANS k-omega approach.
   EQ.5: RANS Spalart Allmaras approach.
















   ..
       !! processed by numpydoc !!

.. py:property:: submod
   :type: int


   
   Get or set the Turbulence sub-model.
   If TMOD = 1:
   EQ.1:Standard model.
   EQ.2:Realizable model.
   If TMOD = 4:
   EQ.1:Standard Wilcox 98 model.
   EQ.2:Standard Wilcox 06 model.
   EQ.3:SST Menter 2003.
















   ..
       !! processed by numpydoc !!

.. py:property:: wlaw
   :type: int


   
   Get or set the Law of the wall ID is RANS turbulence model selected:
   EQ.1: Standard classic law of the wall.
   EQ.2: Standard Launder and Spalding law of the wall.
   EQ.4: Non equilibrium Launder and Spalding law of the wall.
   EQ.5: Automatic classic law of the wall.
















   ..
       !! processed by numpydoc !!

.. py:property:: ks
   :type: float


   
   Get or set the Roughness physical height and Roughness constant. Only used if RANS turbulence model selected.
















   ..
       !! processed by numpydoc !!

.. py:property:: cs
   :type: float


   
   Get or set the Roughness physical height and Roughness constant. Only used if RANS turbulence model selected.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcids1
   :type: int


   
   Get or set the Load curve describing user defined source term in turbulent kinetic energy equation function of time.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcids2
   :type: int


   
   Get or set the Load curve describing user defined source term in turbulent dissipation equation function of time.
















   ..
       !! processed by numpydoc !!

.. py:property:: ce1
   :type: float


   
   Get or set the k-epsilon model constants
















   ..
       !! processed by numpydoc !!

.. py:property:: ce2
   :type: float


   
   Get or set the k-epsilon model constants
















   ..
       !! processed by numpydoc !!

.. py:property:: qe
   :type: float


   
   Get or set the k-epsilon model constants
















   ..
       !! processed by numpydoc !!

.. py:property:: qk
   :type: float


   
   Get or set the k-epsilon model constants
















   ..
       !! processed by numpydoc !!

.. py:property:: cu
   :type: float


   
   Get or set the k-epsilon model constants
















   ..
       !! processed by numpydoc !!

.. py:property:: ccut
   :type: float


   
   Get or set the k-epsilon model constants
















   ..
       !! processed by numpydoc !!

.. py:property:: r
   :type: float


   
   Get or set the k-omega model constants
















   ..
       !! processed by numpydoc !!

.. py:property:: beta_01
   :type: float


   
   Get or set the k-omega model constants
















   ..
       !! processed by numpydoc !!

.. py:property:: beta_w1
   :type: float


   
   Get or set the k-omega model constants
















   ..
       !! processed by numpydoc !!

.. py:property:: sigma_w1
   :type: float


   
   Get or set the k-omega model constants
















   ..
       !! processed by numpydoc !!

.. py:property:: sigma_k1
   :type: float


   
   Get or set the k-omega model constants
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha1
   :type: float


   
   Get or set the k-omega model constants
















   ..
       !! processed by numpydoc !!

.. py:property:: beta_02
   :type: float


   
   Get or set the k-omega model constants
















   ..
       !! processed by numpydoc !!

.. py:property:: sigma_w2
   :type: float


   
   Get or set the k-omega model constants
















   ..
       !! processed by numpydoc !!

.. py:property:: sigma_k2
   :type: float


   
   Get or set the k-omega model constants
















   ..
       !! processed by numpydoc !!

.. py:property:: cl
   :type: float


   
   Get or set the k-omega model constants
















   ..
       !! processed by numpydoc !!

.. py:property:: cb1
   :type: float


   
   Get or set the Spalart-Allmaras constants
















   ..
       !! processed by numpydoc !!

.. py:property:: cb2
   :type: float


   
   Get or set the Spalart-Allmaras constants
















   ..
       !! processed by numpydoc !!

.. py:property:: sigma_v
   :type: float


   
   Get or set the Spalart-Allmaras constants
















   ..
       !! processed by numpydoc !!

.. py:property:: cv1
   :type: float


   
   Get or set the Spalart-Allmaras constants
















   ..
       !! processed by numpydoc !!

.. py:property:: cw1
   :type: float


   
   Get or set the Spalart-Allmaras constants
















   ..
       !! processed by numpydoc !!

.. py:property:: cw2
   :type: float


   
   Get or set the Spalart-Allmaras constants
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ICFD'


.. py:attribute:: subkeyword
   :value: 'CONTROL_TURBULENCE'






