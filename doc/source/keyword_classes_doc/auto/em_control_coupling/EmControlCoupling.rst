





:class:`EmControlCoupling`
==========================


.. py:class:: em_control_coupling.EmControlCoupling(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EM_CONTROL_COUPLING keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EmControlCoupling

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~thcoupl`
            - Get or set the Coupling to the thermal solver. When turned on, the EM solver will transfer the Joule heating terms to the solid mechanics thermal solver.
          * - :py:attr:`~smcoupl`
            - Get or set the Coupling to the solid mechanics solver. When turned on, the EM solver will transfer the Lorentz forces to the solid mechanics solver.
          * - :py:attr:`~thlcid`
            - Get or set the Optional load curve ID. When defined, the heat rate transferred to the thermal solver will be scaled by the value returned by THLCID.
          * - :py:attr:`~smlcid`
            - Get or set the Optional load curve ID. When defined, the forces transferred to the solid mechanics solver will be scaled by the value returned by SMLCID.
          * - :py:attr:`~thcplfl`
            - Get or set the Optional load curve ID. When defined, the forces transferred to the solid mechanics solver will be scaled by the value returned by SMLCID
          * - :py:attr:`~smcplfl`
            - Get or set the Coupling to the heat equation when EM quantities are solved on fluid elements. When turned on, the EM solver will transfer the Joule heating terms to the ICFD solver.
          * - :py:attr:`~cflag`
            - Get or set the convergence flag, negative values call for a curve.
          * - :py:attr:`~nflag`
            - Get or set the flag of num of max iterations for the new convergence, negative values call for a curve.
          * - :py:attr:`~smmod`
            - Get or set the Coupling to the solid mechanics solver. When turned on, the EM solver will transfer forces to the solid mechanics solver.
          * - :py:attr:`~dfx`
            - Get or set the Define function IDs for the force three components if SMMOD=1. Arguments for the Define functions are the same as in *EM_EOS_TABULATED2
          * - :py:attr:`~dfy`
            - Get or set the Define function IDs for the force three components if SMMOD=1. Arguments for the Define functions are the same as in *EM_EOS_TABULATED2
          * - :py:attr:`~dfz`
            - Get or set the Define function IDs for the force three components if SMMOD=1. Arguments for the Define functions are the same as in *EM_EOS_TABULATED2


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

    from em_control_coupling import EmControlCoupling

Property detail
---------------

.. py:property:: thcoupl
   :type: int


   
   Get or set the Coupling to the thermal solver. When turned on, the EM solver will transfer the Joule heating terms to the solid mechanics thermal solver.
   EQ.0:Coupling on.
   EQ.1:Coupling off.
















   ..
       !! processed by numpydoc !!

.. py:property:: smcoupl
   :type: int


   
   Get or set the Coupling to the solid mechanics solver. When turned on, the EM solver will transfer the Lorentz forces to the solid mechanics solver.
   EQ.0:Coupling on.Volumic Lorentz forces are transferred
   EQ.1:Coupling off.
   EQ.2:   Coupling on. Surface magnetic forces are transferred. More accurate representation of EM forces in cases involving magnets or non-linear ferromagnets. See *EM_SOLVER_FEMBEM_MONOLITHIC.
   EQ.3:   Coupling on.Surface magnetic forces are transferred on magnets and ferromagnets while volumic Lorentz forces are transferred to regular conductors
















   ..
       !! processed by numpydoc !!

.. py:property:: thlcid
   :type: int


   
   Get or set the Optional load curve ID. When defined, the heat rate transferred to the thermal solver will be scaled by the value returned by THLCID.
















   ..
       !! processed by numpydoc !!

.. py:property:: smlcid
   :type: int


   
   Get or set the Optional load curve ID. When defined, the forces transferred to the solid mechanics solver will be scaled by the value returned by SMLCID.
















   ..
       !! processed by numpydoc !!

.. py:property:: thcplfl
   :type: int


   
   Get or set the Optional load curve ID. When defined, the forces transferred to the solid mechanics solver will be scaled by the value returned by SMLCID
















   ..
       !! processed by numpydoc !!

.. py:property:: smcplfl
   :type: int


   
   Get or set the Coupling to the heat equation when EM quantities are solved on fluid elements. When turned on, the EM solver will transfer the Joule heating terms to the ICFD solver.
   EQ.0:   Coupling off.
   EQ.1 : Coupling on.
















   ..
       !! processed by numpydoc !!

.. py:property:: cflag
   :type: Optional[int]


   
   Get or set the convergence flag, negative values call for a curve.
















   ..
       !! processed by numpydoc !!

.. py:property:: nflag
   :type: Optional[int]


   
   Get or set the flag of num of max iterations for the new convergence, negative values call for a curve.
















   ..
       !! processed by numpydoc !!

.. py:property:: smmod
   :type: int


   
   Get or set the Coupling to the solid mechanics solver. When turned on, the EM solver will transfer forces to the solid mechanics solver.
   EQ.0:   Off.
   EQ.1 : Force calculation at element level is decided by * DEFINE_FUNCTION.See DFX, DFYand DFZ.
   EQ.2 : Force calculation at element level is decided by usermat routine.See dyn21em.f and user_getEMForceArray routine
















   ..
       !! processed by numpydoc !!

.. py:property:: dfx
   :type: Optional[int]


   
   Get or set the Define function IDs for the force three components if SMMOD=1. Arguments for the Define functions are the same as in *EM_EOS_TABULATED2
















   ..
       !! processed by numpydoc !!

.. py:property:: dfy
   :type: Optional[int]


   
   Get or set the Define function IDs for the force three components if SMMOD=1. Arguments for the Define functions are the same as in *EM_EOS_TABULATED2
















   ..
       !! processed by numpydoc !!

.. py:property:: dfz
   :type: Optional[int]


   
   Get or set the Define function IDs for the force three components if SMMOD=1. Arguments for the Define functions are the same as in *EM_EOS_TABULATED2
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EM'


.. py:attribute:: subkeyword
   :value: 'CONTROL_COUPLING'






