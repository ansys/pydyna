





:class:`EmEpCellmodelTentusscher`
=================================


.. py:class:: em_ep_cellmodel_tentusscher.EmEpCellmodelTentusscher(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EM_EP_CELLMODEL_TENTUSSCHER keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EmEpCellmodelTentusscher

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material ID defined in *MAT_.
          * - :py:attr:`~gas_constant`
            - Get or set the Gas constant.
          * - :py:attr:`~t`
            - Get or set the Temperature.
          * - :py:attr:`~faraday_constant`
            - Get or set the Faraday constant .
          * - :py:attr:`~cm`
            - Get or set the Cell capacitance for unit surface area.
          * - :py:attr:`~vc`
            - Get or set the Cytoplasmic volume.
          * - :py:attr:`~vsr`
            - Get or set the Sarcoplasmic reticulum volume.
          * - :py:attr:`~vss`
            - Get or set the Subspace volume.
          * - :py:attr:`~pkna`
            - Get or set the Relative Iks permeability to Na+
          * - :py:attr:`~ko`
            - Get or set the Extracellular K+ concentration.
          * - :py:attr:`~nao`
            - Get or set the Extracellular Na+ concentration.
          * - :py:attr:`~cao`
            - Get or set the Extracellular Ca2+ concentration.
          * - :py:attr:`~gk1`
            - Get or set the Ik1 conductance.
          * - :py:attr:`~gkr`
            - Get or set the Ikr conductance.
          * - :py:attr:`~gks`
            - Get or set the Iks conductance.
          * - :py:attr:`~gna`
            - Get or set the Ina conductance.
          * - :py:attr:`~gbna`
            - Get or set the Ibna conductance.
          * - :py:attr:`~gcal`
            - Get or set the Ical conductance.
          * - :py:attr:`~gbca`
            - Get or set the Ibca conductance.
          * - :py:attr:`~gto`
            - Get or set the Ito conductance.
          * - :py:attr:`~gpca`
            - Get or set the Ipca conductance.
          * - :py:attr:`~gpk`
            - Get or set the Ikp conductance.
          * - :py:attr:`~pnak`
            - Get or set the P_NaK: sodium potassium pump current (picoA_per_picoF).
          * - :py:attr:`~km`
            - Get or set the K_mK and K_MNa in component sodium_potassium_pump_current (millimolar)
          * - :py:attr:`~kmna`
            - Get or set the K_mK and K_MNa in component sodium_potassium_pump_current (millimolar)
          * - :py:attr:`~knaca`
            - Get or set the Components in component sodium_calcium_exchanger_current (millimolar)
          * - :py:attr:`~ksat`
            - Get or set the Components in component sodium_calcium_exchanger_current (millimolar)
          * - :py:attr:`~alpha`
            - Get or set the Components in component sodium_calcium_exchanger_current (millimolar)
          * - :py:attr:`~gamma`
            - Get or set the Components in component sodium_calcium_exchanger_current (millimolar)
          * - :py:attr:`~kmca`
            - Get or set the Components in component sodium_calcium_exchanger_current (millimolar)
          * - :py:attr:`~kmnai`
            - Get or set the Components in component sodium_calcium_exchanger_current (millimolar)
          * - :py:attr:`~kpca`
            - Get or set the K_pCa: component in calcium_pump_current (millimolar).
          * - :py:attr:`~k1`
            - Get or set the R to O and RI to I Irel transition rate.
          * - :py:attr:`~k2`
            - Get or set the O to I and R to RI Irel transition rate.
          * - :py:attr:`~k3`
            - Get or set the O to R and I to RI Irel transition rate.
          * - :py:attr:`~k4`
            - Get or set the I to O and RI to I Irel transition rate
          * - :py:attr:`~ec`
            - Get or set the CaSR half-saturation constant of Kcasr.
          * - :py:attr:`~maxsr`
            - Get or set the Maximum value of Kcasr
          * - :py:attr:`~minsr`
            - Get or set the Minimum value of Kcasr
          * - :py:attr:`~vrel`
            - Get or set the Maximal Irel conductance.
          * - :py:attr:`~vleak`
            - Get or set the Maximal Ileak conductance.
          * - :py:attr:`~vxfer`
            - Get or set the Maximal Ixfer conductance.
          * - :py:attr:`~vmaxup`
            - Get or set the Maximal Iup conductance.
          * - :py:attr:`~kup`
            - Get or set the Half-saturation constant of Iup.
          * - :py:attr:`~bufc`
            - Get or set the Total cytoplasmic buffer concentration.
          * - :py:attr:`~kbufc`
            - Get or set the Cai half-saturation constant for cytoplasmic buffer.
          * - :py:attr:`~bufsr`
            - Get or set the Total sarcoplasmic buffer concentration (mM)
          * - :py:attr:`~kbufsf`
            - Get or set the CaSR half-saturation constant for sarcoplasmic buffer.
          * - :py:attr:`~bufss`
            - Get or set the Total subspace buffer concentration.
          * - :py:attr:`~kbufss`
            - Get or set the CaSS half-saturation constant for subspace buffer.
          * - :py:attr:`~v`
            - Get or set the Initial value of transmembrane potential.
          * - :py:attr:`~ki`
            - Get or set the Initial value of K_i in component potassium_dynamics.
          * - :py:attr:`~nai`
            - Get or set the Initial value of Na_i in component sodium_dynamics.
          * - :py:attr:`~cai`
            - Get or set the Initial value of Ca_i in component calcium_dynamics.
          * - :py:attr:`~cass`
            - Get or set the Initial value of Ca_ss in component calcium_dynamics.
          * - :py:attr:`~casr`
            - Get or set the Initial value of Ca_SR in component calcium_dynamics.
          * - :py:attr:`~rpri`
            - Get or set the Initial value of R?in component calcium_dynamics.
          * - :py:attr:`~xr1`
            - Get or set the Initial value of Xr1 in component rapid time dependent potassium current Xr1 gate.
          * - :py:attr:`~xr2`
            - Get or set the Initial value of Xr2 in component rapid time dependent potassium current Xr2 gate.
          * - :py:attr:`~xs`
            - Get or set the Initial value of Xs in component slow time dependent potassium current Xs gate.
          * - :py:attr:`~m`
            - Get or set the Initial value of m in component fast_sodium_current_m_gate.
          * - :py:attr:`~h`
            - Get or set the Initial value of h in component fast_sodium_current_h_gate.
          * - :py:attr:`~j`
            - Get or set the Initial value of j in component fast_sodium_current_j_gate.
          * - :py:attr:`~d`
            - Get or set the Initial value of d in component L_type_Ca_current_d_gate.
          * - :py:attr:`~f`
            - Get or set the Initial value of f in component L_type_Ca_current_f_gate.
          * - :py:attr:`~f2`
            - Get or set the Initial value of f2 in component L_type_Ca_current_f2_gate.
          * - :py:attr:`~fcass`
            - Get or set the Initial value of fCass in component L_type_Ca_current_fCass_gate.
          * - :py:attr:`~s`
            - Get or set the Initial value of s in component transient_outward_current_s_gate.
          * - :py:attr:`~r`
            - Get or set the Initial value of r in component transient_outward_current_r_gate.


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

    from em_ep_cellmodel_tentusscher import EmEpCellmodelTentusscher

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material ID defined in *MAT_.
















   ..
       !! processed by numpydoc !!

.. py:property:: gas_constant
   :type: Optional[float]


   
   Get or set the Gas constant.
















   ..
       !! processed by numpydoc !!

.. py:property:: t
   :type: Optional[float]


   
   Get or set the Temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: faraday_constant
   :type: Optional[float]


   
   Get or set the Faraday constant .
















   ..
       !! processed by numpydoc !!

.. py:property:: cm
   :type: Optional[float]


   
   Get or set the Cell capacitance for unit surface area.
















   ..
       !! processed by numpydoc !!

.. py:property:: vc
   :type: Optional[float]


   
   Get or set the Cytoplasmic volume.
















   ..
       !! processed by numpydoc !!

.. py:property:: vsr
   :type: Optional[float]


   
   Get or set the Sarcoplasmic reticulum volume.
















   ..
       !! processed by numpydoc !!

.. py:property:: vss
   :type: Optional[float]


   
   Get or set the Subspace volume.
















   ..
       !! processed by numpydoc !!

.. py:property:: pkna
   :type: Optional[float]


   
   Get or set the Relative Iks permeability to Na+
















   ..
       !! processed by numpydoc !!

.. py:property:: ko
   :type: Optional[float]


   
   Get or set the Extracellular K+ concentration.
















   ..
       !! processed by numpydoc !!

.. py:property:: nao
   :type: Optional[float]


   
   Get or set the Extracellular Na+ concentration.
















   ..
       !! processed by numpydoc !!

.. py:property:: cao
   :type: Optional[float]


   
   Get or set the Extracellular Ca2+ concentration.
















   ..
       !! processed by numpydoc !!

.. py:property:: gk1
   :type: Optional[float]


   
   Get or set the Ik1 conductance.
















   ..
       !! processed by numpydoc !!

.. py:property:: gkr
   :type: Optional[float]


   
   Get or set the Ikr conductance.
















   ..
       !! processed by numpydoc !!

.. py:property:: gks
   :type: Optional[float]


   
   Get or set the Iks conductance.
















   ..
       !! processed by numpydoc !!

.. py:property:: gna
   :type: Optional[float]


   
   Get or set the Ina conductance.
















   ..
       !! processed by numpydoc !!

.. py:property:: gbna
   :type: Optional[float]


   
   Get or set the Ibna conductance.
















   ..
       !! processed by numpydoc !!

.. py:property:: gcal
   :type: Optional[float]


   
   Get or set the Ical conductance.
















   ..
       !! processed by numpydoc !!

.. py:property:: gbca
   :type: Optional[float]


   
   Get or set the Ibca conductance.
















   ..
       !! processed by numpydoc !!

.. py:property:: gto
   :type: Optional[float]


   
   Get or set the Ito conductance.
















   ..
       !! processed by numpydoc !!

.. py:property:: gpca
   :type: Optional[float]


   
   Get or set the Ipca conductance.
















   ..
       !! processed by numpydoc !!

.. py:property:: gpk
   :type: Optional[float]


   
   Get or set the Ikp conductance.
















   ..
       !! processed by numpydoc !!

.. py:property:: pnak
   :type: Optional[float]


   
   Get or set the P_NaK: sodium potassium pump current (picoA_per_picoF).
















   ..
       !! processed by numpydoc !!

.. py:property:: km
   :type: Optional[float]


   
   Get or set the K_mK and K_MNa in component sodium_potassium_pump_current (millimolar)
















   ..
       !! processed by numpydoc !!

.. py:property:: kmna
   :type: Optional[float]


   
   Get or set the K_mK and K_MNa in component sodium_potassium_pump_current (millimolar)
















   ..
       !! processed by numpydoc !!

.. py:property:: knaca
   :type: Optional[float]


   
   Get or set the Components in component sodium_calcium_exchanger_current (millimolar)
















   ..
       !! processed by numpydoc !!

.. py:property:: ksat
   :type: Optional[float]


   
   Get or set the Components in component sodium_calcium_exchanger_current (millimolar)
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha
   :type: Optional[float]


   
   Get or set the Components in component sodium_calcium_exchanger_current (millimolar)
















   ..
       !! processed by numpydoc !!

.. py:property:: gamma
   :type: Optional[float]


   
   Get or set the Components in component sodium_calcium_exchanger_current (millimolar)
















   ..
       !! processed by numpydoc !!

.. py:property:: kmca
   :type: Optional[float]


   
   Get or set the Components in component sodium_calcium_exchanger_current (millimolar)
















   ..
       !! processed by numpydoc !!

.. py:property:: kmnai
   :type: Optional[float]


   
   Get or set the Components in component sodium_calcium_exchanger_current (millimolar)
















   ..
       !! processed by numpydoc !!

.. py:property:: kpca
   :type: Optional[float]


   
   Get or set the K_pCa: component in calcium_pump_current (millimolar).
















   ..
       !! processed by numpydoc !!

.. py:property:: k1
   :type: Optional[float]


   
   Get or set the R to O and RI to I Irel transition rate.
















   ..
       !! processed by numpydoc !!

.. py:property:: k2
   :type: Optional[float]


   
   Get or set the O to I and R to RI Irel transition rate.
















   ..
       !! processed by numpydoc !!

.. py:property:: k3
   :type: Optional[float]


   
   Get or set the O to R and I to RI Irel transition rate.
















   ..
       !! processed by numpydoc !!

.. py:property:: k4
   :type: Optional[float]


   
   Get or set the I to O and RI to I Irel transition rate
















   ..
       !! processed by numpydoc !!

.. py:property:: ec
   :type: Optional[float]


   
   Get or set the CaSR half-saturation constant of Kcasr.
















   ..
       !! processed by numpydoc !!

.. py:property:: maxsr
   :type: Optional[float]


   
   Get or set the Maximum value of Kcasr
















   ..
       !! processed by numpydoc !!

.. py:property:: minsr
   :type: Optional[float]


   
   Get or set the Minimum value of Kcasr
















   ..
       !! processed by numpydoc !!

.. py:property:: vrel
   :type: Optional[float]


   
   Get or set the Maximal Irel conductance.
















   ..
       !! processed by numpydoc !!

.. py:property:: vleak
   :type: Optional[float]


   
   Get or set the Maximal Ileak conductance.
















   ..
       !! processed by numpydoc !!

.. py:property:: vxfer
   :type: Optional[float]


   
   Get or set the Maximal Ixfer conductance.
















   ..
       !! processed by numpydoc !!

.. py:property:: vmaxup
   :type: Optional[float]


   
   Get or set the Maximal Iup conductance.
















   ..
       !! processed by numpydoc !!

.. py:property:: kup
   :type: Optional[float]


   
   Get or set the Half-saturation constant of Iup.
















   ..
       !! processed by numpydoc !!

.. py:property:: bufc
   :type: Optional[float]


   
   Get or set the Total cytoplasmic buffer concentration.
















   ..
       !! processed by numpydoc !!

.. py:property:: kbufc
   :type: Optional[float]


   
   Get or set the Cai half-saturation constant for cytoplasmic buffer.
















   ..
       !! processed by numpydoc !!

.. py:property:: bufsr
   :type: Optional[float]


   
   Get or set the Total sarcoplasmic buffer concentration (mM)
















   ..
       !! processed by numpydoc !!

.. py:property:: kbufsf
   :type: Optional[float]


   
   Get or set the CaSR half-saturation constant for sarcoplasmic buffer.
















   ..
       !! processed by numpydoc !!

.. py:property:: bufss
   :type: Optional[float]


   
   Get or set the Total subspace buffer concentration.
















   ..
       !! processed by numpydoc !!

.. py:property:: kbufss
   :type: Optional[float]


   
   Get or set the CaSS half-saturation constant for subspace buffer.
















   ..
       !! processed by numpydoc !!

.. py:property:: v
   :type: Optional[float]


   
   Get or set the Initial value of transmembrane potential.
















   ..
       !! processed by numpydoc !!

.. py:property:: ki
   :type: Optional[float]


   
   Get or set the Initial value of K_i in component potassium_dynamics.
















   ..
       !! processed by numpydoc !!

.. py:property:: nai
   :type: Optional[float]


   
   Get or set the Initial value of Na_i in component sodium_dynamics.
















   ..
       !! processed by numpydoc !!

.. py:property:: cai
   :type: Optional[float]


   
   Get or set the Initial value of Ca_i in component calcium_dynamics.
















   ..
       !! processed by numpydoc !!

.. py:property:: cass
   :type: Optional[float]


   
   Get or set the Initial value of Ca_ss in component calcium_dynamics.
















   ..
       !! processed by numpydoc !!

.. py:property:: casr
   :type: Optional[float]


   
   Get or set the Initial value of Ca_SR in component calcium_dynamics.
















   ..
       !! processed by numpydoc !!

.. py:property:: rpri
   :type: Optional[float]


   
   Get or set the Initial value of R?in component calcium_dynamics.
















   ..
       !! processed by numpydoc !!

.. py:property:: xr1
   :type: Optional[float]


   
   Get or set the Initial value of Xr1 in component rapid time dependent potassium current Xr1 gate.
















   ..
       !! processed by numpydoc !!

.. py:property:: xr2
   :type: Optional[float]


   
   Get or set the Initial value of Xr2 in component rapid time dependent potassium current Xr2 gate.
















   ..
       !! processed by numpydoc !!

.. py:property:: xs
   :type: Optional[float]


   
   Get or set the Initial value of Xs in component slow time dependent potassium current Xs gate.
















   ..
       !! processed by numpydoc !!

.. py:property:: m
   :type: Optional[float]


   
   Get or set the Initial value of m in component fast_sodium_current_m_gate.
















   ..
       !! processed by numpydoc !!

.. py:property:: h
   :type: Optional[float]


   
   Get or set the Initial value of h in component fast_sodium_current_h_gate.
















   ..
       !! processed by numpydoc !!

.. py:property:: j
   :type: Optional[float]


   
   Get or set the Initial value of j in component fast_sodium_current_j_gate.
















   ..
       !! processed by numpydoc !!

.. py:property:: d
   :type: Optional[float]


   
   Get or set the Initial value of d in component L_type_Ca_current_d_gate.
















   ..
       !! processed by numpydoc !!

.. py:property:: f
   :type: Optional[float]


   
   Get or set the Initial value of f in component L_type_Ca_current_f_gate.
















   ..
       !! processed by numpydoc !!

.. py:property:: f2
   :type: Optional[float]


   
   Get or set the Initial value of f2 in component L_type_Ca_current_f2_gate.
















   ..
       !! processed by numpydoc !!

.. py:property:: fcass
   :type: Optional[float]


   
   Get or set the Initial value of fCass in component L_type_Ca_current_fCass_gate.
















   ..
       !! processed by numpydoc !!

.. py:property:: s
   :type: Optional[float]


   
   Get or set the Initial value of s in component transient_outward_current_s_gate.
















   ..
       !! processed by numpydoc !!

.. py:property:: r
   :type: Optional[float]


   
   Get or set the Initial value of r in component transient_outward_current_r_gate.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EM'


.. py:attribute:: subkeyword
   :value: 'EP_CELLMODEL_TENTUSSCHER'






