





:class:`IcfdModelPorous`
========================


.. py:class:: icfd_model_porous.IcfdModelPorous(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ICFD_MODEL_POROUS keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IcfdModelPorous

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pmmoid`
            - Get or set the Porous media model ID.
          * - :py:attr:`~pmid`
            - Get or set the Porous media model type :
          * - :py:attr:`~por`
            - Get or set the Porosity e.
          * - :py:attr:`~per_thx`
            - Get or set the Permeability k if PMID = 1 or 2. Probe Thickness delta x if PMID = 3 or PMID = 5.
          * - :py:attr:`~ff_thy`
            - Get or set the Forchheimer factor. To Be defined if PMID = 2. Probe Thickness delta y if PMID = 5.
          * - :py:attr:`~thz`
            - Get or set the Probe Thickness delta z if PMID = 5.
          * - :py:attr:`~pvlcidx`
            - Get or set the Pressure function of Velocity Load Curve ID. To be defined if PMID = 3 and PMID = 5. If PMID = 5, this refers to P-V curve in global X direction..
          * - :py:attr:`~pvlcidy`
            - Get or set the Pressure function of Velocity Load Curve ID. To be defined if PMID = 5. This refers to P-V curve in global Y direction.
          * - :py:attr:`~pvlcidz`
            - Get or set the Pressure function of Velocity Load Curve ID. To be defined if PMID = 5. This refers to P-V curve in global Z direction.
          * - :py:attr:`~kxp_`
            - Get or set the Permeability vector in local reference frame (x', y', z'). To be defined in PMID = 4, 5, 6 or 7. Those values become scale factors if    PMID = 5.
          * - :py:attr:`~kyp_`
            - Get or set the Permeability vector in local reference frame (x', y', z'). To be defined in PMID = 4, 5, 6 or 7. Those values become scale factors if    PMID = 5.
          * - :py:attr:`~kzp_`
            - Get or set the Permeability vector in local reference frame (x', y', z'. To be defined in PMID = 4, 5, 6 or 7. Those values become scale factors if     PMID = 5.
          * - :py:attr:`~p_x_pid1r`
            - Get or set the Projection of local permeability vector x' in global reference frame(x, y, z).
          * - :py:attr:`~p_y_pid2r`
            - Get or set the Projection of local permeability vector x' in global reference frame(x, y, z).
          * - :py:attr:`~projxp_z`
            - Get or set the Projection of local permeability vector x' in global reference frame(x, y, z).
          * - :py:attr:`~projyp_x`
            - Get or set the Projection of local permeability vector y' in global reference frame(x, y, z).
          * - :py:attr:`~projyp_y`
            - Get or set the Projection of local permeability vector y' in global reference frame(x, y, z).
          * - :py:attr:`~projyp_z`
            - Get or set the Projection of local permeability vector y' in global reference frame(x, y, z).


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

    from icfd_model_porous import IcfdModelPorous

Property detail
---------------

.. py:property:: pmmoid
   :type: Optional[int]


   
   Get or set the Porous media model ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: pmid
   :type: int


   
   Get or set the Porous media model type :
   EQ.1 : Isotropic porous media - Ergun Correlation.
   EQ.2 : Isotropic porous media - Darcy-Forchheimer model.
   EQ.3 : Isotropic porous media - Permeability defined through Pressure-Velocity Data.
   EQ.4 : Anisotropic porous media - Fixed local reference frame (See Figure 5-3).
   EQ.5 : Anisotropic porous media model - Moving local reference
   frame and permeability vector in local reference frame  (𝑥’, 𝑦’, 𝑧’) defined by three Pressure-Velocity curves.
   EQ.6 : Anisotropic porous media model - Moving local reference frame and permeability vector constant.
   EQ.7: Anisotropic porous media model - Moving local reference
   frame and permeability vector constant. This model differs
   from PMID = 6 in the way the local reference frame is moved.
   EQ.8:   Main parachute model to be used jointly with *MESH_EMBEDSHELL for the parachute surface. Similar to PMID=2.
   EQ.10:  Parachute model to be used jointly with * MESH_EMBEDSHELL where the fabric permeability and Forchheimer factor are computed from the Pressure - Velocity curves of experimental data given by a LOAD_CURVE.Similar to PMID = 3.
   EQ.11 : Parachute model similar to PMID = 8 but pressure gradient is directly defined by coefficients α and β as
















   ..
       !! processed by numpydoc !!

.. py:property:: por
   :type: float


   
   Get or set the Porosity e.
















   ..
       !! processed by numpydoc !!

.. py:property:: per_thx
   :type: float


   
   Get or set the Permeability k if PMID = 1 or 2. Probe Thickness delta x if PMID = 3 or PMID = 5.
















   ..
       !! processed by numpydoc !!

.. py:property:: ff_thy
   :type: float


   
   Get or set the Forchheimer factor. To Be defined if PMID = 2. Probe Thickness delta y if PMID = 5.
















   ..
       !! processed by numpydoc !!

.. py:property:: thz
   :type: float


   
   Get or set the Probe Thickness delta z if PMID = 5.
















   ..
       !! processed by numpydoc !!

.. py:property:: pvlcidx
   :type: Optional[int]


   
   Get or set the Pressure function of Velocity Load Curve ID. To be defined if PMID = 3 and PMID = 5. If PMID = 5, this refers to P-V curve in global X direction..
















   ..
       !! processed by numpydoc !!

.. py:property:: pvlcidy
   :type: Optional[int]


   
   Get or set the Pressure function of Velocity Load Curve ID. To be defined if PMID = 5. This refers to P-V curve in global Y direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: pvlcidz
   :type: Optional[int]


   
   Get or set the Pressure function of Velocity Load Curve ID. To be defined if PMID = 5. This refers to P-V curve in global Z direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: kxp_
   :type: float


   
   Get or set the Permeability vector in local reference frame (x', y', z'). To be defined in PMID = 4, 5, 6 or 7. Those values become scale factors if    PMID = 5.
















   ..
       !! processed by numpydoc !!

.. py:property:: kyp_
   :type: float


   
   Get or set the Permeability vector in local reference frame (x', y', z'). To be defined in PMID = 4, 5, 6 or 7. Those values become scale factors if    PMID = 5.
















   ..
       !! processed by numpydoc !!

.. py:property:: kzp_
   :type: float


   
   Get or set the Permeability vector in local reference frame (x', y', z'. To be defined in PMID = 4, 5, 6 or 7. Those values become scale factors if     PMID = 5.
















   ..
       !! processed by numpydoc !!

.. py:property:: p_x_pid1r
   :type: float


   
   Get or set the Projection of local permeability vector x' in global reference frame(x, y, z).
















   ..
       !! processed by numpydoc !!

.. py:property:: p_y_pid2r
   :type: float


   
   Get or set the Projection of local permeability vector x' in global reference frame(x, y, z).
















   ..
       !! processed by numpydoc !!

.. py:property:: projxp_z
   :type: float


   
   Get or set the Projection of local permeability vector x' in global reference frame(x, y, z).
















   ..
       !! processed by numpydoc !!

.. py:property:: projyp_x
   :type: float


   
   Get or set the Projection of local permeability vector y' in global reference frame(x, y, z).
















   ..
       !! processed by numpydoc !!

.. py:property:: projyp_y
   :type: float


   
   Get or set the Projection of local permeability vector y' in global reference frame(x, y, z).
















   ..
       !! processed by numpydoc !!

.. py:property:: projyp_z
   :type: float


   
   Get or set the Projection of local permeability vector y' in global reference frame(x, y, z).
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ICFD'


.. py:attribute:: subkeyword
   :value: 'MODEL_POROUS'






