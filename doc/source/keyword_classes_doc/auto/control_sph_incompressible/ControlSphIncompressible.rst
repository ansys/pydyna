





:class:`ControlSphIncompressible`
=================================


.. py:class:: control_sph_incompressible.ControlSphIncompressible(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_SPH_INCOMPRESSIBLE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlSphIncompressible

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ibndp`
            - Get or set the Pressure treatment of boundary particles:
          * - :py:attr:`~tavg`
            - Get or set the Tolerance criteria for convergence. If the average relative density (ρ/ρ_0) of particles under compression is below TAVG, this condition is satisfied
          * - :py:attr:`~tmax`
            - Get or set the Tolerance criteria for convergence. If the maximum relative density (ρ/ρ_0) of particles under compression is below TMAX, this condition is satisfied
          * - :py:attr:`~rol`
            - Get or set the In certain scenarios, some fluid particles can end up stuck between moving structures and as a result accumulate very large pressure values. If a particle’s relative density contribution from boundaries is above ROL, it is deactivated.
          * - :py:attr:`~ihtc`
            - Get or set the Flag for Heat Transfer Coefficient calculation.
          * - :py:attr:`~imat`
            - Get or set the Flag for MAT_SPH_INCOMPRESSIBLE_* formulations.


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

    from control_sph_incompressible import ControlSphIncompressible

Property detail
---------------

.. py:property:: ibndp
   :type: int


   
   Get or set the Pressure treatment of boundary particles:
   EQ.0:   Pressure on boundary particles is extrapolated from fluid particles.
   EQ.1 : Pressure on boundary particles is explicitly calculated
















   ..
       !! processed by numpydoc !!

.. py:property:: tavg
   :type: float


   
   Get or set the Tolerance criteria for convergence. If the average relative density (ρ/ρ_0) of particles under compression is below TAVG, this condition is satisfied
















   ..
       !! processed by numpydoc !!

.. py:property:: tmax
   :type: float


   
   Get or set the Tolerance criteria for convergence. If the maximum relative density (ρ/ρ_0) of particles under compression is below TMAX, this condition is satisfied
















   ..
       !! processed by numpydoc !!

.. py:property:: rol
   :type: float


   
   Get or set the In certain scenarios, some fluid particles can end up stuck between moving structures and as a result accumulate very large pressure values. If a particle’s relative density contribution from boundaries is above ROL, it is deactivated.
















   ..
       !! processed by numpydoc !!

.. py:property:: ihtc
   :type: int


   
   Get or set the Flag for Heat Transfer Coefficient calculation.
   EQ.0:   HTCs are not calculated.
   EQ.1 : HTCs are calculated based on fluid properties given in * MAT_SPH_INCOMPRESSIBLE_FLUID cards.
















   ..
       !! processed by numpydoc !!

.. py:property:: imat
   :type: int


   
   Get or set the Flag for MAT_SPH_INCOMPRESSIBLE_* formulations.
   EQ.0:   Surface tension and surface adhesion forces are calculated based on numerical parameters given in the material cards.
   EQ.1 : Surface tension and surface adhesion forces are calculated based on physical properties given in the material cards.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'SPH_INCOMPRESSIBLE'






