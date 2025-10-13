





:class:`MatPowder`
==================


.. py:class:: mat_powder.MatPowder(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_POWDER keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatPowder

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification. A unique number or label must be specified
          * - :py:attr:`~ro`
            - Get or set the Mass density.
          * - :py:attr:`~p11`
            - Get or set the Initial compactness tensor Pij.
          * - :py:attr:`~p22`
            - Get or set the Initial compactness tensor Pij.
          * - :py:attr:`~p33`
            - Get or set the Initial compactness tensor Pij.
          * - :py:attr:`~p12`
            - Get or set the Initial compactness tensor Pij.
          * - :py:attr:`~p23`
            - Get or set the Initial compactness tensor Pij.
          * - :py:attr:`~p13`
            - Get or set the Initial compactness tensor Pij.
          * - :py:attr:`~e0`
            - Get or set the Initial anisotropy variable e (value between 1 and 2).
          * - :py:attr:`~lck`
            - Get or set the Load curve for bulk modulus K as function of relative density d.
          * - :py:attr:`~pr`
            - Get or set the Poisson's ratio v.
          * - :py:attr:`~lcx`
            - Get or set the Load curve for hydrostatic compressive yield X as function of relative density d.
          * - :py:attr:`~lcy`
            - Get or set the Load curve for uniaxial compressive yield Y as function of relative density d.
          * - :py:attr:`~lcc`
            - Get or set the Load curve for shear yield C0 as function of relative density d.
          * - :py:attr:`~l`
            - Get or set the Yield surface parameter L relating hydrostatic compressive yield to point on hydrostatic axis with maximum strength.
          * - :py:attr:`~r`
            - Get or set the Yield surface parameter R governing the shape of the yield surface.
          * - :py:attr:`~ca`
            - Get or set the Hardening parameter Ca.
          * - :py:attr:`~cd`
            - Get or set the Hardening parameter Cd.
          * - :py:attr:`~cv`
            - Get or set the Hardening parameter Cv.
          * - :py:attr:`~p`
            - Get or set the Hardening exponent p.
          * - :py:attr:`~lch`
            - Get or set the Load curve giving back stress parameter H as function of hardening       parameter e.
          * - :py:attr:`~lcfi`
            - Get or set the Load curve giving plastic strain evolution angle  as function of relative volumetric stress.
          * - :py:attr:`~sint`
            - Get or set the Activate sintering
          * - :py:attr:`~tzro`
            - Get or set the Absolute zero temperature T0.
          * - :py:attr:`~lcfk`
            - Get or set the Load curve fk for viscous compliance as function of relative density d.
          * - :py:attr:`~lcfs2`
            - Get or set the Load curve fs2 for viscous compliance as function of temperature T.
          * - :py:attr:`~dv1`
            - Get or set the Volume diffusion coefficient dv1.
          * - :py:attr:`~dv2`
            - Get or set the Volume diffusion coefficient dv2.
          * - :py:attr:`~ds1`
            - Get or set the Surface diffusion coefficient ds1.
          * - :py:attr:`~ds2`
            - Get or set the Surface diffusion coefficient ds1.
          * - :py:attr:`~omega`
            - Get or set the Blending parameter w.
          * - :py:attr:`~rgas`
            - Get or set the Universal gas constant Rgas.
          * - :py:attr:`~lcpr`
            - Get or set the Load curve for viscous Poisson's ratio vy as function of relative density d.
          * - :py:attr:`~lcfs3`
            - Get or set the Load curve fS3 for evolution of mobility factor as function of   temperature T.
          * - :py:attr:`~lctau`
            - Get or set the Load curve for relaxation time t as function of temperature T.
          * - :py:attr:`~alpha`
            - Get or set the Thermal expansion coefficient a.
          * - :py:attr:`~lcfs1`
            - Get or set the Load curve fs1 for sintering stress scaling as function of relative      density d.
          * - :py:attr:`~gamma`
            - Get or set the Surface energy density r affecting sintering stress.
          * - :py:attr:`~l0`
            - Get or set the Grain size l0 affecting sintering stress.
          * - :py:attr:`~lcfks`
            - Get or set the Load curve fks scaling bulk modulus as function of temperature T.
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

    from mat_powder import MatPowder

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification. A unique number or label must be specified
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Mass density.
















   ..
       !! processed by numpydoc !!

.. py:property:: p11
   :type: Optional[float]


   
   Get or set the Initial compactness tensor Pij.
















   ..
       !! processed by numpydoc !!

.. py:property:: p22
   :type: Optional[float]


   
   Get or set the Initial compactness tensor Pij.
















   ..
       !! processed by numpydoc !!

.. py:property:: p33
   :type: Optional[float]


   
   Get or set the Initial compactness tensor Pij.
















   ..
       !! processed by numpydoc !!

.. py:property:: p12
   :type: Optional[float]


   
   Get or set the Initial compactness tensor Pij.
















   ..
       !! processed by numpydoc !!

.. py:property:: p23
   :type: Optional[float]


   
   Get or set the Initial compactness tensor Pij.
















   ..
       !! processed by numpydoc !!

.. py:property:: p13
   :type: Optional[float]


   
   Get or set the Initial compactness tensor Pij.
















   ..
       !! processed by numpydoc !!

.. py:property:: e0
   :type: Optional[float]


   
   Get or set the Initial anisotropy variable e (value between 1 and 2).
















   ..
       !! processed by numpydoc !!

.. py:property:: lck
   :type: Optional[int]


   
   Get or set the Load curve for bulk modulus K as function of relative density d.
















   ..
       !! processed by numpydoc !!

.. py:property:: pr
   :type: Optional[float]


   
   Get or set the Poisson's ratio v.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcx
   :type: Optional[int]


   
   Get or set the Load curve for hydrostatic compressive yield X as function of relative density d.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcy
   :type: Optional[int]


   
   Get or set the Load curve for uniaxial compressive yield Y as function of relative density d.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcc
   :type: Optional[int]


   
   Get or set the Load curve for shear yield C0 as function of relative density d.
















   ..
       !! processed by numpydoc !!

.. py:property:: l
   :type: Optional[float]


   
   Get or set the Yield surface parameter L relating hydrostatic compressive yield to point on hydrostatic axis with maximum strength.
















   ..
       !! processed by numpydoc !!

.. py:property:: r
   :type: Optional[float]


   
   Get or set the Yield surface parameter R governing the shape of the yield surface.
















   ..
       !! processed by numpydoc !!

.. py:property:: ca
   :type: Optional[float]


   
   Get or set the Hardening parameter Ca.
















   ..
       !! processed by numpydoc !!

.. py:property:: cd
   :type: Optional[float]


   
   Get or set the Hardening parameter Cd.
















   ..
       !! processed by numpydoc !!

.. py:property:: cv
   :type: Optional[float]


   
   Get or set the Hardening parameter Cv.
















   ..
       !! processed by numpydoc !!

.. py:property:: p
   :type: Optional[float]


   
   Get or set the Hardening exponent p.
















   ..
       !! processed by numpydoc !!

.. py:property:: lch
   :type: Optional[int]


   
   Get or set the Load curve giving back stress parameter H as function of hardening       parameter e.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcfi
   :type: Optional[int]


   
   Get or set the Load curve giving plastic strain evolution angle  as function of relative volumetric stress.
















   ..
       !! processed by numpydoc !!

.. py:property:: sint
   :type: float


   
   Get or set the Activate sintering
   EQ.0.0: Sintering off
   EQ.1.0: Sintering on.
















   ..
       !! processed by numpydoc !!

.. py:property:: tzro
   :type: Optional[float]


   
   Get or set the Absolute zero temperature T0.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcfk
   :type: Optional[int]


   
   Get or set the Load curve fk for viscous compliance as function of relative density d.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcfs2
   :type: Optional[int]


   
   Get or set the Load curve fs2 for viscous compliance as function of temperature T.
















   ..
       !! processed by numpydoc !!

.. py:property:: dv1
   :type: Optional[float]


   
   Get or set the Volume diffusion coefficient dv1.
















   ..
       !! processed by numpydoc !!

.. py:property:: dv2
   :type: Optional[float]


   
   Get or set the Volume diffusion coefficient dv2.
















   ..
       !! processed by numpydoc !!

.. py:property:: ds1
   :type: Optional[float]


   
   Get or set the Surface diffusion coefficient ds1.
















   ..
       !! processed by numpydoc !!

.. py:property:: ds2
   :type: Optional[float]


   
   Get or set the Surface diffusion coefficient ds1.
















   ..
       !! processed by numpydoc !!

.. py:property:: omega
   :type: Optional[float]


   
   Get or set the Blending parameter w.
















   ..
       !! processed by numpydoc !!

.. py:property:: rgas
   :type: Optional[float]


   
   Get or set the Universal gas constant Rgas.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcpr
   :type: Optional[int]


   
   Get or set the Load curve for viscous Poisson's ratio vy as function of relative density d.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcfs3
   :type: Optional[int]


   
   Get or set the Load curve fS3 for evolution of mobility factor as function of   temperature T.
















   ..
       !! processed by numpydoc !!

.. py:property:: lctau
   :type: Optional[int]


   
   Get or set the Load curve for relaxation time t as function of temperature T.
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha
   :type: Optional[float]


   
   Get or set the Thermal expansion coefficient a.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcfs1
   :type: Optional[int]


   
   Get or set the Load curve fs1 for sintering stress scaling as function of relative      density d.
















   ..
       !! processed by numpydoc !!

.. py:property:: gamma
   :type: Optional[float]


   
   Get or set the Surface energy density r affecting sintering stress.
















   ..
       !! processed by numpydoc !!

.. py:property:: l0
   :type: Optional[float]


   
   Get or set the Grain size l0 affecting sintering stress.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcfks
   :type: Optional[int]


   
   Get or set the Load curve fks scaling bulk modulus as function of temperature T.
















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
   :value: 'POWDER'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





