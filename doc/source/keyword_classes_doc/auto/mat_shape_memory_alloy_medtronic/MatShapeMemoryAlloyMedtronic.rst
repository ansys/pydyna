





:class:`MatShapeMemoryAlloyMedtronic`
=====================================


.. py:class:: mat_shape_memory_alloy_medtronic.MatShapeMemoryAlloyMedtronic(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_SHAPE_MEMORY_ALLOY_MEDTRONIC keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatShapeMemoryAlloyMedtronic

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification. A unique number or label must be specified.
          * - :py:attr:`~rho`
            - Get or set the Mass density.
          * - :py:attr:`~em`
            - Get or set the Martensite Young's modulus.
          * - :py:attr:`~ea`
            - Get or set the Austenite Young's modulus.
          * - :py:attr:`~prm`
            - Get or set the Martensite Poisson's ratio.
          * - :py:attr:`~pra`
            - Get or set the Austenite Poisson's ratio.
          * - :py:attr:`~aopt`
            - Get or set the Material axes option (see MAT_OPTIONTROPIC_ELASTIC, particularly the Material Directions section, for details):
          * - :py:attr:`~stype`
            - Get or set the Initiation/saturation surface type:
          * - :py:attr:`~cpm`
            - Get or set the Martensite volumetric heat capacity (density times specific heat capacity).
          * - :py:attr:`~cpa`
            - Get or set the Austenite volumetric heat capacity (density times specific heat capacity).
          * - :py:attr:`~lh`
            - Get or set the Volumetric latent heat of transformation (density times specific latent heat).
          * - :py:attr:`~tc`
            - Get or set the Thermodynamic temperature.
          * - :py:attr:`~tmf`
            - Get or set the Martensite finish temperature, optional;
          * - :py:attr:`~tms`
            - Get or set the Martensite start temperature, optional;
          * - :py:attr:`~tas`
            - Get or set the Austenite start temperature, optional;
          * - :py:attr:`~taf`
            - Get or set the Austenite finish temperature, optional;
          * - :py:attr:`~a1i`
            - Get or set the Tension/compression asymmetry for initiation surface.
          * - :py:attr:`~a2i`
            - Get or set the Tension/compression asymmetry for initiation surface.
          * - :py:attr:`~bi`
            - Get or set the Radius for initiation surface.
          * - :py:attr:`~ci`
            - Get or set the Eccentricity of initiation surface with respect to material direction.
          * - :py:attr:`~ki`
            - Get or set the Coefficient in initiation energy.
          * - :py:attr:`~mi`
            - Get or set the Exponent in initiation energy.
          * - :py:attr:`~kl`
            - Get or set the Coefficient in volume fraction energy
          * - :py:attr:`~ml`
            - Get or set the Exponent in volume fraction energy.
          * - :py:attr:`~a1s`
            - Get or set the Tension/compression asymmetry for saturation surface.
          * - :py:attr:`~a2s`
            - Get or set the Tension/compression asymmetry for saturation surface.
          * - :py:attr:`~bs`
            - Get or set the Radius for saturation surface.
          * - :py:attr:`~cs`
            - Get or set the Eccentricity of saturation surface with respect to material direction.
          * - :py:attr:`~ks`
            - Get or set the Coefficient in saturation energy.
          * - :py:attr:`~ms`
            - Get or set the Exponent in saturation energy.
          * - :py:attr:`~d0l`
            - Get or set the Initial driving force for volume fraction transformation.
          * - :py:attr:`~d0m`
            - Get or set the Initial driving force for martensite strain transformation.
          * - :py:attr:`~xp`
            - Get or set the Coordinates of point P for AOPT = 1 and 4.
          * - :py:attr:`~yp`
            - Get or set the Coordinates of point P for AOPT = 1 and 4.
          * - :py:attr:`~zp`
            - Get or set the Coordinates of point P for AOPT = 1 and 4.
          * - :py:attr:`~a1`
            - Get or set the Components of vector a for AOPT = 2.
          * - :py:attr:`~a2`
            - Get or set the Components of vector a for AOPT = 2.
          * - :py:attr:`~a3`
            - Get or set the Components of vector a for AOPT = 2.
          * - :py:attr:`~macf`
            - Get or set the Material axes change flag for solid elements:
          * - :py:attr:`~v1`
            - Get or set the Components of vector v for AOPT = 3 and 4.
          * - :py:attr:`~v2`
            - Get or set the Components of vector v for AOPT = 3 and 4.
          * - :py:attr:`~v3`
            - Get or set the Components of vector v for AOPT = 3 and 4.
          * - :py:attr:`~d1`
            - Get or set the Components of vector d for AOPT = 2.
          * - :py:attr:`~d2`
            - Get or set the Components of vector d for AOPT = 2.
          * - :py:attr:`~d3`
            - Get or set the Components of vector d for AOPT = 2.
          * - :py:attr:`~beta`
            - Get or set the Material angle in degrees for AOPT = 3.  This angle may be overridden on the element card; see *ELEMENT_SOLID_ORTHO.
          * - :py:attr:`~ref`
            - Get or set the Use reference geometry to initialize the stress tensor.  The reference geometry is defined by the keyword:
          * - :py:attr:`~n11`
            - Get or set the Additional anisotropy parameters for initiation/saturation surface, relative to material axis given by AOPT. Used for STYPE = 1.
          * - :py:attr:`~n22`
            - Get or set the Additional anisotropy parameters for initiation/saturation surface, relative to material axis given by AOPT. Used for STYPE = 1.
          * - :py:attr:`~n33`
            - Get or set the Additional anisotropy parameters for initiation/saturation surface, relative to material axis given by AOPT. Used for STYPE = 1.
          * - :py:attr:`~n44`
            - Get or set the Additional anisotropy parameters for initiation/saturation surface, relative to material axis given by AOPT. Used for STYPE = 1.
          * - :py:attr:`~n55`
            - Get or set the Additional anisotropy parameters for initiation/saturation surface, relative to material axis given by AOPT. Used for STYPE = 1.
          * - :py:attr:`~n66`
            - Get or set the Additional anisotropy parameters for initiation/saturation surface, relative to material axis given by AOPT. Used for STYPE = 1.
          * - :py:attr:`~n12`
            - Get or set the Additional anisotropy parameters for initiation/saturation surface, relative to material axis given by AOPT. Used for STYPE = 1.
          * - :py:attr:`~n23`
            - Get or set the Additional anisotropy parameters for initiation/saturation surface, relative to material axis given by AOPT. Used for STYPE = 1.
          * - :py:attr:`~n34`
            - Get or set the Additional anisotropy parameters for initiation/saturation surface, relative to material axis given by AOPT. Used for STYPE = 1.
          * - :py:attr:`~n45`
            - Get or set the Additional anisotropy parameters for initiation/saturation surface, relative to material axis given by AOPT. Used for STYPE = 1.
          * - :py:attr:`~n56`
            - Get or set the Additional anisotropy parameters for initiation/saturation surface, relative to material axis given by AOPT. Used for STYPE = 1.
          * - :py:attr:`~n13`
            - Get or set the Additional anisotropy parameters for initiation/saturation surface, relative to material axis given by AOPT. Used for STYPE = 1.
          * - :py:attr:`~n24`
            - Get or set the Additional anisotropy parameters for initiation/saturation surface, relative to material axis given by AOPT. Used for STYPE = 1.
          * - :py:attr:`~n35`
            - Get or set the Additional anisotropy parameters for initiation/saturation surface, relative to material axis given by AOPT. Used for STYPE = 1.
          * - :py:attr:`~n46`
            - Get or set the Additional anisotropy parameters for initiation/saturation surface, relative to material axis given by AOPT. Used for STYPE = 1.
          * - :py:attr:`~n14`
            - Get or set the Additional anisotropy parameters for initiation/saturation surface, relative to material axis given by AOPT. Used for STYPE = 1.
          * - :py:attr:`~n25`
            - Get or set the Additional anisotropy parameters for initiation/saturation surface, relative to material axis given by AOPT. Used for STYPE = 1.
          * - :py:attr:`~n36`
            - Get or set the Additional anisotropy parameters for initiation/saturation surface, relative to material axis given by AOPT. Used for STYPE = 1.
          * - :py:attr:`~n15`
            - Get or set the Additional anisotropy parameters for initiation/saturation surface, relative to material axis given by AOPT. Used for STYPE = 1.
          * - :py:attr:`~n26`
            - Get or set the Additional anisotropy parameters for initiation/saturation surface, relative to material axis given by AOPT. Used for STYPE = 1.
          * - :py:attr:`~n16`
            - Get or set the Additional anisotropy parameters for initiation/saturation surface, relative to material axis given by AOPT. Used for STYPE = 1.
          * - :py:attr:`~kp`
            - Get or set the Coefficient in plastic energy.
          * - :py:attr:`~mp`
            - Get or set the Exponent in plastic energy.
          * - :py:attr:`~kc`
            - Get or set the Coefficient in coupling energy.
          * - :py:attr:`~mc`
            - Get or set the Exponent in plastic energy.
          * - :py:attr:`~d0p`
            - Get or set the Initial driving force for plastic transformation.
          * - :py:attr:`~qp`
            - Get or set the Isotropic hardening coefficient in plastic relation.
          * - :py:attr:`~np`
            - Get or set the Isotropic hardening exponent in plastic relation.
          * - :py:attr:`~ql`
            - Get or set the Isotropic hardening coefficient in volume fraction relation.
          * - :py:attr:`~nl`
            - Get or set the Isotropic hardening exponent in volume fraction relation.
          * - :py:attr:`~qm`
            - Get or set the Isotropic hardening coefficient in martensite kinetic relation.
          * - :py:attr:`~nm`
            - Get or set the Isotropic hardening exponent in martensite kinetic relation.
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

    from mat_shape_memory_alloy_medtronic import MatShapeMemoryAlloyMedtronic

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification. A unique number or label must be specified.
















   ..
       !! processed by numpydoc !!

.. py:property:: rho
   :type: Optional[float]


   
   Get or set the Mass density.
















   ..
       !! processed by numpydoc !!

.. py:property:: em
   :type: Optional[float]


   
   Get or set the Martensite Young's modulus.
















   ..
       !! processed by numpydoc !!

.. py:property:: ea
   :type: Optional[float]


   
   Get or set the Austenite Young's modulus.
















   ..
       !! processed by numpydoc !!

.. py:property:: prm
   :type: Optional[float]


   
   Get or set the Martensite Poisson's ratio.
















   ..
       !! processed by numpydoc !!

.. py:property:: pra
   :type: Optional[float]


   
   Get or set the Austenite Poisson's ratio.
















   ..
       !! processed by numpydoc !!

.. py:property:: aopt
   :type: Optional[int]


   
   Get or set the Material axes option (see MAT_OPTIONTROPIC_ELASTIC, particularly the Material Directions section, for details):
   EQ.0.0: Locally orthotropic with material axes determined by element nodes 1, 2,and 4, as with* DEFINE_COORDINATE_NODES.
   EQ.1.0 : Locally orthotropic with material axes determined by a point, P, in spaceand the global location of the element center; this is the a - direction.
   EQ.2.0: Globally orthotropic with material axes determined by vectors defined below, as with* DEFINE_COORDINATE_VECTOR
   EQ.3.0 : Locally orthotropic material axes determined by a vector v and the normal vector to the plane of the element.The plane of a solid element is the midsurface between the inner surface and outer surface defined by the first four nodes and the last four nodes of the connectivity of the element, respectively.Thus, AOPT = 3 is only available for hexahedrons.a is determined by taking the cross product of v with the normal vector, b is determined by taking the cross product of the normal vector with a,and c is the normal vector.Then aand b are rotated about c by an angle BETA.BETA may be set in the keyword input for the element or in the input for this keyword.Note that the material axes may be switched depending on the choice of MACF.The switch may occur before or after applying BETA depending on the value of MACF.
   EQ.4.0 : Locally orthotropic in a cylindrical coordinate system with the material axes determined by a vector v,and an originating point, P, which define the centerline axis.
   LT.0.0 : The absolute value of AOPT is a coordinate system ID number(CID on * DEFINE_COORDINATE_OPTION).
















   ..
       !! processed by numpydoc !!

.. py:property:: stype
   :type: int


   
   Get or set the Initiation/saturation surface type:
   EQ.0:   uses strain invariants(default)
   EQ.1 : uses principal strains.
















   ..
       !! processed by numpydoc !!

.. py:property:: cpm
   :type: Optional[float]


   
   Get or set the Martensite volumetric heat capacity (density times specific heat capacity).
















   ..
       !! processed by numpydoc !!

.. py:property:: cpa
   :type: Optional[float]


   
   Get or set the Austenite volumetric heat capacity (density times specific heat capacity).
















   ..
       !! processed by numpydoc !!

.. py:property:: lh
   :type: Optional[float]


   
   Get or set the Volumetric latent heat of transformation (density times specific latent heat).
















   ..
       !! processed by numpydoc !!

.. py:property:: tc
   :type: Optional[float]


   
   Get or set the Thermodynamic temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: tmf
   :type: Optional[float]


   
   Get or set the Martensite finish temperature, optional;
















   ..
       !! processed by numpydoc !!

.. py:property:: tms
   :type: Optional[float]


   
   Get or set the Martensite start temperature, optional;
















   ..
       !! processed by numpydoc !!

.. py:property:: tas
   :type: Optional[float]


   
   Get or set the Austenite start temperature, optional;
















   ..
       !! processed by numpydoc !!

.. py:property:: taf
   :type: Optional[float]


   
   Get or set the Austenite finish temperature, optional;
















   ..
       !! processed by numpydoc !!

.. py:property:: a1i
   :type: Optional[float]


   
   Get or set the Tension/compression asymmetry for initiation surface.
















   ..
       !! processed by numpydoc !!

.. py:property:: a2i
   :type: Optional[float]


   
   Get or set the Tension/compression asymmetry for initiation surface.
















   ..
       !! processed by numpydoc !!

.. py:property:: bi
   :type: Optional[float]


   
   Get or set the Radius for initiation surface.
















   ..
       !! processed by numpydoc !!

.. py:property:: ci
   :type: Optional[float]


   
   Get or set the Eccentricity of initiation surface with respect to material direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: ki
   :type: Optional[float]


   
   Get or set the Coefficient in initiation energy.
















   ..
       !! processed by numpydoc !!

.. py:property:: mi
   :type: Optional[float]


   
   Get or set the Exponent in initiation energy.
















   ..
       !! processed by numpydoc !!

.. py:property:: kl
   :type: Optional[float]


   
   Get or set the Coefficient in volume fraction energy
















   ..
       !! processed by numpydoc !!

.. py:property:: ml
   :type: Optional[float]


   
   Get or set the Exponent in volume fraction energy.
















   ..
       !! processed by numpydoc !!

.. py:property:: a1s
   :type: Optional[float]


   
   Get or set the Tension/compression asymmetry for saturation surface.
















   ..
       !! processed by numpydoc !!

.. py:property:: a2s
   :type: Optional[float]


   
   Get or set the Tension/compression asymmetry for saturation surface.
















   ..
       !! processed by numpydoc !!

.. py:property:: bs
   :type: Optional[float]


   
   Get or set the Radius for saturation surface.
















   ..
       !! processed by numpydoc !!

.. py:property:: cs
   :type: Optional[float]


   
   Get or set the Eccentricity of saturation surface with respect to material direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: ks
   :type: Optional[float]


   
   Get or set the Coefficient in saturation energy.
















   ..
       !! processed by numpydoc !!

.. py:property:: ms
   :type: Optional[float]


   
   Get or set the Exponent in saturation energy.
















   ..
       !! processed by numpydoc !!

.. py:property:: d0l
   :type: Optional[float]


   
   Get or set the Initial driving force for volume fraction transformation.
















   ..
       !! processed by numpydoc !!

.. py:property:: d0m
   :type: Optional[float]


   
   Get or set the Initial driving force for martensite strain transformation.
















   ..
       !! processed by numpydoc !!

.. py:property:: xp
   :type: Optional[float]


   
   Get or set the Coordinates of point P for AOPT = 1 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: yp
   :type: Optional[float]


   
   Get or set the Coordinates of point P for AOPT = 1 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: zp
   :type: Optional[float]


   
   Get or set the Coordinates of point P for AOPT = 1 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: a1
   :type: Optional[float]


   
   Get or set the Components of vector a for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: a2
   :type: Optional[float]


   
   Get or set the Components of vector a for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: a3
   :type: Optional[float]


   
   Get or set the Components of vector a for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: macf
   :type: int


   
   Get or set the Material axes change flag for solid elements:
   EQ. - 4:        Switch material axes b and c before BETA rotation
   EQ. - 3 : Switch material axes a and c before BETA rotation
   EQ. - 2 : Switch material axes a and b before BETA rotation
   EQ.1 : No change, default
   EQ.2 : Switch material axes a and b after BETA rotation
   EQ.3 : Switch material axes a and c after BETA rotation
   EQ.4 : Switch material axes b and c after BETA rotation
   Figure Error!Reference source not found.indicates when LS - DYNA applies MACF during the process to obtain the final material axes.If BETA on * ELEMENT_SOLID_{OPTION} is defined, then that BETA is used for the rotation for all AOPT options.Otherwise, for AOPT = 3, the BETA input on Card 7 rotates the axes.For all other values of AOPT, the material axes will be switched as specified by MACF, but no BETA rotation will be performed
















   ..
       !! processed by numpydoc !!

.. py:property:: v1
   :type: Optional[float]


   
   Get or set the Components of vector v for AOPT = 3 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: v2
   :type: Optional[float]


   
   Get or set the Components of vector v for AOPT = 3 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: v3
   :type: Optional[float]


   
   Get or set the Components of vector v for AOPT = 3 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: d1
   :type: Optional[float]


   
   Get or set the Components of vector d for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: d2
   :type: Optional[float]


   
   Get or set the Components of vector d for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: d3
   :type: Optional[float]


   
   Get or set the Components of vector d for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: beta
   :type: Optional[float]


   
   Get or set the Material angle in degrees for AOPT = 3.  This angle may be overridden on the element card; see *ELEMENT_SOLID_ORTHO.
















   ..
       !! processed by numpydoc !!

.. py:property:: ref
   :type: Optional[float]


   
   Get or set the Use reference geometry to initialize the stress tensor.  The reference geometry is defined by the keyword:
   *INITIAL_FOAM_REFERENCE_GEOMETRY.EQ.0.0:        off
   EQ.1.0: on
















   ..
       !! processed by numpydoc !!

.. py:property:: n11
   :type: Optional[float]


   
   Get or set the Additional anisotropy parameters for initiation/saturation surface, relative to material axis given by AOPT. Used for STYPE = 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: n22
   :type: Optional[float]


   
   Get or set the Additional anisotropy parameters for initiation/saturation surface, relative to material axis given by AOPT. Used for STYPE = 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: n33
   :type: Optional[float]


   
   Get or set the Additional anisotropy parameters for initiation/saturation surface, relative to material axis given by AOPT. Used for STYPE = 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: n44
   :type: Optional[float]


   
   Get or set the Additional anisotropy parameters for initiation/saturation surface, relative to material axis given by AOPT. Used for STYPE = 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: n55
   :type: Optional[float]


   
   Get or set the Additional anisotropy parameters for initiation/saturation surface, relative to material axis given by AOPT. Used for STYPE = 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: n66
   :type: Optional[float]


   
   Get or set the Additional anisotropy parameters for initiation/saturation surface, relative to material axis given by AOPT. Used for STYPE = 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: n12
   :type: Optional[float]


   
   Get or set the Additional anisotropy parameters for initiation/saturation surface, relative to material axis given by AOPT. Used for STYPE = 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: n23
   :type: Optional[float]


   
   Get or set the Additional anisotropy parameters for initiation/saturation surface, relative to material axis given by AOPT. Used for STYPE = 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: n34
   :type: Optional[float]


   
   Get or set the Additional anisotropy parameters for initiation/saturation surface, relative to material axis given by AOPT. Used for STYPE = 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: n45
   :type: Optional[float]


   
   Get or set the Additional anisotropy parameters for initiation/saturation surface, relative to material axis given by AOPT. Used for STYPE = 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: n56
   :type: Optional[float]


   
   Get or set the Additional anisotropy parameters for initiation/saturation surface, relative to material axis given by AOPT. Used for STYPE = 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: n13
   :type: Optional[float]


   
   Get or set the Additional anisotropy parameters for initiation/saturation surface, relative to material axis given by AOPT. Used for STYPE = 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: n24
   :type: Optional[float]


   
   Get or set the Additional anisotropy parameters for initiation/saturation surface, relative to material axis given by AOPT. Used for STYPE = 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: n35
   :type: Optional[float]


   
   Get or set the Additional anisotropy parameters for initiation/saturation surface, relative to material axis given by AOPT. Used for STYPE = 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: n46
   :type: Optional[float]


   
   Get or set the Additional anisotropy parameters for initiation/saturation surface, relative to material axis given by AOPT. Used for STYPE = 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: n14
   :type: Optional[float]


   
   Get or set the Additional anisotropy parameters for initiation/saturation surface, relative to material axis given by AOPT. Used for STYPE = 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: n25
   :type: Optional[float]


   
   Get or set the Additional anisotropy parameters for initiation/saturation surface, relative to material axis given by AOPT. Used for STYPE = 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: n36
   :type: Optional[float]


   
   Get or set the Additional anisotropy parameters for initiation/saturation surface, relative to material axis given by AOPT. Used for STYPE = 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: n15
   :type: Optional[float]


   
   Get or set the Additional anisotropy parameters for initiation/saturation surface, relative to material axis given by AOPT. Used for STYPE = 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: n26
   :type: Optional[float]


   
   Get or set the Additional anisotropy parameters for initiation/saturation surface, relative to material axis given by AOPT. Used for STYPE = 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: n16
   :type: Optional[float]


   
   Get or set the Additional anisotropy parameters for initiation/saturation surface, relative to material axis given by AOPT. Used for STYPE = 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: kp
   :type: Optional[float]


   
   Get or set the Coefficient in plastic energy.
















   ..
       !! processed by numpydoc !!

.. py:property:: mp
   :type: Optional[float]


   
   Get or set the Exponent in plastic energy.
















   ..
       !! processed by numpydoc !!

.. py:property:: kc
   :type: Optional[float]


   
   Get or set the Coefficient in coupling energy.
















   ..
       !! processed by numpydoc !!

.. py:property:: mc
   :type: Optional[float]


   
   Get or set the Exponent in plastic energy.
















   ..
       !! processed by numpydoc !!

.. py:property:: d0p
   :type: Optional[float]


   
   Get or set the Initial driving force for plastic transformation.
















   ..
       !! processed by numpydoc !!

.. py:property:: qp
   :type: Optional[float]


   
   Get or set the Isotropic hardening coefficient in plastic relation.
















   ..
       !! processed by numpydoc !!

.. py:property:: np
   :type: Optional[float]


   
   Get or set the Isotropic hardening exponent in plastic relation.
















   ..
       !! processed by numpydoc !!

.. py:property:: ql
   :type: Optional[float]


   
   Get or set the Isotropic hardening coefficient in volume fraction relation.
















   ..
       !! processed by numpydoc !!

.. py:property:: nl
   :type: Optional[float]


   
   Get or set the Isotropic hardening exponent in volume fraction relation.
















   ..
       !! processed by numpydoc !!

.. py:property:: qm
   :type: Optional[float]


   
   Get or set the Isotropic hardening coefficient in martensite kinetic relation.
















   ..
       !! processed by numpydoc !!

.. py:property:: nm
   :type: Optional[float]


   
   Get or set the Isotropic hardening exponent in martensite kinetic relation.
















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
   :value: 'SHAPE_MEMORY_ALLOY_MEDTRONIC'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





