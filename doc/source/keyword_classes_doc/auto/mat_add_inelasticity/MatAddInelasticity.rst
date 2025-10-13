





:class:`MatAddInelasticity`
===========================


.. py:class:: mat_add_inelasticity.MatAddInelasticity(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_ADD_INELASTICITY keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatAddInelasticity

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification for which this erosion definition applies. A unique number or label must be specified.
          * - :py:attr:`~nielinks`
            - Get or set the Number of links/networks/phases specified by the user, An additional link may be added internally if the weights below do not sum up to unity.
          * - :py:attr:`~g`
            - Get or set the Characteristic shear modulus, used for some of the inelasticity models. This should reflect the elastic stiffness for the material without any inelasticity effects. For instance, if *MAT_ELASTIC is used, set G=E/2(1+ν) ..
          * - :py:attr:`~k`
            - Get or set the Characteristic bulk modulus, used for some of the inelasticity models. This should reflect the elastic stiffness for the material without any inelasticity effects. For instance, if *MAT_ELASTIC is used, set K=E/3(1-2ν) ..
          * - :py:attr:`~aopt`
            - Get or set the Material axes option, see AOPT on *MAT_002 for a detailed description.
          * - :py:attr:`~macf`
            - Get or set the Material axes change flag for solid elements:
          * - :py:attr:`~beta`
            - Get or set the Material angle in degrees for AOPT = 0 (shells only) and AOPT = 3 (all element types). This angle may be overriden on the element card; see *ELEMENT_‌SHELL_‌BETA and *ELEMENT_‌SOLID_‌ORTHO.
          * - :py:attr:`~xp`
            - Get or set the Define coordinates of point for AOPT = 1 and 4, see MAT_002.
          * - :py:attr:`~yp`
            - Get or set the Define coordinates of point for AOPT = 1 and 4, see MAT_002.
          * - :py:attr:`~zp`
            - Get or set the Define coordinates of point for AOPT = 1 and 4, see MAT_002.
          * - :py:attr:`~a1`
            - Get or set the Define components of vector for AOPT = 2, see MAT_002.
          * - :py:attr:`~a2`
            - Get or set the Define components of vector for AOPT = 2, see MAT_002.
          * - :py:attr:`~a3`
            - Get or set the Define components of vector for AOPT = 2, see MAT_002.
          * - :py:attr:`~v1`
            - Get or set the Define components of vector for AOPT = 3 and 4, see MAT_002.
          * - :py:attr:`~v2`
            - Get or set the Define components of vector for AOPT = 3 and 4, see MAT_002.
          * - :py:attr:`~v3`
            - Get or set the Define components of vector for AOPT = 3 and 4, see MAT_002.
          * - :py:attr:`~d1`
            - Get or set the Define components of vector for AOPT = 3 and 4, see MAT_002
          * - :py:attr:`~d2`
            - Get or set the Define components of vector for AOPT = 3 and 4, see MAT_002.
          * - :py:attr:`~d3`
            - Get or set the Define components of vector for AOPT = 3 and 4, see MAT_002.
          * - :py:attr:`~nielaws`
            - Get or set the Number of inelasticity laws that apply to this material model at this link, each contributing in its own way to the total inelastic strain (rate).
          * - :py:attr:`~weight`
            - Get or set the Weight of this link/network/phase, used when computing total stress.
          * - :py:attr:`~law`
            - Get or set the Inelasticity law, one of laws listed below must be chosen
          * - :py:attr:`~model`
            - Get or set the Model definition with choice dependent on the specified law above. A valid combination of law and model must be chosen.
          * - :py:attr:`~p1`
            - Get or set the Virgin yield stress.
          * - :py:attr:`~p2`
            - Get or set the Hardening.
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

    from mat_add_inelasticity import MatAddInelasticity

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification for which this erosion definition applies. A unique number or label must be specified.
















   ..
       !! processed by numpydoc !!

.. py:property:: nielinks
   :type: int


   
   Get or set the Number of links/networks/phases specified by the user, An additional link may be added internally if the weights below do not sum up to unity.
















   ..
       !! processed by numpydoc !!

.. py:property:: g
   :type: Optional[float]


   
   Get or set the Characteristic shear modulus, used for some of the inelasticity models. This should reflect the elastic stiffness for the material without any inelasticity effects. For instance, if *MAT_ELASTIC is used, set G=E/2(1+ν) ..
















   ..
       !! processed by numpydoc !!

.. py:property:: k
   :type: Optional[float]


   
   Get or set the Characteristic bulk modulus, used for some of the inelasticity models. This should reflect the elastic stiffness for the material without any inelasticity effects. For instance, if *MAT_ELASTIC is used, set K=E/3(1-2ν) ..
















   ..
       !! processed by numpydoc !!

.. py:property:: aopt
   :type: Optional[float]


   
   Get or set the Material axes option, see AOPT on *MAT_002 for a detailed description.
   EQ.0.0: Locally orthotropic with material axes determined by element nodes.For shells only, the material axes are then rotated about the normal vector to the surface of the shell by the angle BETA.
   EQ.1.0 : Locally orthotropic with material axes determined by a point, P, in spaceand the global location of the element center.This option is for solid elements only.
   EQ.2.0 : Globally orthotropic with material axes determined by vectors defined below
   EQ.3.0 : Locally orthotropic material axes determined by a vector v and the normal vector to the plane of the element.The plane of a solid element is the midsurface between the inner surface and outer surface defined by the first four nodes and the last four nodes of the connectivity of the element, respectively.Thus, for solid elements, AOPT = 3 is only available for hexahedrons.a is determined by taking the cross product of v with the normal vector, b is determined by taking the cross product of the normal vector with a,and c is the normal vector.Then aand b are rotated about c by an angle BETA.BETA may be set in the keyword input for the element or in the input for this keyword.Note that for solids, the material axes may be switched depending on the choice of MACF.The switch may occur before or after applying BETA depending on the value of MACF.
   EQ.4.0 : Locally orthotropic in cylindrical coordinate system with the material axes determined by a vector, v,and an originating point, P, defining the centerline axis.This option is for solid elements only.
   LT.0.0 : The absolute value of AOPT is a coordinate system ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: macf
   :type: float


   
   Get or set the Material axes change flag for solid elements:
   EQ. - 4:        Switch material axes b and c before BETA rotation
   EQ. - 3 : Switch material axes a and c before BETA rotation
   EQ. - 2 : Switch material axes a and b before BETA rotation
   EQ.1 : No change, default
   EQ.2 : Switch material axes a and b after BETA rotation
   EQ.3 : Switch material axes a and c after BETA rotation
   EQ.4 : Switch material axes b and c after BETA rotation
















   ..
       !! processed by numpydoc !!

.. py:property:: beta
   :type: Optional[float]


   
   Get or set the Material angle in degrees for AOPT = 0 (shells only) and AOPT = 3 (all element types). This angle may be overriden on the element card; see *ELEMENT_‌SHELL_‌BETA and *ELEMENT_‌SOLID_‌ORTHO.
















   ..
       !! processed by numpydoc !!

.. py:property:: xp
   :type: Optional[float]


   
   Get or set the Define coordinates of point for AOPT = 1 and 4, see MAT_002.
















   ..
       !! processed by numpydoc !!

.. py:property:: yp
   :type: Optional[float]


   
   Get or set the Define coordinates of point for AOPT = 1 and 4, see MAT_002.
















   ..
       !! processed by numpydoc !!

.. py:property:: zp
   :type: Optional[float]


   
   Get or set the Define coordinates of point for AOPT = 1 and 4, see MAT_002.
















   ..
       !! processed by numpydoc !!

.. py:property:: a1
   :type: Optional[float]


   
   Get or set the Define components of vector for AOPT = 2, see MAT_002.
















   ..
       !! processed by numpydoc !!

.. py:property:: a2
   :type: Optional[float]


   
   Get or set the Define components of vector for AOPT = 2, see MAT_002.
















   ..
       !! processed by numpydoc !!

.. py:property:: a3
   :type: Optional[float]


   
   Get or set the Define components of vector for AOPT = 2, see MAT_002.
















   ..
       !! processed by numpydoc !!

.. py:property:: v1
   :type: Optional[float]


   
   Get or set the Define components of vector for AOPT = 3 and 4, see MAT_002.
















   ..
       !! processed by numpydoc !!

.. py:property:: v2
   :type: Optional[float]


   
   Get or set the Define components of vector for AOPT = 3 and 4, see MAT_002.
















   ..
       !! processed by numpydoc !!

.. py:property:: v3
   :type: Optional[float]


   
   Get or set the Define components of vector for AOPT = 3 and 4, see MAT_002.
















   ..
       !! processed by numpydoc !!

.. py:property:: d1
   :type: Optional[float]


   
   Get or set the Define components of vector for AOPT = 3 and 4, see MAT_002
















   ..
       !! processed by numpydoc !!

.. py:property:: d2
   :type: Optional[float]


   
   Get or set the Define components of vector for AOPT = 3 and 4, see MAT_002.
















   ..
       !! processed by numpydoc !!

.. py:property:: d3
   :type: Optional[float]


   
   Get or set the Define components of vector for AOPT = 3 and 4, see MAT_002.
















   ..
       !! processed by numpydoc !!

.. py:property:: nielaws
   :type: int


   
   Get or set the Number of inelasticity laws that apply to this material model at this link, each contributing in its own way to the total inelastic strain (rate).
















   ..
       !! processed by numpydoc !!

.. py:property:: weight
   :type: Optional[float]


   
   Get or set the Weight of this link/network/phase, used when computing total stress.
















   ..
       !! processed by numpydoc !!

.. py:property:: law
   :type: int


   
   Get or set the Inelasticity law, one of laws listed below must be chosen
   LAW.EQ.3: Isotropic hardening plasticity.
   LAW.EQ.5: Creep.
   LAW.EQ.6: Viscoelasticity
















   ..
       !! processed by numpydoc !!

.. py:property:: model
   :type: Optional[int]


   
   Get or set the Model definition with choice dependent on the specified law above. A valid combination of law and model must be chosen.
   For isotropic hardening plasticity(LAW = 3), choices areEQ.1:   Linear hardening
   EQ.2 : Hardening from curve / table
   For creep(LAW = 5), choices are
   EQ.1 : Norton incremental formulation
   EQ.2 : Norton total formulation
   EQ.3 : Norton - Bailey formulation
   EQ.4 : Bergström - Boyce formulation
   For viscoelasticity(LAW = 6), choices are
   EQ.1 : Bulk and shear decay, with optional temperature shifts, hypoelastic version
   EQ.2 : Bulk and shear decay, with optional temperature shifts, hyperelastic version #1
   EQ.3:   Bulk and shear decay, with optional temperature shifts, hyperelastic version #2
   EQ.4:   Norton - Bailey formulation
   EQ.5 : Bergström - Boyce formulation
















   ..
       !! processed by numpydoc !!

.. py:property:: p1
   :type: Optional[float]


   
   Get or set the Virgin yield stress.
















   ..
       !! processed by numpydoc !!

.. py:property:: p2
   :type: Optional[float]


   
   Get or set the Hardening.
















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
   :value: 'ADD_INELASTICITY'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





