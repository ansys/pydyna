





:class:`Mat4AMicromec`
======================


.. py:class:: mat_4a_micromec.Mat4AMicromec(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_4A_MICROMEC keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: Mat4AMicromec

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification. A unique number or label must be specified.
          * - :py:attr:`~mmopt`
            - Get or set the Option to define micromechanical material behavior
          * - :py:attr:`~bupd`
            - Get or set the Tolerance for update of Strain-Concentration Tensor.
          * - :py:attr:`~failm`
            - Get or set the Option for matrix failure using a ductile DIEM model. See sections Damage Initiation and Damage Evolution in the manual page for *MAT_ADD_DAMAGE_DIEM for a description of ductile damage initialization (DITYP = 0) based on stress triaxiality and a linear damage evolution (DETYP = 0) type. Also see fields LCDI and UPF on Card 9.
          * - :py:attr:`~failf`
            - Get or set the Option for fiber failure
          * - :py:attr:`~numint`
            - Get or set the Number of failed integration points prior to element deletion.
          * - :py:attr:`~aopt`
            - Get or set the Material axes option (see MAT_OPTIONTROPIC_ELASTIC for a more complete description):
          * - :py:attr:`~macf`
            - Get or set the Material axes change flag for solid elements:
          * - :py:attr:`~xp`
            - Get or set the Define coordinates of point p for AOPT = 1 and 4.
          * - :py:attr:`~yp`
            - Get or set the Define coordinates of point p for AOPT = 1 and 4.
          * - :py:attr:`~zp`
            - Get or set the Define coordinates of point p for AOPT = 1 and 4.
          * - :py:attr:`~a1`
            - Get or set the Component of vector a for AOPT = 2.
          * - :py:attr:`~a2`
            - Get or set the Component of vector a for AOPT = 2.
          * - :py:attr:`~a3`
            - Get or set the Component of vector a for AOPT = 2.
          * - :py:attr:`~v1`
            - Get or set the Define components of vector v for AOPT = 3 and 4..
          * - :py:attr:`~v2`
            - Get or set the Define components of vector v for AOPT = 3 and 4..
          * - :py:attr:`~v3`
            - Get or set the Define components of vector v for AOPT = 3 and 4..
          * - :py:attr:`~d1`
            - Get or set the Component of vector d for AOPT = 2.
          * - :py:attr:`~d2`
            - Get or set the Component of vector d for AOPT = 2.
          * - :py:attr:`~d3`
            - Get or set the Component of vector d for AOPT = 2.
          * - :py:attr:`~beta`
            - Get or set the Material angle in degrees for AOPT = 3, may be overwritten on the element card, see
          * - :py:attr:`~fvf`
            - Get or set the Fiber-Volume-Fraction
          * - :py:attr:`~fl`
            - Get or set the Fiber length - if FD = 1 then FL = aspect ratio (may be overwritten by *INITIAL_STRESS_(T)SHELL/SOLID).
          * - :py:attr:`~fd`
            - Get or set the Fiber diameter (may be overwritten by *INITIAL_STRESS_(T)SHELL/SOLID).
          * - :py:attr:`~a11`
            - Get or set the Value of first principal fiber orientation (may be overwritten by *INITIAL_STRESS_(T)SHELL/SOLID)..
          * - :py:attr:`~a22`
            - Get or set the Value of second principal fiber orientation (may be overwritten by *INITIAL_STRESS_(T)SHELL/SOLID).
          * - :py:attr:`~rof`
            - Get or set the Mass density of fiber.
          * - :py:attr:`~el`
            - Get or set the EL, Young's modulus of fiber – longitudinal direction.
          * - :py:attr:`~et`
            - Get or set the ET, Young's modulus of fiber – transverse direction..
          * - :py:attr:`~glt`
            - Get or set the GLT, Shear modulus LT.
          * - :py:attr:`~prtl`
            - Get or set the TL, Poisson's ratio TL.
          * - :py:attr:`~prtt`
            - Get or set the TT, Poisson's ratio TT
          * - :py:attr:`~xt`
            - Get or set the Fiber tensile strength – longitudinal direction.
          * - :py:attr:`~slimxt`
            - Get or set the Factor to determine the minimum stress limit in the fiber after stress maximum (fiber tension).
          * - :py:attr:`~ncyred`
            - Get or set the Number of cycles for stress reduction from maximum to minimum (fiber tension).
          * - :py:attr:`~rom`
            - Get or set the Mass density of matrix..
          * - :py:attr:`~e`
            - Get or set the Young's modulus of matrix.
          * - :py:attr:`~pr`
            - Get or set the Poisson's ratio of matrix.
          * - :py:attr:`~sigyt`
            - Get or set the Yield stress of matrix in tension.
          * - :py:attr:`~etant`
            - Get or set the Tangent modulus of matrix in tension, ignore if (LCST.GT.0.) is defined.
          * - :py:attr:`~eps0`
            - Get or set the Quasi-static threshold strain rate (Johnson-Cook model) for bi-linear hardening.
          * - :py:attr:`~c`
            - Get or set the Johnson-Cook constant for bi-linear hardening.
          * - :py:attr:`~lcidt`
            - Get or set the Load curve ID or Table ID for defining effective stress versus
          * - :py:attr:`~lcdi`
            - Get or set the Damage initiation parameter (ductile) shells:
          * - :py:attr:`~upf`
            - Get or set the Damage evolution parameter
          * - :py:attr:`~ncyred2`
            - Get or set the In case of matrix failure (IFAILM.eq.1 or 11):
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

    from mat_4a_micromec import Mat4AMicromec

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification. A unique number or label must be specified.
















   ..
       !! processed by numpydoc !!

.. py:property:: mmopt
   :type: float


   
   Get or set the Option to define micromechanical material behavior
   EQ.0.0: elastic
   EQ.1.0: elastic-plastic
















   ..
       !! processed by numpydoc !!

.. py:property:: bupd
   :type: float


   
   Get or set the Tolerance for update of Strain-Concentration Tensor.
















   ..
       !! processed by numpydoc !!

.. py:property:: failm
   :type: Optional[float]


   
   Get or set the Option for matrix failure using a ductile DIEM model. See sections Damage Initiation and Damage Evolution in the manual page for *MAT_ADD_DAMAGE_DIEM for a description of ductile damage initialization (DITYP = 0) based on stress triaxiality and a linear damage evolution (DETYP = 0) type. Also see fields LCDI and UPF on Card 9.
   LT.0.0: | FAILM | is effective plastic matrix strain at failure.When the matrix plastic strain reaches this value, the element is deleted from the calculation.
   EQ.0.0 : Only visualization(triaxiality of matrix stresses)
   EQ.1.0 : Active DIEM(triaxiality of matrix stresses)
   EQ.10.0 : Only visualization(triaxiality of composite stresses)
   EQ.11.0 : Active DIEM(triaxiality of composite stresses)
















   ..
       !! processed by numpydoc !!

.. py:property:: failf
   :type: int


   
   Get or set the Option for fiber failure
   EQ.0: only visualization (equivalent fiber stresses)
   EQ.1: active (equivalent fiber stresses.
















   ..
       !! processed by numpydoc !!

.. py:property:: numint
   :type: float


   
   Get or set the Number of failed integration points prior to element deletion.
   LT.0.0: Only for shells. |NUMINT| is the percentage of
   integration points which must exceed the failure criterion before element fails. For shell formulations with 4 integration
   points per layer, the layer is considered failed if any of the integration points in the layer fails.
















   ..
       !! processed by numpydoc !!

.. py:property:: aopt
   :type: Optional[float]


   
   Get or set the Material axes option (see MAT_OPTIONTROPIC_ELASTIC for a more complete description):
   EQ.0.0: Locally orthotropic with material axes determined by element nodes.For shells only, the material axes are then rotated about the normal vector to the surface of the shell by the angle BETA.
   EQ.1.0 : Locally orthotropic with material axes determined by a point, P, in spaceand the global location of the element center.This option is for solid elements only.
   EQ.2.0 : Globally orthotropic with material axes determined by vectors defined below
   EQ.3.0 : Locally orthotropic material axes determined by a vector v and the normal vector to the plane of the element.The plane of a solid element is the midsurface between the inner surface and outer surface defined by the first four nodes and the last four nodes of the connectivity of the element, respectively.Thus, for solid elements, AOPT = 3 is only available for hexahedrons.a is determined by taking the cross product of v with the normal vector, b is determined by taking the cross product of the normal vector with a,and c is the normal vector.Then aand b are rotated about c by an angle BETA.BETA may be set in the keyword input for the element or in the input for this keyword.Note that for solids, the material axes may be switched depending on the choice of MACF.The switch may occur before or after applying BETA depending on the value of MACF.
   EQ.4.0 : Locally orthotropic in cylindrical coordinate system with the material axes determined by a vector, v,and an originating point, P, defining the centerline axis.This option is for solid elements only.
   LT.0.0 : The absolute value of AOPT is a coordinate system ID number(CID on * DEFINE_COORDINATE_NODES, *DEFINE_COORDINATE_SYSTEM or *DEFINE - _COORDINATE_VECTOR).
   The fiber orientation information may be overwritten using* INITIAL_STRESS_(T)SHELL / SOLID
















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
   Figure Error!Reference source not found.indicates when LS - DYNA applies MACF during the process to obtain the final material axes.If BETA on * ELEMENT_SOLID_{OPTION} is defined, then that BETA is used for the rotation for all AOPT options.Otherwise, if AOPT = 3, the BETA input on Card 3 rotates the axes.For all other values of AOPT, the material axes will be switched as specified by MACF, but no BETA rotation will be performed
















   ..
       !! processed by numpydoc !!

.. py:property:: xp
   :type: Optional[float]


   
   Get or set the Define coordinates of point p for AOPT = 1 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: yp
   :type: Optional[float]


   
   Get or set the Define coordinates of point p for AOPT = 1 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: zp
   :type: Optional[float]


   
   Get or set the Define coordinates of point p for AOPT = 1 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: a1
   :type: Optional[float]


   
   Get or set the Component of vector a for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: a2
   :type: Optional[float]


   
   Get or set the Component of vector a for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: a3
   :type: Optional[float]


   
   Get or set the Component of vector a for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: v1
   :type: Optional[float]


   
   Get or set the Define components of vector v for AOPT = 3 and 4..
















   ..
       !! processed by numpydoc !!

.. py:property:: v2
   :type: Optional[float]


   
   Get or set the Define components of vector v for AOPT = 3 and 4..
















   ..
       !! processed by numpydoc !!

.. py:property:: v3
   :type: Optional[float]


   
   Get or set the Define components of vector v for AOPT = 3 and 4..
















   ..
       !! processed by numpydoc !!

.. py:property:: d1
   :type: Optional[float]


   
   Get or set the Component of vector d for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: d2
   :type: Optional[float]


   
   Get or set the Component of vector d for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: d3
   :type: Optional[float]


   
   Get or set the Component of vector d for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: beta
   :type: Optional[float]


   
   Get or set the Material angle in degrees for AOPT = 3, may be overwritten on the element card, see
   *ELEMENT_(T)SHELL_BETA or *ELEMENT_SOLID_ORTHO..
















   ..
       !! processed by numpydoc !!

.. py:property:: fvf
   :type: Optional[float]


   
   Get or set the Fiber-Volume-Fraction
   GT.0: Fiber-Volume-Fraction
   LT.0: |FVF| Fiber-Mass-Fraction.
















   ..
       !! processed by numpydoc !!

.. py:property:: fl
   :type: Optional[float]


   
   Get or set the Fiber length - if FD = 1 then FL = aspect ratio (may be overwritten by *INITIAL_STRESS_(T)SHELL/SOLID).
















   ..
       !! processed by numpydoc !!

.. py:property:: fd
   :type: float


   
   Get or set the Fiber diameter (may be overwritten by *INITIAL_STRESS_(T)SHELL/SOLID).
















   ..
       !! processed by numpydoc !!

.. py:property:: a11
   :type: float


   
   Get or set the Value of first principal fiber orientation (may be overwritten by *INITIAL_STRESS_(T)SHELL/SOLID)..
















   ..
       !! processed by numpydoc !!

.. py:property:: a22
   :type: Optional[float]


   
   Get or set the Value of second principal fiber orientation (may be overwritten by *INITIAL_STRESS_(T)SHELL/SOLID).
















   ..
       !! processed by numpydoc !!

.. py:property:: rof
   :type: Optional[float]


   
   Get or set the Mass density of fiber.
















   ..
       !! processed by numpydoc !!

.. py:property:: el
   :type: Optional[float]


   
   Get or set the EL, Young's modulus of fiber – longitudinal direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: et
   :type: Optional[float]


   
   Get or set the ET, Young's modulus of fiber – transverse direction..
















   ..
       !! processed by numpydoc !!

.. py:property:: glt
   :type: Optional[float]


   
   Get or set the GLT, Shear modulus LT.
















   ..
       !! processed by numpydoc !!

.. py:property:: prtl
   :type: Optional[float]


   
   Get or set the TL, Poisson's ratio TL.
















   ..
       !! processed by numpydoc !!

.. py:property:: prtt
   :type: Optional[float]


   
   Get or set the TT, Poisson's ratio TT
















   ..
       !! processed by numpydoc !!

.. py:property:: xt
   :type: Optional[float]


   
   Get or set the Fiber tensile strength – longitudinal direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: slimxt
   :type: Optional[float]


   
   Get or set the Factor to determine the minimum stress limit in the fiber after stress maximum (fiber tension).
















   ..
       !! processed by numpydoc !!

.. py:property:: ncyred
   :type: float


   
   Get or set the Number of cycles for stress reduction from maximum to minimum (fiber tension).
















   ..
       !! processed by numpydoc !!

.. py:property:: rom
   :type: Optional[float]


   
   Get or set the Mass density of matrix..
















   ..
       !! processed by numpydoc !!

.. py:property:: e
   :type: Optional[float]


   
   Get or set the Young's modulus of matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: pr
   :type: Optional[float]


   
   Get or set the Poisson's ratio of matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: sigyt
   :type: Optional[float]


   
   Get or set the Yield stress of matrix in tension.
















   ..
       !! processed by numpydoc !!

.. py:property:: etant
   :type: Optional[float]


   
   Get or set the Tangent modulus of matrix in tension, ignore if (LCST.GT.0.) is defined.
















   ..
       !! processed by numpydoc !!

.. py:property:: eps0
   :type: Optional[float]


   
   Get or set the Quasi-static threshold strain rate (Johnson-Cook model) for bi-linear hardening.
















   ..
       !! processed by numpydoc !!

.. py:property:: c
   :type: Optional[float]


   
   Get or set the Johnson-Cook constant for bi-linear hardening.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidt
   :type: Optional[int]


   
   Get or set the Load curve ID or Table ID for defining effective stress versus
   effective plastic strain in tension of matrix material (Table to include strain-rate effects, viscoplastic formulation).
















   ..
       !! processed by numpydoc !!

.. py:property:: lcdi
   :type: Optional[int]


   
   Get or set the Damage initiation parameter (ductile) shells:
   Load curve ID representing plastic strain at onset of damage as function of stress triaxiality.
   or Table ID representing plastic strain at onset of damage as function of stress triaxiality and plastic strain rate.
   solids: Load curve ID representing plastic strain at onset of damage as function of stress triaxiality.
   or Table ID representing plastic strain at onset of damage as function of stress triaxiality and lode angle.
   or Table3D ID representing plastic strain at onset of damage as
   function of stress triaxiality, lode angle and plastic strain rate..
















   ..
       !! processed by numpydoc !!

.. py:property:: upf
   :type: Optional[float]


   
   Get or set the Damage evolution parameter
   GT.0.0: plastic displacement at failure, 𝑢𝑓     𝑝
   LT.0.0: |UPF| is a table ID for 𝑢𝑓 𝑝 as a function of triaxiality and   damage.
















   ..
       !! processed by numpydoc !!

.. py:property:: ncyred2
   :type: float


   
   Get or set the In case of matrix failure (IFAILM.eq.1 or 11):
   Number of cycles for stress reduction of fiber stresses until the integration point will be marked as failed.
















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
   :value: '4A_MICROMEC'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





