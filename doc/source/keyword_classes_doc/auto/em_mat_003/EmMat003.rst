





:class:`EmMat003`
=================


.. py:class:: em_mat_003.EmMat003(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EM_MAT_003 keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EmMat003

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material ID: refers to MID in the *PART card.
          * - :py:attr:`~mtype`
            - Get or set the Defines the electromagnetism type of the material:
          * - :py:attr:`~sigma11`
            - Get or set the The 1,1 term in the 3 x 3 electromagnetic conductivity tensor matrix. Note that 1 corresponds to the a material direction.If a negative value is entered, a *DEFINE_FUNCTION will be expected. See remark 3- for available parameters.
          * - :py:attr:`~sigma22`
            - Get or set the The 2,2 term in the 3 x 3 electromagnetic conductivity tensor matrix.If a negative value is entered, a *DEFINE_FUNCTION will be expected. See remark 3- for available parameters.
          * - :py:attr:`~sigma33`
            - Get or set the The 3,3 term in the 3 x 3 electromagnetic conductivity tensor matrix.If a negative value is entered, a *DEFINE_FUNCTION will be expected. See remark 3- for available parameters.
          * - :py:attr:`~sigma12`
            - Get or set the The 1,2 term in the 3 x 3 electromagnetic conductivity tensor matrix.Note that 2 corresponds to the b material direction.. If a negative value is entered, a *DEFINE_FUNCTION will be expected. See remark 3- for available parameters.
          * - :py:attr:`~sigma13`
            - Get or set the The 1,3 term in the 3 x 3 electromagnetic conductivity tensor matrix.If a negative value is entered, a *DEFINE_FUNCTION will be expected. See remark 3- for available parameters.
          * - :py:attr:`~sigma21`
            - Get or set the The 2,1 term in the 3 x 3 electromagnetic conductivity tensor matrix. Note that 1 corresponds to the a material direction.If a negative value is entered, a *DEFINE_FUNCTION will be expected. See remark 3- for available parameters.
          * - :py:attr:`~sigma23`
            - Get or set the The 2,3 term in the 3 x 3 electromagnetic conductivity tensor matrix.If a negative value is entered, a *DEFINE_FUNCTION will be expected. See remark 3- for available parameters.
          * - :py:attr:`~sigma31`
            - Get or set the The 3,1 term in the 3 x 3 electromagnetic conductivity tensor matrix.If a negative value is entered, a *DEFINE_FUNCTION will be expected. See remark 3- for available parameters.
          * - :py:attr:`~sigma32`
            - Get or set the The 3,2 term in the 3 x 3 electromagnetic conductivity tensor matrix.If a negative value is entered, a *DEFINE_FUNCTION will be expected. See remark 3- for available parameters.
          * - :py:attr:`~aopt`
            - Get or set the Material axes option:
          * - :py:attr:`~lambda_`
            - Get or set the Intra- to extracellular conductivity ratio. When non-empty, the elliptic equation is solved to compute extracellular potentials
          * - :py:attr:`~xp`
            - Get or set the Define coordinates of point p for AOPT = 1 and 4.
          * - :py:attr:`~yp`
            - Get or set the Define coordinates of point p for AOPT = 1 and 4.
          * - :py:attr:`~zp`
            - Get or set the Define coordinates of point p for AOPT = 1 and 4.
          * - :py:attr:`~a1`
            - Get or set the Define components of vector a for AOPT = 2.
          * - :py:attr:`~a2`
            - Get or set the Define components of vector a for AOPT = 2.
          * - :py:attr:`~a3`
            - Get or set the Define components of vector a for AOPT = 2.
          * - :py:attr:`~macf`
            - Get or set the Material axes change flag for solid elements:
          * - :py:attr:`~v1`
            - Get or set the Define components of vector v for AOPT = 3 and 4.
          * - :py:attr:`~v2`
            - Get or set the Define components of vector v for AOPT = 3 and 4.
          * - :py:attr:`~v3`
            - Get or set the Define components of vector v for AOPT = 3 and 4.
          * - :py:attr:`~d1`
            - Get or set the Define components of vector d for AOPT = 2.
          * - :py:attr:`~d2`
            - Get or set the Define components of vector d for AOPT = 2.
          * - :py:attr:`~d3`
            - Get or set the Define components of vector d for AOPT = 2.


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

    from em_mat_003 import EmMat003

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material ID: refers to MID in the *PART card.
















   ..
       !! processed by numpydoc !!

.. py:property:: mtype
   :type: int


   
   Get or set the Defines the electromagnetism type of the material:
   EQ.0:   Air or vacuum.
   EQ.1 : Insulator material : These materials have the same electromagnetism behavior as EQ.0.
   EQ.2 : Conductor carrying a source.In these conductors, the eddy current problem is solved, which gives the actual current density.Typically, this would correspond to the coil.In Electrophysiology, it corresponds to the tissue where the monodomain equations are solved for EMSOL = 11 or EMSOL = 13. An * EM_EP_CELLMODEL must be associated to this * EM_MAT_003.
   EQ.4 : Conductor not connected to any current or voltage source, where the Eddy current problem is solved.Typically, this would correspond to the workpiece.In Electrophysiology(EP), for EMSOL = 11, 12 or 13, it corresponds to the batsurrounding the tissue, where only the external potential is solved for.No* EM_EP_CELLMODEL should be associated with these materials
















   ..
       !! processed by numpydoc !!

.. py:property:: sigma11
   :type: Optional[float]


   
   Get or set the The 1,1 term in the 3 x 3 electromagnetic conductivity tensor matrix. Note that 1 corresponds to the a material direction.If a negative value is entered, a *DEFINE_FUNCTION will be expected. See remark 3- for available parameters.
















   ..
       !! processed by numpydoc !!

.. py:property:: sigma22
   :type: Optional[float]


   
   Get or set the The 2,2 term in the 3 x 3 electromagnetic conductivity tensor matrix.If a negative value is entered, a *DEFINE_FUNCTION will be expected. See remark 3- for available parameters.
















   ..
       !! processed by numpydoc !!

.. py:property:: sigma33
   :type: Optional[float]


   
   Get or set the The 3,3 term in the 3 x 3 electromagnetic conductivity tensor matrix.If a negative value is entered, a *DEFINE_FUNCTION will be expected. See remark 3- for available parameters.
















   ..
       !! processed by numpydoc !!

.. py:property:: sigma12
   :type: Optional[int]


   
   Get or set the The 1,2 term in the 3 x 3 electromagnetic conductivity tensor matrix.Note that 2 corresponds to the b material direction.. If a negative value is entered, a *DEFINE_FUNCTION will be expected. See remark 3- for available parameters.
















   ..
       !! processed by numpydoc !!

.. py:property:: sigma13
   :type: Optional[int]


   
   Get or set the The 1,3 term in the 3 x 3 electromagnetic conductivity tensor matrix.If a negative value is entered, a *DEFINE_FUNCTION will be expected. See remark 3- for available parameters.
















   ..
       !! processed by numpydoc !!

.. py:property:: sigma21
   :type: Optional[float]


   
   Get or set the The 2,1 term in the 3 x 3 electromagnetic conductivity tensor matrix. Note that 1 corresponds to the a material direction.If a negative value is entered, a *DEFINE_FUNCTION will be expected. See remark 3- for available parameters.
















   ..
       !! processed by numpydoc !!

.. py:property:: sigma23
   :type: Optional[float]


   
   Get or set the The 2,3 term in the 3 x 3 electromagnetic conductivity tensor matrix.If a negative value is entered, a *DEFINE_FUNCTION will be expected. See remark 3- for available parameters.
















   ..
       !! processed by numpydoc !!

.. py:property:: sigma31
   :type: Optional[float]


   
   Get or set the The 3,1 term in the 3 x 3 electromagnetic conductivity tensor matrix.If a negative value is entered, a *DEFINE_FUNCTION will be expected. See remark 3- for available parameters.
















   ..
       !! processed by numpydoc !!

.. py:property:: sigma32
   :type: Optional[float]


   
   Get or set the The 3,2 term in the 3 x 3 electromagnetic conductivity tensor matrix.If a negative value is entered, a *DEFINE_FUNCTION will be expected. See remark 3- for available parameters.
















   ..
       !! processed by numpydoc !!

.. py:property:: aopt
   :type: int


   
   Get or set the Material axes option:
   EQ.0.0: locally orthotropic with material axes determined by element nodes
   EQ.1.0:locally orthotropic with material axes determined by a point in space and the global location of the element center this is the a-direction.
   EQ.2.0:globally orthotropic with material axes determined by vectors defined below, as with *DEFINE_COORDINATE_VECTOR.
   EQ.3.0:locally orthotropic material axes determined by rotating the material axes about the element normal by an angle,BETA, from a line in the plane of the element defined by the cross product of the vector v with the element normal. The plane of a solid element is the midsurface between the inner surface and outer surface defined by the first four nodes and the last four nodes of the connectivity of the element, respectively.
   EQ.4.0:locally orthotropic in cylindrical coordinate system with the material axes determined by a vector v, and an originating point, P, which define the centerline axis. This option is for solid elements only.
   EQ.5.0:globally defined reference frame with (a,b,c)=(X0,Y0,Z0).
















   ..
       !! processed by numpydoc !!

.. py:property:: lambda_
   :type: Optional[float]


   
   Get or set the Intra- to extracellular conductivity ratio. When non-empty, the elliptic equation is solved to compute extracellular potentials
















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


   
   Get or set the Define components of vector a for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: a2
   :type: Optional[float]


   
   Get or set the Define components of vector a for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: a3
   :type: Optional[float]


   
   Get or set the Define components of vector a for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: macf
   :type: int


   
   Get or set the Material axes change flag for solid elements:
   EQ.1: No change, default
















   ..
       !! processed by numpydoc !!

.. py:property:: v1
   :type: Optional[float]


   
   Get or set the Define components of vector v for AOPT = 3 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: v2
   :type: Optional[float]


   
   Get or set the Define components of vector v for AOPT = 3 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: v3
   :type: Optional[float]


   
   Get or set the Define components of vector v for AOPT = 3 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: d1
   :type: Optional[float]


   
   Get or set the Define components of vector d for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: d2
   :type: Optional[float]


   
   Get or set the Define components of vector d for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: d3
   :type: Optional[float]


   
   Get or set the Define components of vector d for AOPT = 2.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EM'


.. py:attribute:: subkeyword
   :value: 'MAT_003'






