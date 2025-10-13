





:class:`EmMat005`
=================


.. py:class:: em_mat_005.EmMat005(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EM_MAT_005 keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EmMat005

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
          * - :py:attr:`~sigmaxxa`
            - Get or set the The term in the 3 x 3 electromagnetic conductivity tensor matrix for the two conductivities.If a negative value is entered, a *DEFINE_FUNCTION will be expected. See remark 3- for available parameters
          * - :py:attr:`~sigmayya`
            - Get or set the The term in the 3 x 3 electromagnetic conductivity tensor matrix for the two conductivities.If a negative value is entered, a *DEFINE_FUNCTION will be expected. See remark 3- for available parameters
          * - :py:attr:`~sigmazza`
            - Get or set the The term in the 3 x 3 electromagnetic conductivity tensor matrix for the two conductivities.If a negative value is entered, a *DEFINE_FUNCTION will be expected. See remark 3- for available parameters
          * - :py:attr:`~sigmaxya`
            - Get or set the The term in the 3 x 3 electromagnetic conductivity tensor matrix for the two conductivities.
          * - :py:attr:`~sigmaxza`
            - Get or set the The term in the 3 x 3 electromagnetic conductivity tensor matrix for the two conductivities.
          * - :py:attr:`~sigmayxa`
            - Get or set the The term in the 3 x 3 electromagnetic conductivity tensor matrix for the two conductivities.
          * - :py:attr:`~sigmayza`
            - Get or set the The term in the 3 x 3 electromagnetic conductivity tensor matrix for the two conductivities.
          * - :py:attr:`~sigmazxa`
            - Get or set the The term in the 3 x 3 electromagnetic conductivity tensor matrix for the two conductivities.
          * - :py:attr:`~sigmazya`
            - Get or set the The term in the 3 x 3 electromagnetic conductivity tensor matrix for the two conductivities.
          * - :py:attr:`~sigmaxxb`
            - Get or set the The term in the 3 x 3 electromagnetic conductivity tensor matrix for the two conductivities.
          * - :py:attr:`~sigmayyb`
            - Get or set the The term in the 3 x 3 electromagnetic conductivity tensor matrix for the two conductivities.
          * - :py:attr:`~sigmazzb`
            - Get or set the The term in the 3 x 3 electromagnetic conductivity tensor matrix for the two conductivities.
          * - :py:attr:`~sigmaxyb`
            - Get or set the The term in the 3 x 3 electromagnetic conductivity tensor matrix for the two conductivities.
          * - :py:attr:`~sigmaxzb`
            - Get or set the The term in the 3 x 3 electromagnetic conductivity tensor matrix for the two conductivities.
          * - :py:attr:`~sigmayxb`
            - Get or set the The term in the 3 x 3 electromagnetic conductivity tensor matrix for the two conductivities.
          * - :py:attr:`~sigmayzb`
            - Get or set the The term in the 3 x 3 electromagnetic conductivity tensor matrix for the two conductivities.
          * - :py:attr:`~sigmazxb`
            - Get or set the The term in the 3 x 3 electromagnetic conductivity tensor matrix for the two conductivities.
          * - :py:attr:`~sigmazyb`
            - Get or set the The term in the 3 x 3 electromagnetic conductivity tensor matrix for the two conductivities.
          * - :py:attr:`~aopt`
            - Get or set the Material axes option:
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

    from em_mat_005 import EmMat005

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

   EQ.0:   Air or vacuum
   EQ.1 : Insulator material : these materials have the same electromagnetism behavior as EQ.0.
   EQ.2 : In EP, it corresponds to the tissue, where the bidomain equations will be solved for EMSOL = 12 or EMSOL = 13. An * EM_EP_CELLMODEL must be associated to this * EM_MAT_005
   EQ.4 : In EP, it corresponds to the bath where only the external potential is solved for.No* EM_EP_CELLMODEL should be associated with these materials.
   EQ.5 : Material associated to * EM_RANDLES_BATMAC















   ..
       !! processed by numpydoc !!

.. py:property:: sigmaxxa
   :type: Optional[float]


   
   Get or set the The term in the 3 x 3 electromagnetic conductivity tensor matrix for the two conductivities.If a negative value is entered, a *DEFINE_FUNCTION will be expected. See remark 3- for available parameters
















   ..
       !! processed by numpydoc !!

.. py:property:: sigmayya
   :type: Optional[float]


   
   Get or set the The term in the 3 x 3 electromagnetic conductivity tensor matrix for the two conductivities.If a negative value is entered, a *DEFINE_FUNCTION will be expected. See remark 3- for available parameters
















   ..
       !! processed by numpydoc !!

.. py:property:: sigmazza
   :type: Optional[float]


   
   Get or set the The term in the 3 x 3 electromagnetic conductivity tensor matrix for the two conductivities.If a negative value is entered, a *DEFINE_FUNCTION will be expected. See remark 3- for available parameters
















   ..
       !! processed by numpydoc !!

.. py:property:: sigmaxya
   :type: Optional[float]


   
   Get or set the The term in the 3 x 3 electromagnetic conductivity tensor matrix for the two conductivities.
















   ..
       !! processed by numpydoc !!

.. py:property:: sigmaxza
   :type: Optional[float]


   
   Get or set the The term in the 3 x 3 electromagnetic conductivity tensor matrix for the two conductivities.
















   ..
       !! processed by numpydoc !!

.. py:property:: sigmayxa
   :type: Optional[float]


   
   Get or set the The term in the 3 x 3 electromagnetic conductivity tensor matrix for the two conductivities.
















   ..
       !! processed by numpydoc !!

.. py:property:: sigmayza
   :type: Optional[float]


   
   Get or set the The term in the 3 x 3 electromagnetic conductivity tensor matrix for the two conductivities.
















   ..
       !! processed by numpydoc !!

.. py:property:: sigmazxa
   :type: Optional[float]


   
   Get or set the The term in the 3 x 3 electromagnetic conductivity tensor matrix for the two conductivities.
















   ..
       !! processed by numpydoc !!

.. py:property:: sigmazya
   :type: Optional[float]


   
   Get or set the The term in the 3 x 3 electromagnetic conductivity tensor matrix for the two conductivities.
















   ..
       !! processed by numpydoc !!

.. py:property:: sigmaxxb
   :type: Optional[float]


   
   Get or set the The term in the 3 x 3 electromagnetic conductivity tensor matrix for the two conductivities.
















   ..
       !! processed by numpydoc !!

.. py:property:: sigmayyb
   :type: Optional[float]


   
   Get or set the The term in the 3 x 3 electromagnetic conductivity tensor matrix for the two conductivities.
















   ..
       !! processed by numpydoc !!

.. py:property:: sigmazzb
   :type: Optional[float]


   
   Get or set the The term in the 3 x 3 electromagnetic conductivity tensor matrix for the two conductivities.
















   ..
       !! processed by numpydoc !!

.. py:property:: sigmaxyb
   :type: Optional[float]


   
   Get or set the The term in the 3 x 3 electromagnetic conductivity tensor matrix for the two conductivities.
















   ..
       !! processed by numpydoc !!

.. py:property:: sigmaxzb
   :type: Optional[float]


   
   Get or set the The term in the 3 x 3 electromagnetic conductivity tensor matrix for the two conductivities.
















   ..
       !! processed by numpydoc !!

.. py:property:: sigmayxb
   :type: Optional[float]


   
   Get or set the The term in the 3 x 3 electromagnetic conductivity tensor matrix for the two conductivities.
















   ..
       !! processed by numpydoc !!

.. py:property:: sigmayzb
   :type: Optional[float]


   
   Get or set the The term in the 3 x 3 electromagnetic conductivity tensor matrix for the two conductivities.
















   ..
       !! processed by numpydoc !!

.. py:property:: sigmazxb
   :type: Optional[float]


   
   Get or set the The term in the 3 x 3 electromagnetic conductivity tensor matrix for the two conductivities.
















   ..
       !! processed by numpydoc !!

.. py:property:: sigmazyb
   :type: Optional[float]


   
   Get or set the The term in the 3 x 3 electromagnetic conductivity tensor matrix for the two conductivities.
















   ..
       !! processed by numpydoc !!

.. py:property:: aopt
   :type: int


   
   Get or set the Material axes option:
   EQ.0.0:locally orthotropic with material axes determined by element nodes as shown in part (a) the figure in *MAT_002.The a-direction is from node 1 to node 2 of the element.The b-direction is orthogonal to the adirection and is in the plane formed by nodes 1, 2,and 4.
   EQ.1.0: locally orthotropic with material axes determined by a point in space and the global location of the element center; this is the a-direction.
   EQ.2.0: globally orthotropic with material axes determined by vectors defined below, as with *DEFINE_COORDINATE_VECTOR.
   EQ.3.0: locally orthotropic material axes determined by rotating the material axes about the element normal by an angle, BETA, from a line in the plane of the element defined by the cross product of the vector v with the element normal.The plane of a solid element is the midsurface between the inner surface and outer surface defined by the first four nodes and the last four nodes of the connectivity of the element, respectively.
   EQ.4.0: locally orthotropic in cylindrical coordinate system with the material axes determined by a vector v, and an originating point, P, which define the centerline axis.This option is for solid elements only.
   EQ.5.0: globally defined reference frame with(a,b,c)=(X0,Y0,Z0).
















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
   :value: 'MAT_005'






