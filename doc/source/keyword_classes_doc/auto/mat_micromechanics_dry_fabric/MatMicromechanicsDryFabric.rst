





:class:`MatMicromechanicsDryFabric`
===================================


.. py:class:: mat_micromechanics_dry_fabric.MatMicromechanicsDryFabric(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_MICROMECHANICS_DRY_FABRIC keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatMicromechanicsDryFabric

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification.  A unique number or label must be specified.
          * - :py:attr:`~ro`
            - Get or set the Mass density.
          * - :py:attr:`~e1`
            - Get or set the Young's modulus of the yarn in axial-direction
          * - :py:attr:`~e2`
            - Get or set the Young's modulus of the yarn in transverse-direction
          * - :py:attr:`~g12`
            - Get or set the G12, shear modulus of the yarns.
          * - :py:attr:`~g23`
            - Get or set the G23, transverse shear modulus of the yarns.
          * - :py:attr:`~v12`
            - Get or set the Poisson's ratio.
          * - :py:attr:`~v23`
            - Get or set the Transverse Poisson's ratio
          * - :py:attr:`~xt`
            - Get or set the Ultimate strength at failure (not used)..
          * - :py:attr:`~thi`
            - Get or set the Initial brade angle.
          * - :py:attr:`~thl`
            - Get or set the Yarn locking angle.
          * - :py:attr:`~bfi`
            - Get or set the Initial undulation angle in fill direction.
          * - :py:attr:`~bwi`
            - Get or set the Initial undulation angle in warp direction.
          * - :py:attr:`~dscf`
            - Get or set the Discount factor
          * - :py:attr:`~cnst`
            - Get or set the Reorientation damping constant
          * - :py:attr:`~atlr`
            - Get or set the Angle tolerance for locking
          * - :py:attr:`~vmb`
            - Get or set the Viscous modulus for normal strain rate
          * - :py:attr:`~vme`
            - Get or set the Viscous modulus for shear strain rate
          * - :py:attr:`~trs`
            - Get or set the Transverse shear modulus of the fabric layer
          * - :py:attr:`~fflg`
            - Get or set the F-flag
          * - :py:attr:`~aopt`
            - Get or set the Material axes option:
          * - :py:attr:`~a1`
            - Get or set the Components of vector 𝐚 for AOPT = 2.0
          * - :py:attr:`~a2`
            - Get or set the Components of vector 𝐚 for AOPT = 2.0
          * - :py:attr:`~a3`
            - Get or set the Components of vector 𝐚 for AOPT = 2.0
          * - :py:attr:`~v1`
            - Get or set the Components of vector 𝐯 for AOPT = 3.0
          * - :py:attr:`~v2`
            - Get or set the Components of vector 𝐯 for AOPT = 3.0
          * - :py:attr:`~v3`
            - Get or set the Components of vector 𝐯 for AOPT = 3.0.
          * - :py:attr:`~d1`
            - Get or set the Components of vector 𝐝 for AOPT = 2.0
          * - :py:attr:`~d2`
            - Get or set the Components of vector 𝐝 for AOPT = 2.0
          * - :py:attr:`~d3`
            - Get or set the Components of vector 𝐝 for AOPT = 2.0
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

    from mat_micromechanics_dry_fabric import MatMicromechanicsDryFabric

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification.  A unique number or label must be specified.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Mass density.
















   ..
       !! processed by numpydoc !!

.. py:property:: e1
   :type: Optional[float]


   
   Get or set the Young's modulus of the yarn in axial-direction
















   ..
       !! processed by numpydoc !!

.. py:property:: e2
   :type: Optional[float]


   
   Get or set the Young's modulus of the yarn in transverse-direction
















   ..
       !! processed by numpydoc !!

.. py:property:: g12
   :type: Optional[float]


   
   Get or set the G12, shear modulus of the yarns.
















   ..
       !! processed by numpydoc !!

.. py:property:: g23
   :type: Optional[float]


   
   Get or set the G23, transverse shear modulus of the yarns.
















   ..
       !! processed by numpydoc !!

.. py:property:: v12
   :type: Optional[float]


   
   Get or set the Poisson's ratio.
















   ..
       !! processed by numpydoc !!

.. py:property:: v23
   :type: Optional[float]


   
   Get or set the Transverse Poisson's ratio
















   ..
       !! processed by numpydoc !!

.. py:property:: xt
   :type: Optional[float]


   
   Get or set the Ultimate strength at failure (not used)..
















   ..
       !! processed by numpydoc !!

.. py:property:: thi
   :type: Optional[float]


   
   Get or set the Initial brade angle.
















   ..
       !! processed by numpydoc !!

.. py:property:: thl
   :type: Optional[float]


   
   Get or set the Yarn locking angle.
















   ..
       !! processed by numpydoc !!

.. py:property:: bfi
   :type: Optional[float]


   
   Get or set the Initial undulation angle in fill direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: bwi
   :type: Optional[float]


   
   Get or set the Initial undulation angle in warp direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: dscf
   :type: Optional[float]


   
   Get or set the Discount factor
















   ..
       !! processed by numpydoc !!

.. py:property:: cnst
   :type: Optional[float]


   
   Get or set the Reorientation damping constant
















   ..
       !! processed by numpydoc !!

.. py:property:: atlr
   :type: Optional[float]


   
   Get or set the Angle tolerance for locking
















   ..
       !! processed by numpydoc !!

.. py:property:: vmb
   :type: Optional[float]


   
   Get or set the Viscous modulus for normal strain rate
















   ..
       !! processed by numpydoc !!

.. py:property:: vme
   :type: Optional[float]


   
   Get or set the Viscous modulus for shear strain rate
















   ..
       !! processed by numpydoc !!

.. py:property:: trs
   :type: Optional[float]


   
   Get or set the Transverse shear modulus of the fabric layer
















   ..
       !! processed by numpydoc !!

.. py:property:: fflg
   :type: Optional[float]


   
   Get or set the F-flag
















   ..
       !! processed by numpydoc !!

.. py:property:: aopt
   :type: Optional[float]


   
   Get or set the Material axes option:
   EQ.0.0: locally orthotropic with material axes determined by
   element nodes 1, 2, and 4, as with *DEFINE_COORDINATE_NODES.
   EQ.2.0: globally orthotropic with material axes determined by vectors defined below, as with *DEFINE_COORDI_NATE_VECTOR.
   EQ.3.0: locally orthotropic material axes defined by the cross product of the vector v with the element normal.
   LT.0.0: the absolute value of AOPT is a coordinate system ID number (CID on *DEFINE_COORDINATE_NODES,
   *DEFINE_COORDINATE_SYSTEM or *DEFINE_COOR_DINATE_VECTOR). Available with the R3 release of Version 971 and later.
















   ..
       !! processed by numpydoc !!

.. py:property:: a1
   :type: Optional[float]


   
   Get or set the Components of vector 𝐚 for AOPT = 2.0
















   ..
       !! processed by numpydoc !!

.. py:property:: a2
   :type: Optional[float]


   
   Get or set the Components of vector 𝐚 for AOPT = 2.0
















   ..
       !! processed by numpydoc !!

.. py:property:: a3
   :type: Optional[float]


   
   Get or set the Components of vector 𝐚 for AOPT = 2.0
















   ..
       !! processed by numpydoc !!

.. py:property:: v1
   :type: Optional[float]


   
   Get or set the Components of vector 𝐯 for AOPT = 3.0
















   ..
       !! processed by numpydoc !!

.. py:property:: v2
   :type: Optional[float]


   
   Get or set the Components of vector 𝐯 for AOPT = 3.0
















   ..
       !! processed by numpydoc !!

.. py:property:: v3
   :type: Optional[float]


   
   Get or set the Components of vector 𝐯 for AOPT = 3.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: d1
   :type: Optional[float]


   
   Get or set the Components of vector 𝐝 for AOPT = 2.0
















   ..
       !! processed by numpydoc !!

.. py:property:: d2
   :type: Optional[float]


   
   Get or set the Components of vector 𝐝 for AOPT = 2.0
















   ..
       !! processed by numpydoc !!

.. py:property:: d3
   :type: Optional[float]


   
   Get or set the Components of vector 𝐝 for AOPT = 2.0
















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
   :value: 'MICROMECHANICS_DRY_FABRIC'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





