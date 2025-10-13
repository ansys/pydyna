





:class:`MatViscoelasticLooseFabric`
===================================


.. py:class:: mat_viscoelastic_loose_fabric.MatViscoelasticLooseFabric(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_VISCOELASTIC_LOOSE_FABRIC keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatViscoelasticLooseFabric

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification. A unique number has to be chosen.
          * - :py:attr:`~ro`
            - Get or set the Mass density.
          * - :py:attr:`~e1`
            - Get or set the Young's modulus in the yarn axial-direction.
          * - :py:attr:`~e2`
            - Get or set the Young's modulus in the yarn transverse-direction.
          * - :py:attr:`~g12`
            - Get or set the Shear modulus of the yarns
          * - :py:attr:`~eu`
            - Get or set the Ultimate strain at failure.
          * - :py:attr:`~thl`
            - Get or set the Yarn locking angle.
          * - :py:attr:`~thi`
            - Get or set the Initial brade angle.
          * - :py:attr:`~ta`
            - Get or set the Transition angle to locking
          * - :py:attr:`~w`
            - Get or set the Fiber width.
          * - :py:attr:`~s`
            - Get or set the Span between the fibers.
          * - :py:attr:`~t`
            - Get or set the Real fiber thickness.
          * - :py:attr:`~h`
            - Get or set the Effective fiber thickness
          * - :py:attr:`~eka`
            - Get or set the Elastic constant of element.
          * - :py:attr:`~eua`
            - Get or set the Ultimate strain of element
          * - :py:attr:`~vmb`
            - Get or set the Damping coefficient of element
          * - :py:attr:`~c`
            - Get or set the Coefficient of friction between the fibers.
          * - :py:attr:`~g23`
            - Get or set the transverse shear modulus.
          * - :py:attr:`~ekb`
            - Get or set the Elastic constant of element
          * - :py:attr:`~aopt`
            - Get or set the Material axes option:
          * - :py:attr:`~xp`
            - Get or set the
          * - :py:attr:`~yp`
            - Get or set the
          * - :py:attr:`~zp`
            - Get or set the .
          * - :py:attr:`~a1`
            - Get or set the
          * - :py:attr:`~a2`
            - Get or set the
          * - :py:attr:`~a3`
            - Get or set the
          * - :py:attr:`~v1`
            - Get or set the
          * - :py:attr:`~v2`
            - Get or set the
          * - :py:attr:`~v3`
            - Get or set the .
          * - :py:attr:`~d1`
            - Get or set the
          * - :py:attr:`~d2`
            - Get or set the
          * - :py:attr:`~d3`
            - Get or set the
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

    from mat_viscoelastic_loose_fabric import MatViscoelasticLooseFabric

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification. A unique number has to be chosen.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Mass density.
















   ..
       !! processed by numpydoc !!

.. py:property:: e1
   :type: Optional[float]


   
   Get or set the Young's modulus in the yarn axial-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: e2
   :type: Optional[float]


   
   Get or set the Young's modulus in the yarn transverse-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: g12
   :type: Optional[float]


   
   Get or set the Shear modulus of the yarns
















   ..
       !! processed by numpydoc !!

.. py:property:: eu
   :type: Optional[float]


   
   Get or set the Ultimate strain at failure.
















   ..
       !! processed by numpydoc !!

.. py:property:: thl
   :type: Optional[float]


   
   Get or set the Yarn locking angle.
















   ..
       !! processed by numpydoc !!

.. py:property:: thi
   :type: Optional[float]


   
   Get or set the Initial brade angle.
















   ..
       !! processed by numpydoc !!

.. py:property:: ta
   :type: Optional[float]


   
   Get or set the Transition angle to locking
















   ..
       !! processed by numpydoc !!

.. py:property:: w
   :type: Optional[float]


   
   Get or set the Fiber width.
















   ..
       !! processed by numpydoc !!

.. py:property:: s
   :type: Optional[float]


   
   Get or set the Span between the fibers.
















   ..
       !! processed by numpydoc !!

.. py:property:: t
   :type: Optional[float]


   
   Get or set the Real fiber thickness.
















   ..
       !! processed by numpydoc !!

.. py:property:: h
   :type: Optional[float]


   
   Get or set the Effective fiber thickness
















   ..
       !! processed by numpydoc !!

.. py:property:: eka
   :type: Optional[float]


   
   Get or set the Elastic constant of element.
















   ..
       !! processed by numpydoc !!

.. py:property:: eua
   :type: Optional[float]


   
   Get or set the Ultimate strain of element
















   ..
       !! processed by numpydoc !!

.. py:property:: vmb
   :type: Optional[float]


   
   Get or set the Damping coefficient of element
















   ..
       !! processed by numpydoc !!

.. py:property:: c
   :type: Optional[float]


   
   Get or set the Coefficient of friction between the fibers.
















   ..
       !! processed by numpydoc !!

.. py:property:: g23
   :type: Optional[float]


   
   Get or set the transverse shear modulus.
















   ..
       !! processed by numpydoc !!

.. py:property:: ekb
   :type: Optional[float]


   
   Get or set the Elastic constant of element
















   ..
       !! processed by numpydoc !!

.. py:property:: aopt
   :type: Optional[float]


   
   Get or set the Material axes option:
   EQ.0.0: locally orthotropic with material axes determined by
   element nodes 1, 2, and 4, as with *DEFINE_COORDINATE_NODES.
   EQ.2.0: globally orthotropic with material axes determined by vectors defined below, as with *DEFINE_COORDI_NATE_VECTOR.
   EQ.3.0: locally orthotropic material axes determined by rotating the material axes defined by   the cross product of the vector v with the element normal.
   LT.0.0: the absolute value of AOPT is a coordinate system ID number (CID on *DEFINE_COORDINATE_NODES,
   *DEFINE_COORDINATE_SYSTEM or *DEFINE_COOR_DINATE_VECTOR). Available with the R3 release of Version 971 and later.
















   ..
       !! processed by numpydoc !!

.. py:property:: xp
   :type: Optional[float]


   
   Get or set the 
















   ..
       !! processed by numpydoc !!

.. py:property:: yp
   :type: Optional[float]


   
   Get or set the 
















   ..
       !! processed by numpydoc !!

.. py:property:: zp
   :type: Optional[float]


   
   Get or set the .
















   ..
       !! processed by numpydoc !!

.. py:property:: a1
   :type: Optional[float]


   
   Get or set the 
















   ..
       !! processed by numpydoc !!

.. py:property:: a2
   :type: Optional[float]


   
   Get or set the 
















   ..
       !! processed by numpydoc !!

.. py:property:: a3
   :type: Optional[float]


   
   Get or set the 
















   ..
       !! processed by numpydoc !!

.. py:property:: v1
   :type: Optional[float]


   
   Get or set the 
















   ..
       !! processed by numpydoc !!

.. py:property:: v2
   :type: Optional[float]


   
   Get or set the 
















   ..
       !! processed by numpydoc !!

.. py:property:: v3
   :type: Optional[float]


   
   Get or set the .
















   ..
       !! processed by numpydoc !!

.. py:property:: d1
   :type: Optional[float]


   
   Get or set the 
















   ..
       !! processed by numpydoc !!

.. py:property:: d2
   :type: Optional[float]


   
   Get or set the 
















   ..
       !! processed by numpydoc !!

.. py:property:: d3
   :type: Optional[float]


   
   Get or set the 
















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
   :value: 'VISCOELASTIC_LOOSE_FABRIC'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





