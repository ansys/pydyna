





:class:`ContactRigidSurface`
============================


.. py:class:: contact_rigid_surface.ContactRigidSurface(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTACT_RIGID_SURFACE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ContactRigidSurface

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~cid`
            - Get or set the Contact interface ID. This must be a unique number.
          * - :py:attr:`~psid`
            - Get or set the Part set ID of all parts that may contact the rigid surface. See *SET_PART.
          * - :py:attr:`~boxid`
            - Get or set the Include only nodes of the part set that are within the specified box, see *DEFINE_BOX, in contact definition.
          * - :py:attr:`~segid`
            - Get or set the Segment set ID defining the rigid surface. See *SET_SEGMENT.
          * - :py:attr:`~fs`
            - Get or set the Static coefficient of friction.
          * - :py:attr:`~fd`
            - Get or set the Dynamic coefficient of friction.
          * - :py:attr:`~dc`
            - Get or set the Exponential decay coefficient.
          * - :py:attr:`~vc`
            - Get or set the Coefficient for viscous friction.
          * - :py:attr:`~lcidx`
            - Get or set the Load curve ID defining x-direction motion.
          * - :py:attr:`~lcidy`
            - Get or set the Load curve ID defining y-direction motion.
          * - :py:attr:`~lcidz`
            - Get or set the Load curve ID defining z-direction motion.
          * - :py:attr:`~fslcid`
            - Get or set the Load curve ID defining the static coefficient of friction as a function of interface pressure. This option applies to shell segments only.
          * - :py:attr:`~fdlcid`
            - Get or set the Load curve ID defining the dynamic coefficient of friction as a function of interface pressure. This option applies to shell segments only.
          * - :py:attr:`~sfs`
            - Get or set the Scale factor on default slave penalty stiffness, see also *CONTROL_ CONTACT.
          * - :py:attr:`~stthk`
            - Get or set the Optional thickness for slave surface (overrides true thickness). This option applies to contact with shell, solid, and beam elements. True thickness is the element thickness of the shell elements. Thickness offsets are not used for solid element unless this option is specified.
          * - :py:attr:`~sfthk`
            - Get or set the Scale factor for slave surface thickness (scales true thickness). This option applies only to contact with shell elements. True thickness is the element thickness of the shell elements.
          * - :py:attr:`~xpene`
            - Get or set the Contact surface maximum penetration check multiplier. If the penetration of a node through the rigid surface exceeds the product of XPENE and the slave node thickness, the node is set free.
          * - :py:attr:`~bsort`
            - Get or set the Number of cycles between bucket sorts. The default value is set to 10.0 but can be much larger, e.g., 50-100, for fully connected surfaces.
          * - :py:attr:`~ctype`
            - Get or set the The contact formulation. The default, CTYPE=0, is equivalent to the ONE_WAY_SURFACE_TO_SURFACE formulation, and CTYPE=1 is a penalty formulation. If the slave surface belongs to a rigid body, CTYPE=1 must be used.


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

    from contact_rigid_surface import ContactRigidSurface

Property detail
---------------

.. py:property:: cid
   :type: Optional[int]


   
   Get or set the Contact interface ID. This must be a unique number.
















   ..
       !! processed by numpydoc !!

.. py:property:: psid
   :type: Optional[int]


   
   Get or set the Part set ID of all parts that may contact the rigid surface. See *SET_PART.
















   ..
       !! processed by numpydoc !!

.. py:property:: boxid
   :type: int


   
   Get or set the Include only nodes of the part set that are within the specified box, see *DEFINE_BOX, in contact definition.
   EQ.0: all nodes from the part set, PSID, will be included in the contact (default).
















   ..
       !! processed by numpydoc !!

.. py:property:: segid
   :type: Optional[int]


   
   Get or set the Segment set ID defining the rigid surface. See *SET_SEGMENT.
















   ..
       !! processed by numpydoc !!

.. py:property:: fs
   :type: float


   
   Get or set the Static coefficient of friction.
















   ..
       !! processed by numpydoc !!

.. py:property:: fd
   :type: float


   
   Get or set the Dynamic coefficient of friction.
















   ..
       !! processed by numpydoc !!

.. py:property:: dc
   :type: float


   
   Get or set the Exponential decay coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: vc
   :type: float


   
   Get or set the Coefficient for viscous friction.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidx
   :type: int


   
   Get or set the Load curve ID defining x-direction motion.
   EQ.0: There is no motion in the x-coordinate system.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidy
   :type: int


   
   Get or set the Load curve ID defining y-direction motion.
   EQ.0: There is no motion in the y-coordinate system.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidz
   :type: int


   
   Get or set the Load curve ID defining z-direction motion.
   EQ.0: There is no motion in the z-coordinate system.
















   ..
       !! processed by numpydoc !!

.. py:property:: fslcid
   :type: int


   
   Get or set the Load curve ID defining the static coefficient of friction as a function of interface pressure. This option applies to shell segments only.
















   ..
       !! processed by numpydoc !!

.. py:property:: fdlcid
   :type: int


   
   Get or set the Load curve ID defining the dynamic coefficient of friction as a function of interface pressure. This option applies to shell segments only.
















   ..
       !! processed by numpydoc !!

.. py:property:: sfs
   :type: float


   
   Get or set the Scale factor on default slave penalty stiffness, see also *CONTROL_ CONTACT.
   Default is set to 1.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: stthk
   :type: float


   
   Get or set the Optional thickness for slave surface (overrides true thickness). This option applies to contact with shell, solid, and beam elements. True thickness is the element thickness of the shell elements. Thickness offsets are not used for solid element unless this option is specified.
   Default is set to 0.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: sfthk
   :type: float


   
   Get or set the Scale factor for slave surface thickness (scales true thickness). This option applies only to contact with shell elements. True thickness is the element thickness of the shell elements.
   Default is set to 1.0
















   ..
       !! processed by numpydoc !!

.. py:property:: xpene
   :type: float


   
   Get or set the Contact surface maximum penetration check multiplier. If the penetration of a node through the rigid surface exceeds the product of XPENE and the slave node thickness, the node is set free.
   Default is set to 4.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: bsort
   :type: float


   
   Get or set the Number of cycles between bucket sorts. The default value is set to 10.0 but can be much larger, e.g., 50-100, for fully connected surfaces.
















   ..
       !! processed by numpydoc !!

.. py:property:: ctype
   :type: int


   
   Get or set the The contact formulation. The default, CTYPE=0, is equivalent to the ONE_WAY_SURFACE_TO_SURFACE formulation, and CTYPE=1 is a penalty formulation. If the slave surface belongs to a rigid body, CTYPE=1 must be used.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTACT'


.. py:attribute:: subkeyword
   :value: 'RIGID_SURFACE'






