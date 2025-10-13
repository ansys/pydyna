





:class:`ConstrainedJointStiffnessCylindrical`
=============================================


.. py:class:: constrained_joint_stiffness_cylindrical.ConstrainedJointStiffnessCylindrical(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONSTRAINED_JOINT_STIFFNESS_CYLINDRICAL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ConstrainedJointStiffnessCylindrical

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~jsid`
            - Get or set the Joint stiffness ID.
          * - :py:attr:`~pida`
            - Get or set the Part ID for rigid body A, see *PART.
          * - :py:attr:`~pidb`
            - Get or set the Part ID for rigid body B, see *PART.
          * - :py:attr:`~cida`
            - Get or set the Coordinate ID for rigid body A, see *DEFINE_COORDINATE_OPTION.
          * - :py:attr:`~cidb`
            - Get or set the Coordinate ID for rigid body B.
          * - :py:attr:`~jid`
            - Get or set the Joint ID for the joint reaction forces. If zero, tables can t be used in place of load curves for defining the frictional moments.
          * - :py:attr:`~lcidr`
            - Get or set the Load curve ID for r-force as a function of r-distance between the origins of
          * - :py:attr:`~lcidz`
            - Get or set the Load curve ID for z-force as a function of z-distance between the origins of
          * - :py:attr:`~dlcidr`
            - Get or set the Load curve or table ID for r-damping force as a function of rate of
          * - :py:attr:`~dlcidp`
            - Get or set the Load curve or table ID for p-damping force as a function of rate of
          * - :py:attr:`~dlcidz`
            - Get or set the Load curve or table ID for z-damping force as a function of rate of
          * - :py:attr:`~lcidt`
            - Get or set the Load curve ID for theta-moment as a function of angle theta between the
          * - :py:attr:`~dlcidt`
            - Get or set the Load curve ID for theta-moment as a function of rate of angle theta between the
          * - :py:attr:`~esr`
            - Get or set the Elastic stiffness for friction and stop displacement for r-translation. See Figure 0 - 3.
          * - :py:attr:`~ffr`
            - Get or set the Frictional force limiting value for r-translation. This option may also be
          * - :py:attr:`~esz`
            - Get or set the Elastic stiffness for friction and stop displacement for z-translation.
          * - :py:attr:`~ffz`
            - Get or set the Frictional force limiting value for 𝑧-translation. This option may also be thought of as an elastic - plastic spring.
          * - :py:attr:`~rad1`
            - Get or set the Radius of pin, must be strictly positive.
          * - :py:attr:`~rad2`
            - Get or set the Radius of hole, must be strictly larger than RAD1 to model a play in the connection.
          * - :py:attr:`~psdr`
            - Get or set the Stop displacement for r-translation. Ignored if zero.
          * - :py:attr:`~nsdz`
            - Get or set the Stop displacement for negative z-translation. Ignored if zero.
          * - :py:attr:`~psdz`
            - Get or set the Stop displacement for positive z-translation. Ignored if zero.


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

    from constrained_joint_stiffness_cylindrical import ConstrainedJointStiffnessCylindrical

Property detail
---------------

.. py:property:: jsid
   :type: Optional[int]


   
   Get or set the Joint stiffness ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: pida
   :type: Optional[int]


   
   Get or set the Part ID for rigid body A, see *PART.
















   ..
       !! processed by numpydoc !!

.. py:property:: pidb
   :type: Optional[int]


   
   Get or set the Part ID for rigid body B, see *PART.
















   ..
       !! processed by numpydoc !!

.. py:property:: cida
   :type: Optional[int]


   
   Get or set the Coordinate ID for rigid body A, see *DEFINE_COORDINATE_OPTION.
















   ..
       !! processed by numpydoc !!

.. py:property:: cidb
   :type: int


   
   Get or set the Coordinate ID for rigid body B.
   If zero, the coordinate ID for rigid body A is used (default).See *DEFINE_COORDINATE_OPTION.
















   ..
       !! processed by numpydoc !!

.. py:property:: jid
   :type: Optional[int]


   
   Get or set the Joint ID for the joint reaction forces. If zero, tables can t be used in place of load curves for defining the frictional moments.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidr
   :type: int


   
   Get or set the Load curve ID for r-force as a function of r-distance between the origins of
   CIDAand CIDB.See * DEFINE_CURVE.
   EQ.0: The applied force is set to 0.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidz
   :type: int


   
   Get or set the Load curve ID for z-force as a function of z-distance between the origins of
   CIDAand CIDB.See * DEFINE_CURVE.
   EQ.0: The applied force is set to 0.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: dlcidr
   :type: int


   
   Get or set the Load curve or table ID for r-damping force as a function of rate of
   r-distance per unit timeand optionally r - distance(if table) between the
   origins of CIDAand CIDB.See * DEFINE_CURVE or *DEFINE_TABLE.
   EQ.0: Damping is not considered.
















   ..
       !! processed by numpydoc !!

.. py:property:: dlcidp
   :type: int


   
   Get or set the Load curve or table ID for p-damping force as a function of rate of
   p-distance per unit timeand optionally r - distance(if table) between the
   origins of CIDAand CIDB.See * DEFINE_CURVE or *DEFINE_TABLE.
   EQ.0: Damping is not considered.
















   ..
       !! processed by numpydoc !!

.. py:property:: dlcidz
   :type: int


   
   Get or set the Load curve or table ID for z-damping force as a function of rate of
   z-distance per unit timeand optionally r - distance(if table) between the
   origins of CIDAand CIDB.See * DEFINE_CURVE or *DEFINE_TABLE.
   EQ.0: Damping is not considered.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidt
   :type: int


   
   Get or set the Load curve ID for theta-moment as a function of angle theta between the
   z-directions of CIDAand CIDB.See * DEFINE_CURVE.
   EQ.0: The applied moment is set to 0.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: dlcidt
   :type: int


   
   Get or set the Load curve ID for theta-moment as a function of rate of angle theta between the
   z-directions of CIDAand CIDB.See * DEFINE_CURVE.
   EQ.0: The applied moment is set to 0.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: esr
   :type: float


   
   Get or set the Elastic stiffness for friction and stop displacement for r-translation. See Figure 0 - 3.
   EQ.0.0: Friction and stop angles are inactive for r - translation.
















   ..
       !! processed by numpydoc !!

.. py:property:: ffr
   :type: float


   
   Get or set the Frictional force limiting value for r-translation. This option may also be
   thought of as an elastic - plastic spring.See Figure 0 - 3.
   EQ.0.0: Friction is inactive for r - translation.
   LT.0 : -FFR is the load curve ID defining the yield force as a function r - translation.
















   ..
       !! processed by numpydoc !!

.. py:property:: esz
   :type: float


   
   Get or set the Elastic stiffness for friction and stop displacement for z-translation.
   EQ.0.0: Friction and stop angles are inactive for z - translation.
















   ..
       !! processed by numpydoc !!

.. py:property:: ffz
   :type: float


   
   Get or set the Frictional force limiting value for 𝑧-translation. This option may also be thought of as an elastic - plastic spring.
   EQ.0.0: Friction is inactive for z - translation.
   LT.0 : -FFZ is the load curve ID defining the yield force as a function of z - translation.
















   ..
       !! processed by numpydoc !!

.. py:property:: rad1
   :type: float


   
   Get or set the Radius of pin, must be strictly positive.
















   ..
       !! processed by numpydoc !!

.. py:property:: rad2
   :type: float


   
   Get or set the Radius of hole, must be strictly larger than RAD1 to model a play in the connection.
















   ..
       !! processed by numpydoc !!

.. py:property:: psdr
   :type: float


   
   Get or set the Stop displacement for r-translation. Ignored if zero.
















   ..
       !! processed by numpydoc !!

.. py:property:: nsdz
   :type: float


   
   Get or set the Stop displacement for negative z-translation. Ignored if zero.
















   ..
       !! processed by numpydoc !!

.. py:property:: psdz
   :type: float


   
   Get or set the Stop displacement for positive z-translation. Ignored if zero.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONSTRAINED'


.. py:attribute:: subkeyword
   :value: 'JOINT_STIFFNESS_CYLINDRICAL'






