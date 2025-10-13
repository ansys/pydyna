





:class:`DefineBoxSphLocal`
==========================


.. py:class:: define_box_sph_local.DefineBoxSphLocal(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_BOX_SPH_LOCAL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineBoxSphLocal

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~boxid`
            - Get or set the Box ID. A unique number must be defined.
          * - :py:attr:`~xmn`
            - Get or set the Minimum x-coordinate.
          * - :py:attr:`~xmx`
            - Get or set the Maximum x-coordinate.
          * - :py:attr:`~ymn`
            - Get or set the Minimum y-coordinate.
          * - :py:attr:`~ymx`
            - Get or set the Maximum y-coordinate.
          * - :py:attr:`~zmn`
            - Get or set the Minimum z-coordinate.
          * - :py:attr:`~zmx`
            - Get or set the Maximum z-coordinate.
          * - :py:attr:`~vid`
            - Get or set the Vector ID of DOF, see *DEFINE_VECTOR.
          * - :py:attr:`~lcid`
            - Get or set the Load curve ID to describe motion value versus time, see *DEFINE_CURVE
          * - :py:attr:`~vd`
            - Get or set the Velocity/Displacement flag:
          * - :py:attr:`~nid`
            - Get or set the Referential nodal ID for VD=2 (SPH box will move with this node)
          * - :py:attr:`~ireact`
            - Get or set the Reactivation flag:
          * - :py:attr:`~ibuff`
            - Get or set the Buffer zone flag:
          * - :py:attr:`~ishow`
            - Get or set the Create dummy part to visualize position of activation box in post-processing.
          * - :py:attr:`~pid`
            - Get or set the Part ID used for visualization if ISHOW=1.
          * - :py:attr:`~xx`
            - Get or set the X-coordinate on local x-axis.  Origin lies at (0,0,0).  Define if the LOCAL option is active
          * - :py:attr:`~yx`
            - Get or set the Y-coordinate on local x-axis.  Define if the LOCAL option is active..
          * - :py:attr:`~zx`
            - Get or set the Z-coordinate on local x-axis.  Define if the LOCAL option is active.
          * - :py:attr:`~xv`
            - Get or set the X-coordinate of local x-y vector.  Define if the LOCAL option is active
          * - :py:attr:`~yv`
            - Get or set the Y-coordinate of local x-y vector.  Define if the LOCAL option is active.
          * - :py:attr:`~zv`
            - Get or set the Z-coordinate of local x-y vector.  Define if the LOCAL option is active..
          * - :py:attr:`~cx`
            - Get or set the X-global coordinate of offset vector to origin of local system.  Define if the LOCAL option is active.
          * - :py:attr:`~cy`
            - Get or set the Y-global coordinate of offset vector to origin of local system.  Define if the LOCAL option is active.
          * - :py:attr:`~cz`
            - Get or set the Z-global coordinate of offset vector to origin of local system.  Define if the LOCAL option is active.
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

    from define_box_sph_local import DefineBoxSphLocal

Property detail
---------------

.. py:property:: boxid
   :type: Optional[int]


   
   Get or set the Box ID. A unique number must be defined.
















   ..
       !! processed by numpydoc !!

.. py:property:: xmn
   :type: float


   
   Get or set the Minimum x-coordinate.
















   ..
       !! processed by numpydoc !!

.. py:property:: xmx
   :type: float


   
   Get or set the Maximum x-coordinate.
















   ..
       !! processed by numpydoc !!

.. py:property:: ymn
   :type: float


   
   Get or set the Minimum y-coordinate.
















   ..
       !! processed by numpydoc !!

.. py:property:: ymx
   :type: float


   
   Get or set the Maximum y-coordinate.
















   ..
       !! processed by numpydoc !!

.. py:property:: zmn
   :type: float


   
   Get or set the Minimum z-coordinate.
















   ..
       !! processed by numpydoc !!

.. py:property:: zmx
   :type: float


   
   Get or set the Maximum z-coordinate.
















   ..
       !! processed by numpydoc !!

.. py:property:: vid
   :type: int


   
   Get or set the Vector ID of DOF, see *DEFINE_VECTOR.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: int


   
   Get or set the Load curve ID to describe motion value versus time, see *DEFINE_CURVE
















   ..
       !! processed by numpydoc !!

.. py:property:: vd
   :type: int


   
   Get or set the Velocity/Displacement flag:
   EQ.0: velocity,
   EQ.1: displacement
   EQ.2:  referential node
















   ..
       !! processed by numpydoc !!

.. py:property:: nid
   :type: int


   
   Get or set the Referential nodal ID for VD=2 (SPH box will move with this node)
















   ..
       !! processed by numpydoc !!

.. py:property:: ireact
   :type: int


   
   Get or set the Reactivation flag:
   EQ.0:   particles outside of the box are permanently deactivated,
   EQ.1 : deactivated particles get reactivated when they enter the box
















   ..
       !! processed by numpydoc !!

.. py:property:: ibuff
   :type: int


   
   Get or set the Buffer zone flag:
   EQ.0: particles on the edge of the box don’t get any special treatment.
   EQ.1 : particles on the edge of the box are frozen in space and act as neighbors for active particles inside the box.
   This option is mainly used for fluid simulations to prevent the fluid from spilling out of the activation box.
















   ..
       !! processed by numpydoc !!

.. py:property:: ishow
   :type: int


   
   Get or set the Create dummy part to visualize position of activation box in post-processing.
   EQ.0: no part is created.
   EQ.1 : a dummy part is added for visualization
















   ..
       !! processed by numpydoc !!

.. py:property:: pid
   :type: int


   
   Get or set the Part ID used for visualization if ISHOW=1.
   EQ.0:   a unique Part ID is automatically created.
   GT.0 : the part created by ISHOW = 1 is numbered PID.This should be a unique part ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: xx
   :type: float


   
   Get or set the X-coordinate on local x-axis.  Origin lies at (0,0,0).  Define if the LOCAL option is active
















   ..
       !! processed by numpydoc !!

.. py:property:: yx
   :type: float


   
   Get or set the Y-coordinate on local x-axis.  Define if the LOCAL option is active..
















   ..
       !! processed by numpydoc !!

.. py:property:: zx
   :type: float


   
   Get or set the Z-coordinate on local x-axis.  Define if the LOCAL option is active.
















   ..
       !! processed by numpydoc !!

.. py:property:: xv
   :type: float


   
   Get or set the X-coordinate of local x-y vector.  Define if the LOCAL option is active
















   ..
       !! processed by numpydoc !!

.. py:property:: yv
   :type: float


   
   Get or set the Y-coordinate of local x-y vector.  Define if the LOCAL option is active.
















   ..
       !! processed by numpydoc !!

.. py:property:: zv
   :type: float


   
   Get or set the Z-coordinate of local x-y vector.  Define if the LOCAL option is active..
















   ..
       !! processed by numpydoc !!

.. py:property:: cx
   :type: float


   
   Get or set the X-global coordinate of offset vector to origin of local system.  Define if the LOCAL option is active.
















   ..
       !! processed by numpydoc !!

.. py:property:: cy
   :type: float


   
   Get or set the Y-global coordinate of offset vector to origin of local system.  Define if the LOCAL option is active.
















   ..
       !! processed by numpydoc !!

.. py:property:: cz
   :type: float


   
   Get or set the Z-global coordinate of offset vector to origin of local system.  Define if the LOCAL option is active.
















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
   :value: 'DEFINE'


.. py:attribute:: subkeyword
   :value: 'BOX_SPH_LOCAL'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





