





:class:`Contact2DNodeToSolidTied`
=================================


.. py:class:: contact_2d_node_to_solid_tied.Contact2DNodeToSolidTied(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTACT_2D_NODE_TO_SOLID_TIED keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: Contact2DNodeToSolidTied

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~sph`
            - Get or set the Nodal set ID or part set ID for SPH nodes. If SPH>0, a node set ID is assumed, if SPH<0 a part set ID is assumed
          * - :py:attr:`~solid`
            - Get or set the Solid part set ID. SOLID<0 since only part set is allowed
          * - :py:attr:`~tbirth`
            - Get or set the Birth time for contact
          * - :py:attr:`~tdeath`
            - Get or set the Death time for contact
          * - :py:attr:`~soft`
            - Get or set the Soft constraint option:
          * - :py:attr:`~vc`
            - Get or set the Coefficient for viscous friction. This is used to limit the friction force to a maximum.
          * - :py:attr:`~offd`
            - Get or set the Contact offset distance for SPH nodes. It does not currently apply to tied contacts. Recommended to be half of the original particle
          * - :py:attr:`~pen`
            - Get or set the Scale factor for penalty.        EQ. 0.0: default set to: 1.0
          * - :py:attr:`~fs`
            - Get or set the Static coefficient of friction
          * - :py:attr:`~fd`
            - Get or set the Dynamic coefficient of friction
          * - :py:attr:`~dc`
            - Get or set the Exponential decay coefficient.


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

    from contact_2d_node_to_solid_tied import Contact2DNodeToSolidTied

Property detail
---------------

.. py:property:: sph
   :type: Optional[int]


   
   Get or set the Nodal set ID or part set ID for SPH nodes. If SPH>0, a node set ID is assumed, if SPH<0 a part set ID is assumed
















   ..
       !! processed by numpydoc !!

.. py:property:: solid
   :type: Optional[int]


   
   Get or set the Solid part set ID. SOLID<0 since only part set is allowed
















   ..
       !! processed by numpydoc !!

.. py:property:: tbirth
   :type: Optional[int]


   
   Get or set the Birth time for contact
















   ..
       !! processed by numpydoc !!

.. py:property:: tdeath
   :type: Optional[int]


   
   Get or set the Death time for contact
















   ..
       !! processed by numpydoc !!

.. py:property:: soft
   :type: int


   
   Get or set the Soft constraint option:
   EQ.0: penalty formulation,
   EQ.1: soft constraint formulation.
   The soft constraint may be necessary if the material constants of the
   parts in contact have a wide variation in the elastic bulk moduli. In
   the soft constraint option, the interface stiffness is based on the
   nodal mass and the global time step size. The soft constraint option
   is also recommended for axisymmetric simulations.
















   ..
       !! processed by numpydoc !!

.. py:property:: vc
   :type: Optional[float]


   
   Get or set the Coefficient for viscous friction. This is used to limit the friction force to a maximum.
















   ..
       !! processed by numpydoc !!

.. py:property:: offd
   :type: Optional[float]


   
   Get or set the Contact offset distance for SPH nodes. It does not currently apply to tied contacts. Recommended to be half of the original particle
   spacing in contact direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: pen
   :type: float


   
   Get or set the Scale factor for penalty.        EQ. 0.0: default set to: 1.0
















   ..
       !! processed by numpydoc !!

.. py:property:: fs
   :type: float


   
   Get or set the Static coefficient of friction
















   ..
       !! processed by numpydoc !!

.. py:property:: fd
   :type: float


   
   Get or set the Dynamic coefficient of friction
















   ..
       !! processed by numpydoc !!

.. py:property:: dc
   :type: float


   
   Get or set the Exponential decay coefficient.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTACT'


.. py:attribute:: subkeyword
   :value: '2D_NODE_TO_SOLID_TIED'






