





:class:`DefineSphToSphCoupling`
===============================


.. py:class:: define_sph_to_sph_coupling.DefineSphToSphCoupling(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_SPH_TO_SPH_COUPLING keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineSphToSphCoupling

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ssid`
            - Get or set the Slave part or part set ID.
          * - :py:attr:`~msid`
            - Get or set the Master part or part set ID
          * - :py:attr:`~sstyp`
            - Get or set the Slave part type:
          * - :py:attr:`~mstyp`
            - Get or set the Master part type:
          * - :py:attr:`~ibox1`
            - Get or set the Box ID for slave parts
          * - :py:attr:`~ibox2`
            - Get or set the Box ID for master parts
          * - :py:attr:`~pfact`
            - Get or set the Penalty scale factor
          * - :py:attr:`~srad`
            - Get or set the Scale factor for nodes to nodes contact criteria, See Remark 3
          * - :py:attr:`~dfact`
            - Get or set the Penalty scale factor for contact damping coefficient, See Remark 4.
          * - :py:attr:`~isoft`
            - Get or set the Soft constraint option:
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

    from define_sph_to_sph_coupling import DefineSphToSphCoupling

Property detail
---------------

.. py:property:: ssid
   :type: Optional[int]


   
   Get or set the Slave part or part set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: msid
   :type: Optional[int]


   
   Get or set the Master part or part set ID
















   ..
       !! processed by numpydoc !!

.. py:property:: sstyp
   :type: int


   
   Get or set the Slave part type:
   EQ. 0:  Part set ID,
   EQ. 1:  Part ID
   ,
















   ..
       !! processed by numpydoc !!

.. py:property:: mstyp
   :type: int


   
   Get or set the Master part type:
   EQ. 0:  Part set ID,
   EQ. 1:  Part ID
















   ..
       !! processed by numpydoc !!

.. py:property:: ibox1
   :type: Optional[int]


   
   Get or set the Box ID for slave parts
















   ..
       !! processed by numpydoc !!

.. py:property:: ibox2
   :type: Optional[int]


   
   Get or set the Box ID for master parts
















   ..
       !! processed by numpydoc !!

.. py:property:: pfact
   :type: float


   
   Get or set the Penalty scale factor
















   ..
       !! processed by numpydoc !!

.. py:property:: srad
   :type: float


   
   Get or set the Scale factor for nodes to nodes contact criteria, See Remark 3
















   ..
       !! processed by numpydoc !!

.. py:property:: dfact
   :type: float


   
   Get or set the Penalty scale factor for contact damping coefficient, See Remark 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: isoft
   :type: int


   
   Get or set the Soft constraint option:
   EQ. 0: penalty formulation
   EQ. 1: soft constraint formulation
   The soft constraint may be necessary if the material constants of the parts in contact have a wide variation in the elastic bulk moduli. In the soft constraint option, the interface stiffness is based on the nodal mass and the global time step size.
















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
   :value: 'SPH_TO_SPH_COUPLING'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





