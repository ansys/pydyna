





:class:`DefineQuasarCoupling`
=============================


.. py:class:: define_quasar_coupling.DefineQuasarCoupling(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_QUASAR_COUPLING keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineQuasarCoupling

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~node`
            - Get or set the Coupled node/node set
          * - :py:attr:`~type`
            - Get or set the Region type:
          * - :py:attr:`~romid`
            - Get or set the Cadlm’s ROM ID
          * - :py:attr:`~pid`
            - Get or set the LS-DYNA Part/Part set ID
          * - :py:attr:`~ptype`
            - Get or set the Type for PID:
          * - :py:attr:`~iopt`
            - Get or set the Option for exchanging data between LS-DYNA/Cadlm Quasar
          * - :py:attr:`~cid`
            - Get or set the Reference coordinate system for transform data from LS-DYNA global system to Quasar local system
          * - :py:attr:`~ex_id`
            - Get or set the Node set to exclude from Quasar output.  LS-DYNA still expects the complete set of data. (Quasar can predict the forces from a reduced data set.)
          * - :py:attr:`~frcfrq`
            - Get or set the Number of cycles between QUASAR force updates for the coupling interface
          * - :py:attr:`~encname1`
            - Get or set the LS-DYNA output file to QUASAR
          * - :py:attr:`~encname2`
            - Get or set the QUASAR output file to LS-DYNA
          * - :py:attr:`~var1`
            - Get or set the User defined constant
          * - :py:attr:`~var2`
            - Get or set the User defined constant
          * - :py:attr:`~var3`
            - Get or set the User defined constant
          * - :py:attr:`~var4`
            - Get or set the User defined constant
          * - :py:attr:`~var5`
            - Get or set the User defined constant
          * - :py:attr:`~var6`
            - Get or set the User defined constant
          * - :py:attr:`~var7`
            - Get or set the User defined constant
          * - :py:attr:`~var8`
            - Get or set the User defined constant
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

    from define_quasar_coupling import DefineQuasarCoupling

Property detail
---------------

.. py:property:: node
   :type: Optional[int]


   
   Get or set the Coupled node/node set
















   ..
       !! processed by numpydoc !!

.. py:property:: type
   :type: int


   
   Get or set the Region type:
   EQ.0:   node ID
   EQ.1 : node set ID
















   ..
       !! processed by numpydoc !!

.. py:property:: romid
   :type: Optional[int]


   
   Get or set the Cadlm’s ROM ID
















   ..
       !! processed by numpydoc !!

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the LS-DYNA Part/Part set ID
















   ..
       !! processed by numpydoc !!

.. py:property:: ptype
   :type: int


   
   Get or set the Type for PID:
   EQ.0:   part ID(Default)
   EQ.1 : part set ID
















   ..
       !! processed by numpydoc !!

.. py:property:: iopt
   :type: int


   
   Get or set the Option for exchanging data between LS-DYNA/Cadlm Quasar
   EQ.0:   Default.LS - DYNA output nodal translational and rotational coordinates and input nodal translational and rotational forces
   EQ.1 : LS - DYNA output nodal translational and rotational displacements and input nodal translational and rotational forces
   EQ.2 : LS - DYNA output nodal translational coordinates and input nodal translational forces
   EQ.3 : LS - DYNA output nodal translational displacements and input nodal translational forces
















   ..
       !! processed by numpydoc !!

.. py:property:: cid
   :type: Optional[int]


   
   Get or set the Reference coordinate system for transform data from LS-DYNA global system to Quasar local system
















   ..
       !! processed by numpydoc !!

.. py:property:: ex_id
   :type: Optional[int]


   
   Get or set the Node set to exclude from Quasar output.  LS-DYNA still expects the complete set of data. (Quasar can predict the forces from a reduced data set.)
















   ..
       !! processed by numpydoc !!

.. py:property:: frcfrq
   :type: Optional[int]


   
   Get or set the Number of cycles between QUASAR force updates for the coupling interface
















   ..
       !! processed by numpydoc !!

.. py:property:: encname1
   :type: Optional[str]


   
   Get or set the LS-DYNA output file to QUASAR
















   ..
       !! processed by numpydoc !!

.. py:property:: encname2
   :type: Optional[str]


   
   Get or set the QUASAR output file to LS-DYNA
















   ..
       !! processed by numpydoc !!

.. py:property:: var1
   :type: Optional[float]


   
   Get or set the User defined constant
















   ..
       !! processed by numpydoc !!

.. py:property:: var2
   :type: Optional[float]


   
   Get or set the User defined constant
















   ..
       !! processed by numpydoc !!

.. py:property:: var3
   :type: Optional[float]


   
   Get or set the User defined constant
















   ..
       !! processed by numpydoc !!

.. py:property:: var4
   :type: Optional[float]


   
   Get or set the User defined constant
















   ..
       !! processed by numpydoc !!

.. py:property:: var5
   :type: Optional[float]


   
   Get or set the User defined constant
















   ..
       !! processed by numpydoc !!

.. py:property:: var6
   :type: Optional[float]


   
   Get or set the User defined constant
















   ..
       !! processed by numpydoc !!

.. py:property:: var7
   :type: Optional[float]


   
   Get or set the User defined constant
















   ..
       !! processed by numpydoc !!

.. py:property:: var8
   :type: Optional[float]


   
   Get or set the User defined constant
















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
   :value: 'QUASAR_COUPLING'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





