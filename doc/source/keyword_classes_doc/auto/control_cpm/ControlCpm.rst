





:class:`ControlCpm`
===================


.. py:class:: control_cpm.ControlCpm(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_CPM keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlCpm

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~cpmout`
            - Get or set the Control CPM output database to d3plot
          * - :py:attr:`~np2p`
            - Get or set the Number of cycles for repartition particle among processors. This option is only used in LS-DYNA/MPP. (Default=5)
          * - :py:attr:`~ncpmts`
            - Get or set the Time step size estimation:
          * - :py:attr:`~cpmerr`
            - Get or set the EQ.0: disable checking and only output warning messages (Default)
          * - :py:attr:`~sffdc`
            - Get or set the Scale factor for the force decay constant. The default value is 1.0 and allowable arrange is [0.01,100.0].
          * - :py:attr:`~cpmmf`
            - Get or set the Flag to consider airbag system velocity based on the coordinates system defined by fields NID1, NID2, and NID3 on *AIRBAG_PARTICLE:


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

    from control_cpm import ControlCpm

Property detail
---------------

.. py:property:: cpmout
   :type: int


   
   Get or set the Control CPM output database to d3plot
   EQ.11: full CPM database in version 3 format (default)
   EQ.21: full CPM database in version 4 format
   EQ.22: CPM coordinates only in version 4 format
   EQ.23: CPM summary only in version 4 format
















   ..
       !! processed by numpydoc !!

.. py:property:: np2p
   :type: int


   
   Get or set the Number of cycles for repartition particle among processors. This option is only used in LS-DYNA/MPP. (Default=5)
















   ..
       !! processed by numpydoc !!

.. py:property:: ncpmts
   :type: int


   
   Get or set the Time step size estimation:
   EQ.0: not consider CPM (default)
   EQ.1: use 1 micro-second as CPM time step size. This provides a better time step size if the model is made by rigid body
















   ..
       !! processed by numpydoc !!

.. py:property:: cpmerr
   :type: int


   
   Get or set the EQ.0: disable checking and only output warning messages (Default)
   EQ.1: enable error checking. If it detects any problem, the code
   will error terminate the job, or try to fix the problem. Activated checks include:
   1.  Airbag integrity (see Remark 2)
   2.  Chamber integrity: this step applies the airbag in  tegrity check to the chamber.
   3.  Inconsistent orientation between the shell reference geometry and FEM shell connectivity
















   ..
       !! processed by numpydoc !!

.. py:property:: sffdc
   :type: float


   
   Get or set the Scale factor for the force decay constant. The default value is 1.0 and allowable arrange is [0.01,100.0].
















   ..
       !! processed by numpydoc !!

.. py:property:: cpmmf
   :type: int


   
   Get or set the Flag to consider airbag system velocity based on the coordinates system defined by fields NID1, NID2, and NID3 on *AIRBAG_PARTICLE:
   EQ.0:   no(default)
   EQ.1 : yes.The flow energy from the rigid body motion is fed back to the CPM particles.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'CPM'






