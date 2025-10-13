





:class:`EmMat001`
=================


.. py:class:: em_mat_001.EmMat001(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EM_MAT_001 keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EmMat001

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
            - Get or set the defines the electromagnetism type of the material:
          * - :py:attr:`~sigma`
            - Get or set the initial electrical conductivity of the material.
          * - :py:attr:`~eosid`
            - Get or set the Optional ID of the EOS to be used for the electrical conductivity (see *EM_EOS card).
          * - :py:attr:`~deatht`
            - Get or set the Death time for the material. After DEATHT, the material will no longer be considered a conductor and removed from the EM solve.
          * - :py:attr:`~rdltype`
            - Get or set the Used for the application: composite Tshell battery, with **EM_RANDLES_TSHELL.Defines the function of the layer associated to MID:


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

    from em_mat_001 import EmMat001

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material ID: refers to MID in the *PART card.
















   ..
       !! processed by numpydoc !!

.. py:property:: mtype
   :type: int


   
   Get or set the defines the electromagnetism type of the material:
   EQ.0: Air or vacuum
   EQ.1: Insulator material: these materials have the same electromagnetism behavior as EQ.0
   EQ.2: Conductor carrying a source. In these conductors, the eddy current problem is solved, which gives the actual current density. Typically, this would correspond to the coil.
   EQ.3: Fluid conductor. In that case, MID refers to the ID given in * ICFD_PART.
   EQ.4: Conductor not connected to any current or voltage source, where the Eddy current problem is solved. Typically, this would correspond to the workpiece.
















   ..
       !! processed by numpydoc !!

.. py:property:: sigma
   :type: Optional[float]


   
   Get or set the initial electrical conductivity of the material.
















   ..
       !! processed by numpydoc !!

.. py:property:: eosid
   :type: Optional[int]


   
   Get or set the Optional ID of the EOS to be used for the electrical conductivity (see *EM_EOS card).
















   ..
       !! processed by numpydoc !!

.. py:property:: deatht
   :type: float


   
   Get or set the Death time for the material. After DEATHT, the material will no longer be considered a conductor and removed from the EM solve.
















   ..
       !! processed by numpydoc !!

.. py:property:: rdltype
   :type: int


   
   Get or set the Used for the application: composite Tshell battery, with **EM_RANDLES_TSHELL.Defines the function of the layer associated to MID:
   EQ.0:   Default. Conductor which is not part of a battery cell
   EQ.1:Current Collector Positive
   EQ.2: Positive Electrode
   EQ.3:Separator
   EQ.4:Negative Electrode
   EQ.5:Current Collector Negative
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EM'


.. py:attribute:: subkeyword
   :value: 'MAT_001'






