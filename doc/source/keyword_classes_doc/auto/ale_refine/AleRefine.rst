





:class:`AleRefine`
==================


.. py:class:: ale_refine.AleRefine(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ALE_REFINE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: AleRefine

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the Set ID.
          * - :py:attr:`~type`
            - Get or set the Set type:
          * - :py:attr:`~nlvl`
            - Get or set the Number of refinement levels.
          * - :py:attr:`~mmsid`
            - Get or set the Multi-Material Set ID:
          * - :py:attr:`~ntotrf`
            - Get or set the Total number of ALE elements to refine.
          * - :py:attr:`~ncycrf`
            - Get or set the Number of cycles between each refinement.
          * - :py:attr:`~critrf`
            - Get or set the Refinement criterion:
          * - :py:attr:`~valrf`
            - Get or set the Criterion value to reach for the refinement.
          * - :py:attr:`~begrf`
            - Get or set the Time to begin the refinement.
          * - :py:attr:`~endrf`
            - Get or set the Time to end the refinement.
          * - :py:attr:`~layrf`
            - Get or set the Number of element layers to refine around a element reaching the refinement criterion.
          * - :py:attr:`~maxrm`
            - Get or set the Maximum number of child clusters to remove.
          * - :py:attr:`~ncycrm`
            - Get or set the Number of cycles between each deletion.
          * - :py:attr:`~critrm`
            - Get or set the Deletion criterion:
          * - :py:attr:`~valrm`
            - Get or set the Criterion value to reach in each child elements of a cluster for its deletion.
          * - :py:attr:`~begrm`
            - Get or set the Time to begin the deletion.
          * - :py:attr:`~endrm`
            - Get or set the Time to end the deletion.


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

    from ale_refine import AleRefine

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the Set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: type
   :type: int


   
   Get or set the Set type:
   EQ.0: ALE Part Set,
   EQ.1: ALE Part,
   EQ.2: Lagrangian Part Set coupled to ALE,
   EQ.3: Lagrangian Part coupled to ALE,
   EQ.4: Shell Set coupled to ALE,
   EQ.5: Solid Set.
















   ..
       !! processed by numpydoc !!

.. py:property:: nlvl
   :type: int


   
   Get or set the Number of refinement levels.
















   ..
       !! processed by numpydoc !!

.. py:property:: mmsid
   :type: int


   
   Get or set the Multi-Material Set ID:
   LT.0: only ALE elements with all the multi-material groups listed in*SET_MULTI-MATERIAL_GROUP_LIST can be refined.
   GT.0: ALE elements with at least one of the multi-material groups can be refined.
















   ..
       !! processed by numpydoc !!

.. py:property:: ntotrf
   :type: int


   
   Get or set the Total number of ALE elements to refine.
















   ..
       !! processed by numpydoc !!

.. py:property:: ncycrf
   :type: int


   
   Get or set the Number of cycles between each refinement.
















   ..
       !! processed by numpydoc !!

.. py:property:: critrf
   :type: int


   
   Get or set the Refinement criterion:
   EQ.0: static refinement.
   EQ.1: Pressure
   EQ.2: Relative Volume
   EQ.3: Volume Fraction.
















   ..
       !! processed by numpydoc !!

.. py:property:: valrf
   :type: float


   
   Get or set the Criterion value to reach for the refinement.
















   ..
       !! processed by numpydoc !!

.. py:property:: begrf
   :type: float


   
   Get or set the Time to begin the refinement.
















   ..
       !! processed by numpydoc !!

.. py:property:: endrf
   :type: float


   
   Get or set the Time to end the refinement.
















   ..
       !! processed by numpydoc !!

.. py:property:: layrf
   :type: int


   
   Get or set the Number of element layers to refine around a element reaching the refinement criterion.
















   ..
       !! processed by numpydoc !!

.. py:property:: maxrm
   :type: int


   
   Get or set the Maximum number of child clusters to remove.
   LT.0: for the whole run.GT
   GT.0: every NCYCRM cycles.
















   ..
       !! processed by numpydoc !!

.. py:property:: ncycrm
   :type: int


   
   Get or set the Number of cycles between each deletion.
















   ..
       !! processed by numpydoc !!

.. py:property:: critrm
   :type: int


   
   Get or set the Deletion criterion:
   EQ.0: no deletion.
   EQ.1: Pressure.
   EQ.2: Relative Volume.
   EQ.3: Volume Fraction.
















   ..
       !! processed by numpydoc !!

.. py:property:: valrm
   :type: float


   
   Get or set the Criterion value to reach in each child elements of a cluster for its deletion.
















   ..
       !! processed by numpydoc !!

.. py:property:: begrm
   :type: float


   
   Get or set the Time to begin the deletion.
















   ..
       !! processed by numpydoc !!

.. py:property:: endrm
   :type: float


   
   Get or set the Time to end the deletion.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ALE'


.. py:attribute:: subkeyword
   :value: 'REFINE'






