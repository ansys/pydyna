





:class:`DualcesePartMultiphase`
===============================


.. py:class:: dualcese_part_multiphase.DualcesePartMultiphase(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DUALCESE_PART_MULTIPHASE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DualcesePartMultiphase

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Part ID (must be different from any PID on a *DUALCESE_PART card)
          * - :py:attr:`~react_id`
            - Get or set the ID of chemical reaction rate model (see *DUALCESE_REACTION_RATE_... cards)
          * - :py:attr:`~eossid`
            - Get or set the Set ID of multiphase EOS set specification (see *DUALCESE_EOS_SET)
          * - :py:attr:`~mid`
            - Get or set the Material ID defined by a *DUALCESE_MAT_... card
          * - :py:attr:`~fsitype`
            - Get or set the FSI type to use on this part:
          * - :py:attr:`~mmshid`
            - Get or set the ID of the mesh motion algorithm to use for the moving mesh FSI solver on this part (region of the current dual CESE mesh).  This ID refers to a *DUALCESE_CONTROL_MESH_MOV card ID.


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

    from dualcese_part_multiphase import DualcesePartMultiphase

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID (must be different from any PID on a *DUALCESE_PART card)
















   ..
       !! processed by numpydoc !!

.. py:property:: react_id
   :type: Optional[int]


   
   Get or set the ID of chemical reaction rate model (see *DUALCESE_REACTION_RATE_... cards)
















   ..
       !! processed by numpydoc !!

.. py:property:: eossid
   :type: Optional[int]


   
   Get or set the Set ID of multiphase EOS set specification (see *DUALCESE_EOS_SET)
















   ..
       !! processed by numpydoc !!

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material ID defined by a *DUALCESE_MAT_... card
















   ..
       !! processed by numpydoc !!

.. py:property:: fsitype
   :type: Optional[str]


   
   Get or set the FSI type to use on this part:
   EQ.<BLANK>:     If left blank, no FSI is performed.
   EQ.IBM: Immersed boundary FSI solver
   EQ.MOVMESH:     Moving mesh FSI solver(FSITYPE = MMM may also be used for the same effect)
















   ..
       !! processed by numpydoc !!

.. py:property:: mmshid
   :type: Optional[int]


   
   Get or set the ID of the mesh motion algorithm to use for the moving mesh FSI solver on this part (region of the current dual CESE mesh).  This ID refers to a *DUALCESE_CONTROL_MESH_MOV card ID.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DUALCESE'


.. py:attribute:: subkeyword
   :value: 'PART_MULTIPHASE'






