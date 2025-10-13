





:class:`DualcesePart`
=====================


.. py:class:: dualcese_part.DualcesePart(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DUALCESE_PART keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DualcesePart

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Part ID (must be different from any other *DUALCESE_PART part ID or from a *DUALCESE_PART_MULTIPHASE PID
          * - :py:attr:`~mid`
            - Get or set the Material ID refering to DUALCESE_MAT_material
          * - :py:attr:`~eosid`
            - Get or set the Equation of state ID defined by a *DUALCESE_EOS_... card
          * - :py:attr:`~fsitype`
            - Get or set the FSI type to use on this part:
          * - :py:attr:`~mmshid`
            - Get or set the ID of the mesh motion algorithm to use for the moving mesh FSI solver on this part (region of the current dual CESE mesh)).  This ID refers to a *DUALCESE_CONTROL_MESH_MOV card ID.


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

    from dualcese_part import DualcesePart

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID (must be different from any other *DUALCESE_PART part ID or from a *DUALCESE_PART_MULTIPHASE PID
















   ..
       !! processed by numpydoc !!

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material ID refering to DUALCESE_MAT_material
















   ..
       !! processed by numpydoc !!

.. py:property:: eosid
   :type: Optional[int]


   
   Get or set the Equation of state ID defined by a *DUALCESE_EOS_... card
















   ..
       !! processed by numpydoc !!

.. py:property:: fsitype
   :type: Optional[str]


   
   Get or set the FSI type to use on this part:
   BLANK:  no FSI performed
   EQ.IBM : Immersed boundary FSI solver
   EQ.MOVMESH : Moving mesh FSI solver(FSITYPE =MMM may also be used for the same effect
















   ..
       !! processed by numpydoc !!

.. py:property:: mmshid
   :type: Optional[int]


   
   Get or set the ID of the mesh motion algorithm to use for the moving mesh FSI solver on this part (region of the current dual CESE mesh)).  This ID refers to a *DUALCESE_CONTROL_MESH_MOV card ID.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DUALCESE'


.. py:attribute:: subkeyword
   :value: 'PART'






