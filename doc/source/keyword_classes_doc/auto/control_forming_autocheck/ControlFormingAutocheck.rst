





:class:`ControlFormingAutocheck`
================================


.. py:class:: control_forming_autocheck.ControlFormingAutocheck(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_FORMING_AUTOCHECK keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlFormingAutocheck

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~icheck`
            - Get or set the Tool mesh checking/fixing flag:
          * - :py:attr:`~igd`
            - Get or set the Not used.
          * - :py:attr:`~ioffset`
            - Get or set the Tool mesh offset flag. Note this variable works only when IOUTPUT is defined.  When ICHECK is set to “1”:
          * - :py:attr:`~ioutputp`
            - Get or set the Output option flag:


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

    from control_forming_autocheck import ControlFormingAutocheck

Property detail
---------------

.. py:property:: icheck
   :type: int


   
   Get or set the Tool mesh checking/fixing flag:
   ICHECK.EQ.0:    Do not activate mesh checking/fixing feature.
   ICHECK.EQ.1:    Activate comprehensive mesh check and fix those problematic tool meshes which cause unreasonable forming results and/or error termination.  The keyword with this variable only can be inserted in any regular forming simulation; the simulation will continue after tool mesh checking/fixing is done, see Example 1.  The fixed tool meshes can be viewed and recovered from the resulting D3PLOT files.  If the termination time is set to “0.0” or the keyword *CONTROL_TERMINATION is absent all together, the simulation will terminate as soon as  “checking/fixing” is completed, and fixed tool meshes can be extracted from the D3PLOT files.
















   ..
       !! processed by numpydoc !!

.. py:property:: igd
   :type: Optional[int]


   
   Get or set the Not used.
















   ..
       !! processed by numpydoc !!

.. py:property:: ioffset
   :type: int


   
   Get or set the Tool mesh offset flag. Note this variable works only when IOUTPUT is defined.  When ICHECK is set to “1”:
   IOFFSET.EQ.0:   Do not offset rigid tool mesh.  The sheet blank does not need to be present.  IOUTPUT must be defined.  All corresponding files for defined IOUTPUT=1~4 will still be output; however, both rigid_offset.inc and rigid_offset_before.inc will be the same – the checked and fixed tool mesh file without offset.  See Example 2.
   IOFFSET.EQ.1:   Perform rigid tool mesh offset using the variable MST (see Figure 0-1) defined in *CONTACT_FORMING.... The blank must be defined and positioned completely above or below the rigid tool to be offset.  Both part ID and part SID (MSTYP) can be used in defining the MSID.  IOUTPUT must also be defined.
















   ..
       !! processed by numpydoc !!

.. py:property:: ioutputp
   :type: Optional[int]


   
   Get or set the Output option flag:
   IOUTPUT.EQ.1:   Output offset rigid tool meshes into a keyword file rigid_offset.inc, and terminates the simulation.
   IOUTPUT.EQ.2:   Output offset rigid tool meshes as well as nodes used to define draw beads into a keyword file rigid_offset.inc, and terminates the simulation.  See Example 4.
   IOUTPUT.EQ.3:   Output checked/fixed tool as well as offset rigid tool meshes into two separate keyword files, rigid_offset_before.inc, and rigid_offset.inc, respectively, and terminates the simulation.  See Example 3.
   IOUTPUT.EQ.4:   Output checked/fixed tool meshes, offset rigid tool meshes as well as the nodes used to define draw beads into two separate keyword files, rigid_offset_before.inc, and rigid_offset.inc, respectively, and terminates the simulation.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'FORMING_AUTOCHECK'






