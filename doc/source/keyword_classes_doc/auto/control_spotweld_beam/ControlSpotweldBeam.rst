





:class:`ControlSpotweldBeam`
============================


.. py:class:: control_spotweld_beam.ControlSpotweldBeam(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_SPOTWELD_BEAM keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlSpotweldBeam

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~lct`
            - Get or set the Load curve ID for scaling the response in tension based on the shell element size
          * - :py:attr:`~lcs`
            - Get or set the Load curve ID for scaling the response in shear based on the shell element size.
          * - :py:attr:`~t_ort`
            - Get or set the Table ID for scaling the tension response (and shear response if T_ORS=0) based on the location of the beam node relative to the centroid of the shell.
          * - :py:attr:`~prtflg`
            - Get or set the Set this flag to 1 to print for each spotweld attachment: the beam, node, and shell ID's, the parametric coordinates that define the constraint location, the angle used in the table lookup, and the three scale factors obtained from the load curves and table lookup.
          * - :py:attr:`~t_ors`
            - Get or set the Optional table ID for scaling the shear response based on the location of the beam node relative to the centroid of the shell.
          * - :py:attr:`~rpbhx`
            - Get or set the Replace each spot weld beam element with a cluster of RPBHX solid elements.  RPBHX may be set to 1, 4, or 8.  When RPBHX is set to 4 or 8, a table is generated to output the force and moment resultants into the SWFORC file, if this file is active.  This table is described by the keyword: *DEFINE_HEX_SPOTWELD_ASSEMBLY.   The ID's of the beam elements are used as the cluster spot weld ID's so the ID's in the SWFORC file are unchanged.  The beam elements are automatically deleted from the calculation, and the section and material data is automatically changed to be used with solid elements.
          * - :py:attr:`~bmsid`
            - Get or set the Optional beam setID defining the beam element ID's that are to be converted to hex assemblies. If zero, all spotweld beam elements are converted to hex assemblies. See the keyword, *SET_BEAM_GENERAL for an efficient way of defining beam sets
          * - :py:attr:`~id_off`
            - Get or set the This optional ID offset applies if and only if BMSID is nonzero. Beams, which share part ID's with beams that are converted to hex assemblies, will be assigned new part ID's by adding to the original part ID the value of ID_OFF. If ID_OFF is zero the new part ID for such beams will be assigned to be larger than the largest part ID in the model


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

    from control_spotweld_beam import ControlSpotweldBeam

Property detail
---------------

.. py:property:: lct
   :type: int


   
   Get or set the Load curve ID for scaling the response in tension based on the shell element size
















   ..
       !! processed by numpydoc !!

.. py:property:: lcs
   :type: Optional[int]


   
   Get or set the Load curve ID for scaling the response in shear based on the shell element size.
















   ..
       !! processed by numpydoc !!

.. py:property:: t_ort
   :type: int


   
   Get or set the Table ID for scaling the tension response (and shear response if T_ORS=0) based on the location of the beam node relative to the centroid of the shell.
















   ..
       !! processed by numpydoc !!

.. py:property:: prtflg
   :type: int


   
   Get or set the Set this flag to 1 to print for each spotweld attachment: the beam, node, and shell ID's, the parametric coordinates that define the constraint location, the angle used in the table lookup, and the three scale factors obtained from the load curves and table lookup.
















   ..
       !! processed by numpydoc !!

.. py:property:: t_ors
   :type: int


   
   Get or set the Optional table ID for scaling the shear response based on the location of the beam node relative to the centroid of the shell.
















   ..
       !! processed by numpydoc !!

.. py:property:: rpbhx
   :type: int


   
   Get or set the Replace each spot weld beam element with a cluster of RPBHX solid elements.  RPBHX may be set to 1, 4, or 8.  When RPBHX is set to 4 or 8, a table is generated to output the force and moment resultants into the SWFORC file, if this file is active.  This table is described by the keyword: *DEFINE_HEX_SPOTWELD_ASSEMBLY.   The ID's of the beam elements are used as the cluster spot weld ID's so the ID's in the SWFORC file are unchanged.  The beam elements are automatically deleted from the calculation, and the section and material data is automatically changed to be used with solid elements.
















   ..
       !! processed by numpydoc !!

.. py:property:: bmsid
   :type: int


   
   Get or set the Optional beam setID defining the beam element ID's that are to be converted to hex assemblies. If zero, all spotweld beam elements are converted to hex assemblies. See the keyword, *SET_BEAM_GENERAL for an efficient way of defining beam sets
















   ..
       !! processed by numpydoc !!

.. py:property:: id_off
   :type: int


   
   Get or set the This optional ID offset applies if and only if BMSID is nonzero. Beams, which share part ID's with beams that are converted to hex assemblies, will be assigned new part ID's by adding to the original part ID the value of ID_OFF. If ID_OFF is zero the new part ID for such beams will be assigned to be larger than the largest part ID in the model
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'SPOTWELD_BEAM'






