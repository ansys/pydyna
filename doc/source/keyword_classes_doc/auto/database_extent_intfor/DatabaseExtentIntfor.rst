





:class:`DatabaseExtentIntfor`
=============================


.. py:class:: database_extent_intfor.DatabaseExtentIntfor(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DATABASE_EXTENT_INTFOR keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DatabaseExtentIntfor

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~nglbv`
            - Get or set the Output option for global variables
          * - :py:attr:`~nvelo`
            - Get or set the Output option for nodal velocity
          * - :py:attr:`~npresu`
            - Get or set the Output option for pressures
          * - :py:attr:`~nshear`
            - Get or set the Output option for shear stress in r and s-direction
          * - :py:attr:`~nforce`
            - Get or set the Output option for X, Y and Z-force at nodes
          * - :py:attr:`~ngapc`
            - Get or set the Output option for contact gap at nodes and surface energy density
          * - :py:attr:`~nfail`
            - Get or set the Flag for display of deleted contact segments
          * - :py:attr:`~ieverf`
            - Get or set the Every interface force state for the  intfor  database is written to a separate file:
          * - :py:attr:`~nwear`
            - Get or set the Output contact wear data, see *CONTACT_ADD_WEAR
          * - :py:attr:`~nwusr`
            - Get or set the Number of user wear history variables to output from user defined wear routines, see *CONTACT_ADD_WEAR.
          * - :py:attr:`~nhuf`
            - Get or set the Number of user friction history variables to output from user
          * - :py:attr:`~ntied`
            - Get or set the Output tied segments for Mortar contact. See Remark 3.
          * - :py:attr:`~neng`
            - Get or set the Output (total) sliding interface energy density for Mortar contact, see also ENGOUT on * CONTROL_OUTPUT.
          * - :py:attr:`~npen`
            - Get or set the Output penetration info for Mortar contact. A nodal field gives the


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

    from database_extent_intfor import DatabaseExtentIntfor

Property detail
---------------

.. py:property:: nglbv
   :type: int


   
   Get or set the Output option for global variables
   EQ.-1: no
   EQ.1: yes.
















   ..
       !! processed by numpydoc !!

.. py:property:: nvelo
   :type: int


   
   Get or set the Output option for nodal velocity
   EQ.-1: no
   EQ.1: yes.
















   ..
       !! processed by numpydoc !!

.. py:property:: npresu
   :type: int


   
   Get or set the Output option for pressures
   EQ.-1: no
   EQ.1: output interface pressure only
   EQ.2: output interface press and peak pressure
   EQ.3: output interface pressure, peak pressure, and time to peak pressure.
















   ..
       !! processed by numpydoc !!

.. py:property:: nshear
   :type: int


   
   Get or set the Output option for shear stress in r and s-direction
   EQ.-1: no
   EQ.1: yes.
















   ..
       !! processed by numpydoc !!

.. py:property:: nforce
   :type: int


   
   Get or set the Output option for X, Y and Z-force at nodes
   EQ.-1: no
   EQ.1: yes.
















   ..
       !! processed by numpydoc !!

.. py:property:: ngapc
   :type: int


   
   Get or set the Output option for contact gap at nodes and surface energy density
   EQ.-1: no
   EQ.1: yes.
















   ..
       !! processed by numpydoc !!

.. py:property:: nfail
   :type: int


   
   Get or set the Flag for display of deleted contact segments
   EQ.0: all segments are displayed,
   EQ.1: remove deleted contact segments from display.
















   ..
       !! processed by numpydoc !!

.. py:property:: ieverf
   :type: int


   
   Get or set the Every interface force state for the  intfor  database is written to a separate file:
   EQ.0: more than one interface force state can be on each intfor file,
   EQ.1: one interface force output state only on each intfor file..
















   ..
       !! processed by numpydoc !!

.. py:property:: nwear
   :type: int


   
   Get or set the Output contact wear data, see *CONTACT_ADD_WEAR
   EQ.0: No output.
   EQ.1: Output wear depth.
   EQ.2: Output wear depth and sliding distance.
















   ..
       !! processed by numpydoc !!

.. py:property:: nwusr
   :type: int


   
   Get or set the Number of user wear history variables to output from user defined wear routines, see *CONTACT_ADD_WEAR.
















   ..
       !! processed by numpydoc !!

.. py:property:: nhuf
   :type: int


   
   Get or set the Number of user friction history variables to output from user
   defined friction routines; see *USER_INTERFACE_FRICTION
   (MPP only). See Remark 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: ntied
   :type: int


   
   Get or set the Output tied segments for Mortar contact. See Remark 3.
   EQ.0: No output
   EQ.1: Output.
















   ..
       !! processed by numpydoc !!

.. py:property:: neng
   :type: int


   
   Get or set the Output (total) sliding interface energy density for Mortar contact, see also ENGOUT on * CONTROL_OUTPUT.
   EQ.0: No output
   EQ.1 : Output.
















   ..
       !! processed by numpydoc !!

.. py:property:: npen
   :type: int


   
   Get or set the Output penetration info for Mortar contact. A nodal field gives the
   pentration for each node(magnitude and direction) in the sliding interface,
   see also PENOUT on* CONTROL_OUTPUT.
   EQ.0 : No output
   GE.1 : Output absolute penetration
   GE.2 : Output relative penetration.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DATABASE'


.. py:attribute:: subkeyword
   :value: 'EXTENT_INTFOR'






