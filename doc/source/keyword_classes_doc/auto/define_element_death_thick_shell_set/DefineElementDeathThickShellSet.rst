





:class:`DefineElementDeathThickShellSet`
========================================


.. py:class:: define_element_death_thick_shell_set.DefineElementDeathThickShellSet(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_ELEMENT_DEATH_THICK_SHELL_SET keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineElementDeathThickShellSet

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~sid`
            - Get or set the Element set ID
          * - :py:attr:`~time`
            - Get or set the Deletion time for elimination of the element or element set. If BOXID is nonzero, a TIME value of zero is restt to 1.0E+16.
          * - :py:attr:`~boxid`
            - Get or set the Element inside or outside of defined box are deleted depending on the value INOUT
          * - :py:attr:`~inout`
            - Get or set the Location of deleted element:
          * - :py:attr:`~idgrp`
            - Get or set the Group ID. Elements sharing the same positive value of IDGRP
          * - :py:attr:`~cid`
            - Get or set the Coordinate ID for transforming box BOXID. If CID is not
          * - :py:attr:`~percent`
            - Get or set the Deletion percentage.
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

    from define_element_death_thick_shell_set import DefineElementDeathThickShellSet

Property detail
---------------

.. py:property:: sid
   :type: Optional[int]


   
   Get or set the Element set ID
















   ..
       !! processed by numpydoc !!

.. py:property:: time
   :type: float


   
   Get or set the Deletion time for elimination of the element or element set. If BOXID is nonzero, a TIME value of zero is restt to 1.0E+16.
















   ..
       !! processed by numpydoc !!

.. py:property:: boxid
   :type: Optional[int]


   
   Get or set the Element inside or outside of defined box are deleted depending on the value INOUT
















   ..
       !! processed by numpydoc !!

.. py:property:: inout
   :type: int


   
   Get or set the Location of deleted element:
   EQ.0:Element inside box are deleted.
   EQ.1:Element outside of box are deleted
















   ..
       !! processed by numpydoc !!

.. py:property:: idgrp
   :type: int


   
   Get or set the Group ID. Elements sharing the same positive value of IDGRP
   are considered to be in the same group. All elements in a group
   will be simultaneously deleted one cycle after a percentage of the elements (specified in PERCENT) fail.
   There is no requirement that each *DEFINE_ELEMENT_DEATH
   command have a unique IDGRP. In other words, elements in a
   single group can come from multiple *DEFINE_ELEMENT_DEATH commands.
   Elements in which IDGRP = 0 are not assigned to a group and
   thus deletion of one element does not cause deletion of the other elements.
















   ..
       !! processed by numpydoc !!

.. py:property:: cid
   :type: int


   
   Get or set the Coordinate ID for transforming box BOXID. If CID is not
   specified, the box is in the global coordinate system. The box
   rotates and translates with the coordinate system only if the
   coordinate system is flagged for an update every time step
















   ..
       !! processed by numpydoc !!

.. py:property:: percent
   :type: float


   
   Get or set the Deletion percentage.
   EQ.0.0: When one element fails, all elements in the group will be deleted (default).
   GT.0.0: Percentage of elements failed before elements in group IDGRP are deleted
















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
   :value: 'ELEMENT_DEATH_THICK_SHELL_SET'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





