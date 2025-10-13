





:class:`PartModes`
==================


.. py:class:: part_modes.PartModes(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA PART_MODES keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: PartModes

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Part identification. This part must be a rigid body.
          * - :py:attr:`~nmfb`
            - Get or set the Number of kept modes in flexible body.
          * - :py:attr:`~form`
            - Get or set the Flexible body formulation:
          * - :py:attr:`~ansid`
            - Get or set the Attachment node set ID (optional).
          * - :py:attr:`~format`
            - Get or set the Input format of modal information:
          * - :py:attr:`~kmflag`
            - Get or set the Kept mode flag. Selects method for identifying modes to keep:
          * - :py:attr:`~nupdf`
            - Get or set the Nodal update flag.
          * - :py:attr:`~sigrec`
            - Get or set the Stree recovery flag. If active, attachment nodes should not be used.
          * - :py:attr:`~filename`
            - Get or set the The path and name of a file which contains the modes for this rigid body.
          * - :py:attr:`~mode1`
            - Get or set the Keep normal mode, MODEi.
          * - :py:attr:`~mode2`
            - Get or set the Keep normal mode, MODEi.
          * - :py:attr:`~mode3`
            - Get or set the Keep normal mode, MODEi.
          * - :py:attr:`~mode4`
            - Get or set the Keep normal mode, MODEi.
          * - :py:attr:`~mode5`
            - Get or set the Keep normal mode, MODEi.
          * - :py:attr:`~mode6`
            - Get or set the Keep normal mode, MODEi.
          * - :py:attr:`~mode7`
            - Get or set the Keep normal mode, MODEi.
          * - :py:attr:`~mode8`
            - Get or set the Keep normal mode, MODEi.
          * - :py:attr:`~mstart`
            - Get or set the First mode for damping, (1 <= MSTART <= NMFB) .
          * - :py:attr:`~mstop`
            - Get or set the Last mode for damping, MSTOP, (1 <= MSTOP <= NMFB) . All modes between MSTART and MSTOP inclusive are subject to the same modal damping coefficient, DAMPF.
          * - :py:attr:`~dampf`
            - Get or set the Modal damping coefficient.


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

    from part_modes import PartModes

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part identification. This part must be a rigid body.
















   ..
       !! processed by numpydoc !!

.. py:property:: nmfb
   :type: Optional[int]


   
   Get or set the Number of kept modes in flexible body.
   The number of modes in the file, FILENAME, must equal or exceed NMFB. If KMFLAG=0 the first NMFB modes in the file are used.
















   ..
       !! processed by numpydoc !!

.. py:property:: form
   :type: int


   
   Get or set the Flexible body formulation:
   EQ.0: exact (default),
   EQ.1: fast.
















   ..
       !! processed by numpydoc !!

.. py:property:: ansid
   :type: Optional[int]


   
   Get or set the Attachment node set ID (optional).
















   ..
       !! processed by numpydoc !!

.. py:property:: format
   :type: int


   
   Get or set the Input format of modal information:
   EQ.0:  NASTRAN.pch file.
   EQ.1:  (not supported)
   EQ.2:  NASTRAN.pch file (LS-DYNA binary version).  The binary version of this file is automatically created if a NASTRAN.pch file is read.  The name of the binary file is the name of the NASTRAN.pch file but with ".bin" appended.  The binary file is smaller and can be read much faster.
   EQ.3:  LS-DYNA d3eigv binary eigenvalue database (see *CONTROL_IMPLICIT_EIGENVALUE).
   EQ.4:  LS-DYNA d3mode binary constraint/attachment mode database (see *CONTROL_IMPLICIT_MODE).
   EQ.5:  Both d3eigv and d3mode databases are input.  Database names must be "d3eigv" and "d3mode", and FILENAME below is ignored.  NMFB above gives the total number of modes in both databases.
















   ..
       !! processed by numpydoc !!

.. py:property:: kmflag
   :type: int


   
   Get or set the Kept mode flag. Selects method for identifying modes to keep:
   EQ.0: the first NMFB modes in the file, FILENAME, are used (default),
   EQ.1: define NMFB kept modes with additional input.
















   ..
       !! processed by numpydoc !!

.. py:property:: nupdf
   :type: int


   
   Get or set the Nodal update flag.
   If active, an attachment node set, ANSID, must be defined.
   EQ.0: all nodes of the rigid part are updated each cycle (default),
   EQ.1: only attachment nodes are fully updated. All nodes in the body are output based on the rigid body motion without the addition of the modal displacements. For maximum benefit an attachment node set can also be defined with the *PART_ATTACHMENT_NODES option. The same attachment node set ID should be used here.
















   ..
       !! processed by numpydoc !!

.. py:property:: sigrec
   :type: int


   
   Get or set the Stree recovery flag. If active, attachment nodes should not be used.
   EQ.0: no stress recovery
   EQ.1: recover stresses.
















   ..
       !! processed by numpydoc !!

.. py:property:: filename
   :type: Optional[str]


   
   Get or set the The path and name of a file which contains the modes for this rigid body.
   Maximum 80 characters.
















   ..
       !! processed by numpydoc !!

.. py:property:: mode1
   :type: Optional[int]


   
   Get or set the Keep normal mode, MODEi.
















   ..
       !! processed by numpydoc !!

.. py:property:: mode2
   :type: Optional[int]


   
   Get or set the Keep normal mode, MODEi.
















   ..
       !! processed by numpydoc !!

.. py:property:: mode3
   :type: Optional[int]


   
   Get or set the Keep normal mode, MODEi.
















   ..
       !! processed by numpydoc !!

.. py:property:: mode4
   :type: Optional[int]


   
   Get or set the Keep normal mode, MODEi.
















   ..
       !! processed by numpydoc !!

.. py:property:: mode5
   :type: Optional[int]


   
   Get or set the Keep normal mode, MODEi.
















   ..
       !! processed by numpydoc !!

.. py:property:: mode6
   :type: Optional[int]


   
   Get or set the Keep normal mode, MODEi.
















   ..
       !! processed by numpydoc !!

.. py:property:: mode7
   :type: Optional[int]


   
   Get or set the Keep normal mode, MODEi.
















   ..
       !! processed by numpydoc !!

.. py:property:: mode8
   :type: Optional[int]


   
   Get or set the Keep normal mode, MODEi.
















   ..
       !! processed by numpydoc !!

.. py:property:: mstart
   :type: Optional[int]


   
   Get or set the First mode for damping, (1 <= MSTART <= NMFB) .
















   ..
       !! processed by numpydoc !!

.. py:property:: mstop
   :type: Optional[int]


   
   Get or set the Last mode for damping, MSTOP, (1 <= MSTOP <= NMFB) . All modes between MSTART and MSTOP inclusive are subject to the same modal damping coefficient, DAMPF.
















   ..
       !! processed by numpydoc !!

.. py:property:: dampf
   :type: Optional[float]


   
   Get or set the Modal damping coefficient.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'PART'


.. py:attribute:: subkeyword
   :value: 'MODES'






