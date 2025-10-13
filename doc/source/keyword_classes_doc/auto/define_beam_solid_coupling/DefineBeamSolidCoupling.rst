





:class:`DefineBeamSolidCoupling`
================================


.. py:class:: define_beam_solid_coupling.DefineBeamSolidCoupling(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_BEAM_SOLID_COUPLING keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineBeamSolidCoupling

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~lstrid`
            - Get or set the Part set ID or part ID of the Lagrangian structure.  LSTRTYPE below indicates the ID type specified by LSTRTYPE.
          * - :py:attr:`~msolidm`
            - Get or set the Part set ID or part ID of the solid block.  SOLTYPE below indicates the ID type specified by SOLTYPE.
          * - :py:attr:`~lstrtype`
            - Get or set the Type of Lagrangian structures set:
          * - :py:attr:`~soltype`
            - Get or set the Type of solid set:
          * - :py:attr:`~form`
            - Get or set the Coupling type
          * - :py:attr:`~psf`
            - Get or set the Scale factor for penalty stiffness
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

    from define_beam_solid_coupling import DefineBeamSolidCoupling

Property detail
---------------

.. py:property:: lstrid
   :type: Optional[int]


   
   Get or set the Part set ID or part ID of the Lagrangian structure.  LSTRTYPE below indicates the ID type specified by LSTRTYPE.
















   ..
       !! processed by numpydoc !!

.. py:property:: msolidm
   :type: Optional[int]


   
   Get or set the Part set ID or part ID of the solid block.  SOLTYPE below indicates the ID type specified by SOLTYPE.
















   ..
       !! processed by numpydoc !!

.. py:property:: lstrtype
   :type: int


   
   Get or set the Type of Lagrangian structures set:
   EQ.0:   Part set
   EQ.1:   Part
















   ..
       !! processed by numpydoc !!

.. py:property:: soltype
   :type: int


   
   Get or set the Type of solid set:
   EQ.0:   Part set
   EQ.1:   Part
















   ..
       !! processed by numpydoc !!

.. py:property:: form
   :type: int


   
   Get or set the Coupling type
   EQ.0: Constrained acceleration and velocity
   EQ.1: Penalty tied in all directions
















   ..
       !! processed by numpydoc !!

.. py:property:: psf
   :type: float


   
   Get or set the Scale factor for penalty stiffness
















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
   :value: 'BEAM_SOLID_COUPLING'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





