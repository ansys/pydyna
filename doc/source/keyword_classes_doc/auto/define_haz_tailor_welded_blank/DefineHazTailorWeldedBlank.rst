





:class:`DefineHazTailorWeldedBlank`
===================================


.. py:class:: define_haz_tailor_welded_blank.DefineHazTailorWeldedBlank(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_HAZ_TAILOR_WELDED_BLANK keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineHazTailorWeldedBlank

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~idtwb`
            - Get or set the Tailor Welded Blank ID.
          * - :py:attr:`~idns`
            - Get or set the Node Set ID defining the location of the line weld.
          * - :py:attr:`~idp`
            - Get or set the Part or part set ID. Applies to all HAZ parts if IDP = 0 (default).
          * - :py:attr:`~ipflag`
            - Get or set the IDP type:
          * - :py:attr:`~imonflag`
            - Get or set the Monotonicity flag for load curves ISW and IFW on *DEFINE_HAZ_PROPERTIES:
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

    from define_haz_tailor_welded_blank import DefineHazTailorWeldedBlank

Property detail
---------------

.. py:property:: idtwb
   :type: int


   
   Get or set the Tailor Welded Blank ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: idns
   :type: int


   
   Get or set the Node Set ID defining the location of the line weld.
















   ..
       !! processed by numpydoc !!

.. py:property:: idp
   :type: int


   
   Get or set the Part or part set ID. Applies to all HAZ parts if IDP = 0 (default).
















   ..
       !! processed by numpydoc !!

.. py:property:: ipflag
   :type: int


   
   Get or set the IDP type:
   EQ.0:   part ID(default)
   EQ.1 : part set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: imonflag
   :type: int


   
   Get or set the Monotonicity flag for load curves ISW and IFW on *DEFINE_HAZ_PROPERTIES:
   EQ.0:   ISW and IFW increase monotonically.
   EQ.1 : ISW and IFW are allowed to be arbitrary load curves.
















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
   :value: 'HAZ_TAILOR_WELDED_BLANK'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





