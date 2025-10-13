





:class:`DefineSetAdaptive`
==========================


.. py:class:: define_set_adaptive.DefineSetAdaptive(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_SET_ADAPTIVE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineSetAdaptive

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~setid`
            - Get or set the Element(shell) set ID or part set ID
          * - :py:attr:`~stype`
            - Get or set the Set type for SETID:   1-element set(shell)   2-part set
          * - :py:attr:`~adplvl`
            - Get or set the Adaptive refinement level for all elements in SETID set.
          * - :py:attr:`~adpsize`
            - Get or set the Minimum element size to be adapted based on element edge length for all elements in SETID set.
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

    from define_set_adaptive import DefineSetAdaptive

Property detail
---------------

.. py:property:: setid
   :type: Optional[int]


   
   Get or set the Element(shell) set ID or part set ID
















   ..
       !! processed by numpydoc !!

.. py:property:: stype
   :type: int


   
   Get or set the Set type for SETID:   1-element set(shell)   2-part set
















   ..
       !! processed by numpydoc !!

.. py:property:: adplvl
   :type: Optional[int]


   
   Get or set the Adaptive refinement level for all elements in SETID set.
















   ..
       !! processed by numpydoc !!

.. py:property:: adpsize
   :type: Optional[float]


   
   Get or set the Minimum element size to be adapted based on element edge length for all elements in SETID set.
















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
   :value: 'SET_ADAPTIVE'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





