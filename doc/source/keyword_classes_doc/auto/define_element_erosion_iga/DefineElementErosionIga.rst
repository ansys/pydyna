





:class:`DefineElementErosionIga`
================================


.. py:class:: define_element_erosion_iga.DefineElementErosionIga(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_ELEMENT_EROSION_IGA keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineElementErosionIga

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~sid`
            - Get or set the part ID, or part set ID, see *PART, *SET_PART or *SET_SHELL_OPTION.
          * - :py:attr:`~styp`
            - Get or set the ID type of SID:
          * - :py:attr:`~numfip`
            - Get or set the Number of layers which must fail prior to element deletion.
          * - :py:attr:`~nifp`
            - Get or set the Number of integration points within one layer that need to fail, to indicate a failed layer.
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

    from define_element_erosion_iga import DefineElementErosionIga

Property detail
---------------

.. py:property:: sid
   :type: Optional[int]


   
   Get or set the part ID, or part set ID, see *PART, *SET_PART or *SET_SHELL_OPTION.
















   ..
       !! processed by numpydoc !!

.. py:property:: styp
   :type: int


   
   Get or set the ID type of SID:
   EQ.1:   shell element ID
   EQ.2:   shell element set ID
   EQ.3:   part ID
   EQ.4:   part set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: numfip
   :type: float


   
   Get or set the Number of layers which must fail prior to element deletion.
   LT.0.0:  is the percentage of layers which must fail prior to element deletion.
















   ..
       !! processed by numpydoc !!

.. py:property:: nifp
   :type: int


   
   Get or set the Number of integration points within one layer that need to fail, to indicate a failed layer.
















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
   :value: 'ELEMENT_EROSION_IGA'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





