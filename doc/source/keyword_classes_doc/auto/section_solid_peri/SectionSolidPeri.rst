





:class:`SectionSolidPeri`
=========================


.. py:class:: section_solid_peri.SectionSolidPeri(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA SECTION_SOLID_PERI keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: SectionSolidPeri

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~secid`
            - Get or set the Section ID. SECID is referenced on the *PART card and must be unique.
          * - :py:attr:`~elform`
            - Get or set the Element formulation:
          * - :py:attr:`~dr`
            - Get or set the the ration of radius of the support zone over the element characteristic length.Normally set DR between: 0.8~1.2.
          * - :py:attr:`~ptype`
            - Get or set the 0: bond based peridynamic (Default)
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

    from section_solid_peri import SectionSolidPeri

Property detail
---------------

.. py:property:: secid
   :type: Optional[int]


   
   Get or set the Section ID. SECID is referenced on the *PART card and must be unique.
















   ..
       !! processed by numpydoc !!

.. py:property:: elform
   :type: int


   
   Get or set the Element formulation:
   mut be set as 48.
















   ..
       !! processed by numpydoc !!

.. py:property:: dr
   :type: float


   
   Get or set the the ration of radius of the support zone over the element characteristic length.Normally set DR between: 0.8~1.2.
















   ..
       !! processed by numpydoc !!

.. py:property:: ptype
   :type: int


   
   Get or set the 0: bond based peridynamic (Default)
   .1: state based peridynamic (Future using, not support now.).
















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
   :value: 'SECTION'


.. py:attribute:: subkeyword
   :value: 'SOLID_PERI'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





