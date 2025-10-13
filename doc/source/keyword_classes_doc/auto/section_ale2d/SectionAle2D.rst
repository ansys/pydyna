





:class:`SectionAle2D`
=====================


.. py:class:: section_ale2d.SectionAle2D(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA SECTION_ALE2D keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: SectionAle2D

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~secid`
            - Get or set the Section ID. SECID is referenced on the *PART card and must be unique.
          * - :py:attr:`~aleform`
            - Get or set the ALE formulation:
          * - :py:attr:`~aet`
            - Get or set the Ambient Element Type: can be defined for ALEFORM 7 and 11.
          * - :py:attr:`~elform`
            - Get or set the Element formulation:
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

    from section_ale2d import SectionAle2D

Property detail
---------------

.. py:property:: secid
   :type: Optional[int]


   
   Get or set the Section ID. SECID is referenced on the *PART card and must be unique.
















   ..
       !! processed by numpydoc !!

.. py:property:: aleform
   :type: int


   
   Get or set the ALE formulation:
   EQ.6  : Single material Eulerian formulation
   EQ.7  : Single material Ambient Eulerian formulation
   EQ.11: Multi-Material ALE formulation
















   ..
       !! processed by numpydoc !!

.. py:property:: aet
   :type: Optional[int]


   
   Get or set the Ambient Element Type: can be defined for ALEFORM 7 and 11.
   EQ.4: pressure inflow
   .
















   ..
       !! processed by numpydoc !!

.. py:property:: elform
   :type: int


   
   Get or set the Element formulation:
   EQ.13: plane strain (x-y plane)
   EQ.14: axisymmetric solid (y-axis of symmetry) - area weighted
















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
   :value: 'ALE2D'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





