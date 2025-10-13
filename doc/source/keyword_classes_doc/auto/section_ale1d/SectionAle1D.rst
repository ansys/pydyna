





:class:`SectionAle1D`
=====================


.. py:class:: section_ale1d.SectionAle1D(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA SECTION_ALE1D keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: SectionAle1D

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
            - Get or set the Ambient Element Type:
          * - :py:attr:`~elform`
            - Get or set the Element formulation:
          * - :py:attr:`~thick`
            - Get or set the Thickness
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

    from section_ale1d import SectionAle1D

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
   :type: int


   
   Get or set the Ambient Element Type:
   EQ.4: pressure inflow
   .
















   ..
       !! processed by numpydoc !!

.. py:property:: elform
   :type: int


   
   Get or set the Element formulation:
   EQ.7:plane strain (x-y plane ; element volume= 1*dx*thick)
   EQ.8: : cylindric  (y-axis of symmetry ; element volume= x*dx*thick)
   EQ.-8: spherical (element volume = x*x*dx)
















   ..
       !! processed by numpydoc !!

.. py:property:: thick
   :type: Optional[float]


   
   Get or set the Thickness
















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
   :value: 'ALE1D'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





