





:class:`SectionSpringDamper`
============================


.. py:class:: section_spring_damper.SectionSpringDamper(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA SECTION_SPRING_DAMPER keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: SectionSpringDamper

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~secid`
            - Get or set the Section ID. SECID is referenced on the *PART card and must be unique.
          * - :py:attr:`~dro`
            - Get or set the Displacement/Rotation Option:
          * - :py:attr:`~kd`
            - Get or set the Dynamic magnification factor.
          * - :py:attr:`~v0`
            - Get or set the Test velocity.
          * - :py:attr:`~cl`
            - Get or set the Clearance.
          * - :py:attr:`~fd`
            - Get or set the Failure deflection (twist for DRO=1).
          * - :py:attr:`~cdl`
            - Get or set the Deflection (twist for DRO=1)limit in compression.
          * - :py:attr:`~tdl`
            - Get or set the Deflection (twist for DRO=1)limit in tension.
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

    from section_spring_damper import SectionSpringDamper

Property detail
---------------

.. py:property:: secid
   :type: Optional[int]


   
   Get or set the Section ID. SECID is referenced on the *PART card and must be unique.
















   ..
       !! processed by numpydoc !!

.. py:property:: dro
   :type: int


   
   Get or set the Displacement/Rotation Option:
   EQ.0: the material describes a translational spring/damper,
   EQ.1: the material describes a torsional spring/damper.
















   ..
       !! processed by numpydoc !!

.. py:property:: kd
   :type: float


   
   Get or set the Dynamic magnification factor.
















   ..
       !! processed by numpydoc !!

.. py:property:: v0
   :type: float


   
   Get or set the Test velocity.
















   ..
       !! processed by numpydoc !!

.. py:property:: cl
   :type: float


   
   Get or set the Clearance.
















   ..
       !! processed by numpydoc !!

.. py:property:: fd
   :type: float


   
   Get or set the Failure deflection (twist for DRO=1).
















   ..
       !! processed by numpydoc !!

.. py:property:: cdl
   :type: Optional[int]


   
   Get or set the Deflection (twist for DRO=1)limit in compression.
















   ..
       !! processed by numpydoc !!

.. py:property:: tdl
   :type: Optional[int]


   
   Get or set the Deflection (twist for DRO=1)limit in tension.
















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
   :value: 'SPRING_DAMPER'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





