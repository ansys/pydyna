





:class:`DefineFormingSolidSurface`
==================================


.. py:class:: define_forming_solid_surface.DefineFormingSolidSurface(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_FORMING_SOLID_SURFACE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineFormingSolidSurface

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~spid`
            - Get or set the Surface pair ID. A unique ID must be used.
          * - :py:attr:`~ssetlow`
            - Get or set the Segment set ID defining the lower side of solid elements
          * - :py:attr:`~ssetupp`
            - Get or set the Segment set ID defining the upper side of solid elements
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

    from define_forming_solid_surface import DefineFormingSolidSurface

Property detail
---------------

.. py:property:: spid
   :type: Optional[int]


   
   Get or set the Surface pair ID. A unique ID must be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: ssetlow
   :type: Optional[int]


   
   Get or set the Segment set ID defining the lower side of solid elements
















   ..
       !! processed by numpydoc !!

.. py:property:: ssetupp
   :type: Optional[int]


   
   Get or set the Segment set ID defining the upper side of solid elements
















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
   :value: 'FORMING_SOLID_SURFACE'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





