





:class:`DefineSphAmbientDrag`
=============================


.. py:class:: define_sph_ambient_drag.DefineSphAmbientDrag(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_SPH_AMBIENT_DRAG keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineSphAmbientDrag

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~icid`
            - Get or set the Coupling with ICFD:
          * - :py:attr:`~vx`
            - Get or set the X-velocity of the inject elements
          * - :py:attr:`~vy`
            - Get or set the Y-velocity of the inject elements
          * - :py:attr:`~vz`
            - Get or set the Z-velocity of the inject elements
          * - :py:attr:`~rhoa`
            - Get or set the Density of the ambient material
          * - :py:attr:`~mua`
            - Get or set the Viscosity of the ambient material
          * - :py:attr:`~sftens`
            - Get or set the Surface tension coefficient for the interface between the SPH fluid and ambient materials
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

    from define_sph_ambient_drag import DefineSphAmbientDrag

Property detail
---------------

.. py:property:: icid
   :type: int


   
   Get or set the Coupling with ICFD:
   EQ.0: No coupling
















   ..
       !! processed by numpydoc !!

.. py:property:: vx
   :type: float


   
   Get or set the X-velocity of the inject elements
















   ..
       !! processed by numpydoc !!

.. py:property:: vy
   :type: float


   
   Get or set the Y-velocity of the inject elements
















   ..
       !! processed by numpydoc !!

.. py:property:: vz
   :type: float


   
   Get or set the Z-velocity of the inject elements
















   ..
       !! processed by numpydoc !!

.. py:property:: rhoa
   :type: Optional[float]


   
   Get or set the Density of the ambient material
















   ..
       !! processed by numpydoc !!

.. py:property:: mua
   :type: Optional[float]


   
   Get or set the Viscosity of the ambient material
















   ..
       !! processed by numpydoc !!

.. py:property:: sftens
   :type: Optional[float]


   
   Get or set the Surface tension coefficient for the interface between the SPH fluid and ambient materials
















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
   :value: 'SPH_AMBIENT_DRAG'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





