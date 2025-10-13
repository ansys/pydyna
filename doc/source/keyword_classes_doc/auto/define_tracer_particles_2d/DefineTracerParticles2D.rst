





:class:`DefineTracerParticles2D`
================================


.. py:class:: define_tracer_particles_2d.DefineTracerParticles2D(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_TRACER_PARTICLES_2D keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineTracerParticles2D

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~nset`
            - Get or set the The node set ID for the nodes used as tracer particles.
          * - :py:attr:`~pset`
            - Get or set the Optional part set ID. If this part set is specified, only tracer
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

    from define_tracer_particles_2d import DefineTracerParticles2D

Property detail
---------------

.. py:property:: nset
   :type: Optional[int]


   
   Get or set the The node set ID for the nodes used as tracer particles.
















   ..
       !! processed by numpydoc !!

.. py:property:: pset
   :type: Optional[int]


   
   Get or set the Optional part set ID. If this part set is specified, only tracer
   particles in these parts are updated and the others are stationary.
   If this part set is not specified, all tracer particles are updated.
















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
   :value: 'TRACER_PARTICLES_2D'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





