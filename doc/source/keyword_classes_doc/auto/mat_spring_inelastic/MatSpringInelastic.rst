





:class:`MatSpringInelastic`
===========================


.. py:class:: mat_spring_inelastic.MatSpringInelastic(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_SPRING_INELASTIC keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatSpringInelastic

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification. A uniques number has to be used.
          * - :py:attr:`~lcfd`
            - Get or set the Load curve identification describing arbitrary force/torque versus displacement/twist relationship. This curve must be defined in the positive force-displacement quadrant regardless of whether the spring acts in tension or compression.
          * - :py:attr:`~ku`
            - Get or set the Unloading stiffness (optional). The maximum of KU and the maximum loading stiffness in the force/displacement or the moment/twist curve is used for unloading.
          * - :py:attr:`~ctf`
            - Get or set the Flag for compression/tension:
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

    from mat_spring_inelastic import MatSpringInelastic

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification. A uniques number has to be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcfd
   :type: Optional[int]


   
   Get or set the Load curve identification describing arbitrary force/torque versus displacement/twist relationship. This curve must be defined in the positive force-displacement quadrant regardless of whether the spring acts in tension or compression.
















   ..
       !! processed by numpydoc !!

.. py:property:: ku
   :type: Optional[float]


   
   Get or set the Unloading stiffness (optional). The maximum of KU and the maximum loading stiffness in the force/displacement or the moment/twist curve is used for unloading.
















   ..
       !! processed by numpydoc !!

.. py:property:: ctf
   :type: float


   
   Get or set the Flag for compression/tension:
   EQ.-1.0: tension only,
   EQ.0.0: default is set to 1.0,
   EQ.1.0: compression only (default).
















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
   :value: 'MAT'


.. py:attribute:: subkeyword
   :value: 'SPRING_INELASTIC'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





