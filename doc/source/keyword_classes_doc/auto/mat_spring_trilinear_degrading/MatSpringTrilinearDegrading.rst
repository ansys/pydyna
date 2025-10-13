





:class:`MatSpringTrilinearDegrading`
====================================


.. py:class:: mat_spring_trilinear_degrading.MatSpringTrilinearDegrading(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_SPRING_TRILINEAR_DEGRADING keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatSpringTrilinearDegrading

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material number. A unique number has to be used.
          * - :py:attr:`~defl1`
            - Get or set the Deflection at point where concrete cracking occurs.
          * - :py:attr:`~f1`
            - Get or set the Force corresponding to DEFL1
          * - :py:attr:`~defl2`
            - Get or set the Deflection at point where reinforcement yields
          * - :py:attr:`~f2`
            - Get or set the Force corresponding to DEFL2
          * - :py:attr:`~defl3`
            - Get or set the Deflection at complete failure
          * - :py:attr:`~f3`
            - Get or set the Force corresponding to DEFL3
          * - :py:attr:`~fflag`
            - Get or set the Failure flag.
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

    from mat_spring_trilinear_degrading import MatSpringTrilinearDegrading

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material number. A unique number has to be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: defl1
   :type: Optional[float]


   
   Get or set the Deflection at point where concrete cracking occurs.
















   ..
       !! processed by numpydoc !!

.. py:property:: f1
   :type: Optional[float]


   
   Get or set the Force corresponding to DEFL1
















   ..
       !! processed by numpydoc !!

.. py:property:: defl2
   :type: Optional[float]


   
   Get or set the Deflection at point where reinforcement yields
















   ..
       !! processed by numpydoc !!

.. py:property:: f2
   :type: Optional[float]


   
   Get or set the Force corresponding to DEFL2
















   ..
       !! processed by numpydoc !!

.. py:property:: defl3
   :type: Optional[float]


   
   Get or set the Deflection at complete failure
















   ..
       !! processed by numpydoc !!

.. py:property:: f3
   :type: Optional[float]


   
   Get or set the Force corresponding to DEFL3
















   ..
       !! processed by numpydoc !!

.. py:property:: fflag
   :type: Optional[float]


   
   Get or set the Failure flag.
















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
   :value: 'SPRING_TRILINEAR_DEGRADING'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





