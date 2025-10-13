





:class:`MatS03`
===============


.. py:class:: mat_s03.MatS03(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_S03 keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatS03

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material number. A unique number has to be used.
          * - :py:attr:`~k`
            - Get or set the Elastic stiffness (force/displacement) or (moment/rotation).
          * - :py:attr:`~kt`
            - Get or set the Tangent stiffness (force/displacement) or (moment/rotation).
          * - :py:attr:`~fy`
            - Get or set the Yield (force) or (moment).
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

    from mat_s03 import MatS03

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material number. A unique number has to be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: k
   :type: Optional[float]


   
   Get or set the Elastic stiffness (force/displacement) or (moment/rotation).
















   ..
       !! processed by numpydoc !!

.. py:property:: kt
   :type: Optional[float]


   
   Get or set the Tangent stiffness (force/displacement) or (moment/rotation).
















   ..
       !! processed by numpydoc !!

.. py:property:: fy
   :type: Optional[float]


   
   Get or set the Yield (force) or (moment).
















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
   :value: 'S03'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





