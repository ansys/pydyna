





:class:`ControlFrequencyDomain`
===============================


.. py:class:: control_frequency_domain.ControlFrequencyDomain(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_FREQUENCY_DOMAIN keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlFrequencyDomain

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~refgeo`
            - Get or set the Flag for reference geometry in acoustic eigenvalue analysis:
          * - :py:attr:`~mpn`
            - Get or set the Large mass added per node, to be used in large mass method for enforced motion.


   .. tab-item:: Attributes

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~keyword`
            - 
          * - :py:attr:`~subkeyword`
            - 






Import detail
-------------

.. code-block:: python

    from control_frequency_domain import ControlFrequencyDomain

Property detail
---------------

.. py:property:: refgeo
   :type: int


   
   Get or set the Flag for reference geometry in acoustic eigenvalue analysis:
   EQ.0:   use original geometry (t = 0),
   EQ.1:   use deformed geometry at the end of transient analysis.
















   ..
       !! processed by numpydoc !!

.. py:property:: mpn
   :type: float


   
   Get or set the Large mass added per node, to be used in large mass method for enforced motion.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'FREQUENCY_DOMAIN'






