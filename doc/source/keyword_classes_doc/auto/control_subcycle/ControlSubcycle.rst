





:class:`ControlSubcycle`
========================


.. py:class:: control_subcycle.ControlSubcycle(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_SUBCYCLE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlSubcycle

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~k`
            - Get or set the Ratio between the largest and smallest time step
          * - :py:attr:`~l`
            - Get or set the The relative time step at which external forces such as contacts and loads are calculated


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

    from control_subcycle import ControlSubcycle

Property detail
---------------

.. py:property:: k
   :type: int


   
   Get or set the Ratio between the largest and smallest time step
















   ..
       !! processed by numpydoc !!

.. py:property:: l
   :type: int


   
   Get or set the The relative time step at which external forces such as contacts and loads are calculated
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'SUBCYCLE'






