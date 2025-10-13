





:class:`EosMurnaghan`
=====================


.. py:class:: eos_murnaghan.EosMurnaghan(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EOS_MURNAGHAN keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EosMurnaghan

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~eosid`
            - Get or set the Equation of state ID, a unique number or label must be specified.
          * - :py:attr:`~gamma`
            - Get or set the Constants in the equation of state.
          * - :py:attr:`~k0`
            - Get or set the Constants in the equation of state
          * - :py:attr:`~v0`
            - Get or set the Initial relative volume.


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

    from eos_murnaghan import EosMurnaghan

Property detail
---------------

.. py:property:: eosid
   :type: Optional[int]


   
   Get or set the Equation of state ID, a unique number or label must be specified.
















   ..
       !! processed by numpydoc !!

.. py:property:: gamma
   :type: Optional[float]


   
   Get or set the Constants in the equation of state.
















   ..
       !! processed by numpydoc !!

.. py:property:: k0
   :type: Optional[float]


   
   Get or set the Constants in the equation of state
















   ..
       !! processed by numpydoc !!

.. py:property:: v0
   :type: Optional[float]


   
   Get or set the Initial relative volume.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EOS'


.. py:attribute:: subkeyword
   :value: 'MURNAGHAN'






