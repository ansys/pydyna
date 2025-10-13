





:class:`EosPhaseChange`
=======================


.. py:class:: eos_phase_change.EosPhaseChange(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EOS_PHASE_CHANGE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EosPhaseChange

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~eosid`
            - Get or set the Equation of state ID, a unique number or label must be specified.
          * - :py:attr:`~rhol`
            - Get or set the Density of liquid
          * - :py:attr:`~rhov`
            - Get or set the Density of saturated vapor
          * - :py:attr:`~cl`
            - Get or set the Speed of sound of liquid.
          * - :py:attr:`~cv`
            - Get or set the Speed of sound of vapor.
          * - :py:attr:`~gamal`
            - Get or set the Gamma constant of liquid.
          * - :py:attr:`~pv`
            - Get or set the Pressure of vapor.
          * - :py:attr:`~kl`
            - Get or set the Bulk compressibility of liquid.


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

    from eos_phase_change import EosPhaseChange

Property detail
---------------

.. py:property:: eosid
   :type: Optional[int]


   
   Get or set the Equation of state ID, a unique number or label must be specified.
















   ..
       !! processed by numpydoc !!

.. py:property:: rhol
   :type: Optional[float]


   
   Get or set the Density of liquid
















   ..
       !! processed by numpydoc !!

.. py:property:: rhov
   :type: Optional[float]


   
   Get or set the Density of saturated vapor
















   ..
       !! processed by numpydoc !!

.. py:property:: cl
   :type: Optional[float]


   
   Get or set the Speed of sound of liquid.
















   ..
       !! processed by numpydoc !!

.. py:property:: cv
   :type: Optional[float]


   
   Get or set the Speed of sound of vapor.
















   ..
       !! processed by numpydoc !!

.. py:property:: gamal
   :type: Optional[float]


   
   Get or set the Gamma constant of liquid.
















   ..
       !! processed by numpydoc !!

.. py:property:: pv
   :type: Optional[float]


   
   Get or set the Pressure of vapor.
















   ..
       !! processed by numpydoc !!

.. py:property:: kl
   :type: Optional[float]


   
   Get or set the Bulk compressibility of liquid.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EOS'


.. py:attribute:: subkeyword
   :value: 'PHASE_CHANGE'






