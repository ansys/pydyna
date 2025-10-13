





:class:`DualceseEosJwl`
=======================


.. py:class:: dualcese_eos_jwl.DualceseEosJwl(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DUALCESE_EOS_JWL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DualceseEosJwl

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~eosid`
            - Get or set the Equation of state ID for the dual CESE solver
          * - :py:attr:`~a`
            - Get or set the Model parameter (in pressure units), A
          * - :py:attr:`~b`
            - Get or set the Model parameter (in pressure units), B
          * - :py:attr:`~eps1`
            - Get or set the Model constant (dimensionless), 1
          * - :py:attr:`~eps2`
            - Get or set the Model constant (dimensionless), 2
          * - :py:attr:`~gammao`
            - Get or set the Gruneisen coefficient
          * - :py:attr:`~rhoo`
            - Get or set the Initial or reference density
          * - :py:attr:`~eo`
            - Get or set the Represents the heat of detonation released during the reactions, or the constant rate of afterburn energy added
          * - :py:attr:`~cv`
            - Get or set the Heat capacity, C_v


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

    from dualcese_eos_jwl import DualceseEosJwl

Property detail
---------------

.. py:property:: eosid
   :type: Optional[int]


   
   Get or set the Equation of state ID for the dual CESE solver
















   ..
       !! processed by numpydoc !!

.. py:property:: a
   :type: Optional[float]


   
   Get or set the Model parameter (in pressure units), A
















   ..
       !! processed by numpydoc !!

.. py:property:: b
   :type: Optional[float]


   
   Get or set the Model parameter (in pressure units), B
















   ..
       !! processed by numpydoc !!

.. py:property:: eps1
   :type: Optional[float]


   
   Get or set the Model constant (dimensionless), 1
















   ..
       !! processed by numpydoc !!

.. py:property:: eps2
   :type: Optional[float]


   
   Get or set the Model constant (dimensionless), 2
















   ..
       !! processed by numpydoc !!

.. py:property:: gammao
   :type: Optional[float]


   
   Get or set the Gruneisen coefficient
















   ..
       !! processed by numpydoc !!

.. py:property:: rhoo
   :type: Optional[float]


   
   Get or set the Initial or reference density
















   ..
       !! processed by numpydoc !!

.. py:property:: eo
   :type: Optional[float]


   
   Get or set the Represents the heat of detonation released during the reactions, or the constant rate of afterburn energy added
















   ..
       !! processed by numpydoc !!

.. py:property:: cv
   :type: Optional[float]


   
   Get or set the Heat capacity, C_v
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DUALCESE'


.. py:attribute:: subkeyword
   :value: 'EOS_JWL'






