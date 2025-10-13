





:class:`EosMieGruneisen`
========================


.. py:class:: eos_mie_gruneisen.EosMieGruneisen(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EOS_MIE_GRUNEISEN keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EosMieGruneisen

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
            - Get or set the Gruneisen gamma.
          * - :py:attr:`~a1`
            - Get or set the Hugoniot polynomial coefficient
          * - :py:attr:`~a2`
            - Get or set the Hugoniot polynomial coefficient.
          * - :py:attr:`~a3`
            - Get or set the Hugoniot polynomial coefficient.
          * - :py:attr:`~pel`
            - Get or set the Crush pressure.
          * - :py:attr:`~pco`
            - Get or set the Compaction pressure.
          * - :py:attr:`~n`
            - Get or set the Porosity exponent.
          * - :py:attr:`~alpha0`
            - Get or set the Initial porosity.
          * - :py:attr:`~e0`
            - Get or set the Initial internal energy.
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

    from eos_mie_gruneisen import EosMieGruneisen

Property detail
---------------

.. py:property:: eosid
   :type: Optional[int]


   
   Get or set the Equation of state ID, a unique number or label must be specified.
















   ..
       !! processed by numpydoc !!

.. py:property:: gamma
   :type: Optional[float]


   
   Get or set the Gruneisen gamma.
















   ..
       !! processed by numpydoc !!

.. py:property:: a1
   :type: Optional[float]


   
   Get or set the Hugoniot polynomial coefficient
















   ..
       !! processed by numpydoc !!

.. py:property:: a2
   :type: Optional[float]


   
   Get or set the Hugoniot polynomial coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: a3
   :type: Optional[float]


   
   Get or set the Hugoniot polynomial coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: pel
   :type: Optional[float]


   
   Get or set the Crush pressure.
















   ..
       !! processed by numpydoc !!

.. py:property:: pco
   :type: Optional[float]


   
   Get or set the Compaction pressure.
















   ..
       !! processed by numpydoc !!

.. py:property:: n
   :type: Optional[float]


   
   Get or set the Porosity exponent.
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha0
   :type: Optional[float]


   
   Get or set the Initial porosity.
















   ..
       !! processed by numpydoc !!

.. py:property:: e0
   :type: Optional[float]


   
   Get or set the Initial internal energy.
















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
   :value: 'MIE_GRUNEISEN'






