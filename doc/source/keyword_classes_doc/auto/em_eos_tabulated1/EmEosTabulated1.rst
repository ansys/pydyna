





:class:`EmEosTabulated1`
========================


.. py:class:: em_eos_tabulated1.EmEosTabulated1(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EM_EOS_TABULATED1 keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EmEosTabulated1

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~eosid`
            - Get or set the Id of the EM_EOS.
          * - :py:attr:`~lcid`
            - Get or set the Load curve Id. The load curve describes the electrical conductivity (ordinate) vs the temperature (abscissa). The user needs to make sure the temperature and the electrical conductivity given by the load curve are in the correct units. Also, it is advised to give some bounds to the load curve (conductivities at very low and very high temperatures) to avoid bad extrapolations of the conductivity if the temperature gets out of the load curve bounds.


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

    from em_eos_tabulated1 import EmEosTabulated1

Property detail
---------------

.. py:property:: eosid
   :type: Optional[int]


   
   Get or set the Id of the EM_EOS.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve Id. The load curve describes the electrical conductivity (ordinate) vs the temperature (abscissa). The user needs to make sure the temperature and the electrical conductivity given by the load curve are in the correct units. Also, it is advised to give some bounds to the load curve (conductivities at very low and very high temperatures) to avoid bad extrapolations of the conductivity if the temperature gets out of the load curve bounds.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EM'


.. py:attribute:: subkeyword
   :value: 'EOS_TABULATED1'






