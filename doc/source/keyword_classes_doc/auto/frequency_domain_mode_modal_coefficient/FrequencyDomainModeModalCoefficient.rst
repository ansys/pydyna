





:class:`FrequencyDomainModeModalCoefficient`
============================================


.. py:class:: frequency_domain_mode_modal_coefficient.FrequencyDomainModeModalCoefficient(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA FREQUENCY_DOMAIN_MODE_MODAL_COEFFICIENT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: FrequencyDomainModeModalCoefficient

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~dskip`
            - Get or set the The threshold modal coefficient ratio. All modes with the ratio of its modal coefficient over the largest modal coefficient less than this value will be skipped.


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

    from frequency_domain_mode_modal_coefficient import FrequencyDomainModeModalCoefficient

Property detail
---------------

.. py:property:: dskip
   :type: Optional[float]


   
   Get or set the The threshold modal coefficient ratio. All modes with the ratio of its modal coefficient over the largest modal coefficient less than this value will be skipped.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'FREQUENCY'


.. py:attribute:: subkeyword
   :value: 'DOMAIN_MODE_MODAL_COEFFICIENT'






