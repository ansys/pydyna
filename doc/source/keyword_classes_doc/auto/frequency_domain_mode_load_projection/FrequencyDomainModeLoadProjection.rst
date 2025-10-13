





:class:`FrequencyDomainModeLoadProjection`
==========================================


.. py:class:: frequency_domain_mode_load_projection.FrequencyDomainModeLoadProjection(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA FREQUENCY_DOMAIN_MODE_LOAD_PROJECTION keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: FrequencyDomainModeLoadProjection

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~nmsort`
            - Get or set the Number of modes to be retained which have the largest load projection ratios.
          * - :py:attr:`~dskip`
            - Get or set the The threshold load projection ratio. All modes with load projection ratio less than this value will be skipped.


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

    from frequency_domain_mode_load_projection import FrequencyDomainModeLoadProjection

Property detail
---------------

.. py:property:: nmsort
   :type: Optional[int]


   
   Get or set the Number of modes to be retained which have the largest load projection ratios.
















   ..
       !! processed by numpydoc !!

.. py:property:: dskip
   :type: Optional[float]


   
   Get or set the The threshold load projection ratio. All modes with load projection ratio less than this value will be skipped.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'FREQUENCY'


.. py:attribute:: subkeyword
   :value: 'DOMAIN_MODE_LOAD_PROJECTION'






