





:class:`DefineMultiscale`
=========================


.. py:class:: define_multiscale.DefineMultiscale(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_MULTISCALE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineMultiscale

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the MULTISCALE local model ID to use.  See *INCLUDE_‌MULTISCALE.
          * - :py:attr:`~bset`
            - Get or set the Beam set which uses this multi-scale local model ID for failure modeling
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

    from define_multiscale import DefineMultiscale

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the MULTISCALE local model ID to use.  See *INCLUDE_‌MULTISCALE.
















   ..
       !! processed by numpydoc !!

.. py:property:: bset
   :type: Optional[int]


   
   Get or set the Beam set which uses this multi-scale local model ID for failure modeling
















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
   :value: 'DEFINE'


.. py:attribute:: subkeyword
   :value: 'MULTISCALE'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





