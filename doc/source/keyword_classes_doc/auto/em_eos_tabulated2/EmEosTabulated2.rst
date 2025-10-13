





:class:`EmEosTabulated2`
========================


.. py:class:: em_eos_tabulated2.EmEosTabulated2(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EM_EOS_TABULATED2 keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EmEosTabulated2

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
            - Get or set the Load curve Id, Define Function ID, Table ID or Table 2D ID.
          * - :py:attr:`~iflag`
            - Get or set the Only used is a Table ID or a Table 2D ID is given in LCID.


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

    from em_eos_tabulated2 import EmEosTabulated2

Property detail
---------------

.. py:property:: eosid
   :type: Optional[int]


   
   Get or set the Id of the EM_EOS.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve Id, Define Function ID, Table ID or Table 2D ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: iflag
   :type: int


   
   Get or set the Only used is a Table ID or a Table 2D ID is given in LCID.
   EQ.0: Gives load curve ID function of temperature. Load curves give conductivity function of material's density.
   EQ.1: Gives load curve ID function of material's density. Load curves give conductivity function of temperature.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EM'


.. py:attribute:: subkeyword
   :value: 'EOS_TABULATED2'






