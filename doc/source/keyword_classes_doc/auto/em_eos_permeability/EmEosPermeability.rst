





:class:`EmEosPermeability`
==========================


.. py:class:: em_eos_permeability.EmEosPermeability(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EM_EOS_PERMEABILITY keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EmEosPermeability

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~eosid`
            - Get or set the Id of the EM_EOS.
          * - :py:attr:`~eostype`
            - Get or set the Defines the type of EOS:
          * - :py:attr:`~lcid`
            - Get or set the Load curve Id.


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

    from em_eos_permeability import EmEosPermeability

Property detail
---------------

.. py:property:: eosid
   :type: Optional[int]


   
   Get or set the Id of the EM_EOS.
















   ..
       !! processed by numpydoc !!

.. py:property:: eostype
   :type: int


   
   Get or set the Defines the type of EOS:
   EQ.1: Permeability defined by a B function of H curve (B= uH).
   EQ.2: Permeability defined by a H function of B curve (H= B/u).
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve Id.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EM'


.. py:attribute:: subkeyword
   :value: 'EOS_PERMEABILITY'






