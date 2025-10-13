





:class:`EmRandlesShort`
=======================


.. py:class:: em_randles_short.EmRandlesShort(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EM_RANDLES_SHORT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EmRandlesShort

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~areatype`
            - Get or set the Works the same way as RDLAREA in *EM_RANDLES_SOLID or in *EM_RANDLES_TSHELL:
          * - :py:attr:`~funcid`
            - Get or set the DEFINE_FUNCTION ID giving the local resistance function of local parameters for the local randle circuit..


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

    from em_randles_short import EmRandlesShort

Property detail
---------------

.. py:property:: areatype
   :type: int


   
   Get or set the Works the same way as RDLAREA in *EM_RANDLES_SOLID or in *EM_RANDLES_TSHELL:
   EQ.1:The resistance in FUNCTID is per unit area.
   EQ.2:Default. The resistance in FUNCTID is for the whole cell(the whole cell is shorted), and then a factor based on areaLocal/areaGlobal is applied.
   EQ.3:The resistance in FUNCTID is taken as is in each Randles circuit.
















   ..
       !! processed by numpydoc !!

.. py:property:: funcid
   :type: Optional[int]


   
   Get or set the DEFINE_FUNCTION ID giving the local resistance function of local parameters for the local randle circuit..
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EM'


.. py:attribute:: subkeyword
   :value: 'RANDLES_SHORT'






