





:class:`EmEpFiberinitial`
=========================


.. py:class:: em_ep_fiberinitial.EmEpFiberinitial(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EM_EP_FIBERINITIAL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EmEpFiberinitial

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the ID of the Laplace system to solve (define new id with each new line)
          * - :py:attr:`~partid`
            - Get or set the Part id on which the system is solved
          * - :py:attr:`~stype`
            - Get or set the Segment type:
          * - :py:attr:`~ssid1`
            - Get or set the Set on which a potential of value 1 is prescribed
          * - :py:attr:`~ssid0`
            - Get or set the Set on which a potential of value 0 is prescribed


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

    from em_ep_fiberinitial import EmEpFiberinitial

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the ID of the Laplace system to solve (define new id with each new line)
















   ..
       !! processed by numpydoc !!

.. py:property:: partid
   :type: Optional[int]


   
   Get or set the Part id on which the system is solved
















   ..
       !! processed by numpydoc !!

.. py:property:: stype
   :type: int


   
   Get or set the Segment type:
   EQ.1:   node set
   EQ.2 : segment set
















   ..
       !! processed by numpydoc !!

.. py:property:: ssid1
   :type: Optional[int]


   
   Get or set the Set on which a potential of value 1 is prescribed
















   ..
       !! processed by numpydoc !!

.. py:property:: ssid0
   :type: Optional[int]


   
   Get or set the Set on which a potential of value 0 is prescribed
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EM'


.. py:attribute:: subkeyword
   :value: 'EP_FIBERINITIAL'






