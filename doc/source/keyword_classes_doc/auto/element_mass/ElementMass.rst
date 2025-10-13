





:class:`ElementMass`
====================


.. py:class:: element_mass.ElementMass(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ELEMENT_MASS keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ElementMass

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~eid`
            - Get or set the Element ID. A unique number must be used.
          * - :py:attr:`~nid`
            - Get or set the Node ID. Node to which the mass is assigned.
          * - :py:attr:`~mass`
            - Get or set the Mass value.
          * - :py:attr:`~pid`
            - Get or set the Part ID. This input is optional.


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

    from element_mass import ElementMass

Property detail
---------------

.. py:property:: eid
   :type: Optional[int]


   
   Get or set the Element ID. A unique number must be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: nid
   :type: Optional[int]


   
   Get or set the Node ID. Node to which the mass is assigned.
















   ..
       !! processed by numpydoc !!

.. py:property:: mass
   :type: float


   
   Get or set the Mass value.
















   ..
       !! processed by numpydoc !!

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID. This input is optional.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ELEMENT'


.. py:attribute:: subkeyword
   :value: 'MASS'






