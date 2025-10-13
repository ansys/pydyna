





:class:`ElementSph`
===================


.. py:class:: element_sph.ElementSph(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ELEMENT_SPH keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ElementSph

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~nid`
            - Get or set the Node ID and Element ID are the same for the SPH option.
          * - :py:attr:`~pid`
            - Get or set the Part ID to which this node (element) belongs.
          * - :py:attr:`~mass`
            - Get or set the Mass or volume of SPH element.


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

    from element_sph import ElementSph

Property detail
---------------

.. py:property:: nid
   :type: Optional[int]


   
   Get or set the Node ID and Element ID are the same for the SPH option.
















   ..
       !! processed by numpydoc !!

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID to which this node (element) belongs.
















   ..
       !! processed by numpydoc !!

.. py:property:: mass
   :type: float


   
   Get or set the Mass or volume of SPH element.
   GT.0: Mass value.
   LT.0: Volume. The absolute value will be used as volume. The density(rho) will be retrieved from the material card defined in PID. SPH element mass is calculated by abs(MASS)*rho
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ELEMENT'


.. py:attribute:: subkeyword
   :value: 'SPH'






