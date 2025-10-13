





:class:`InterfaceAcoustic`
==========================


.. py:class:: interface_acoustic.InterfaceAcoustic(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA INTERFACE_ACOUSTIC keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: InterfaceAcoustic

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ifid`
            - Get or set the Segment set ID of structural faces for this interface.  This structural IFID can be used by *BOUNDARY_ACOUSTIC_INTERFACE.


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

    from interface_acoustic import InterfaceAcoustic

Property detail
---------------

.. py:property:: ifid
   :type: Optional[int]


   
   Get or set the Segment set ID of structural faces for this interface.  This structural IFID can be used by *BOUNDARY_ACOUSTIC_INTERFACE.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'INTERFACE'


.. py:attribute:: subkeyword
   :value: 'ACOUSTIC'






