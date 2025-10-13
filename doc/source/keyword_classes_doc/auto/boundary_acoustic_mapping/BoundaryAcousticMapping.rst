





:class:`BoundaryAcousticMapping`
================================


.. py:class:: boundary_acoustic_mapping.BoundaryAcousticMapping(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BOUNDARY_ACOUSTIC_MAPPING keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BoundaryAcousticMapping

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ssid`
            - Get or set the Set or part ID
          * - :py:attr:`~styp`
            - Get or set the Type of "ID" (see remark 1):


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

    from boundary_acoustic_mapping import BoundaryAcousticMapping

Property detail
---------------

.. py:property:: ssid
   :type: Optional[int]


   
   Get or set the Set or part ID
















   ..
       !! processed by numpydoc !!

.. py:property:: styp
   :type: int


   
   Get or set the Type of "ID" (see remark 1):
   EQ.0: part set ID.
   EQ.1: part ID.
   EQ.2: segment set ID.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'BOUNDARY'


.. py:attribute:: subkeyword
   :value: 'ACOUSTIC_MAPPING'






