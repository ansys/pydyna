





:class:`IcfdPartVol`
====================


.. py:class:: icfd_part_vol.IcfdPartVol(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ICFD_PART_VOL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IcfdPartVol

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Part identification for vol.
          * - :py:attr:`~secid`
            - Get or set the Section identification defined in the *ICFD_SECTION section.
          * - :py:attr:`~mid`
            - Get or set the Material identification.
          * - :py:attr:`~nodes`
            - Get the table of nodes.
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

    from icfd_part_vol import IcfdPartVol

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part identification for vol.
















   ..
       !! processed by numpydoc !!

.. py:property:: secid
   :type: Optional[int]


   
   Get or set the Section identification defined in the *ICFD_SECTION section.
















   ..
       !! processed by numpydoc !!

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification.
















   ..
       !! processed by numpydoc !!

.. py:property:: nodes
   :type: pandas.DataFrame


   
   Get the table of nodes.
















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
   :value: 'ICFD'


.. py:attribute:: subkeyword
   :value: 'PART_VOL'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





