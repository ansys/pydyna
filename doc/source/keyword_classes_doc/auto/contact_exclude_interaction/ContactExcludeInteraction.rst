





:class:`ContactExcludeInteraction`
==================================


.. py:class:: contact_exclude_interaction.ContactExcludeInteraction(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTACT_EXCLUDE_INTERACTION keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ContactExcludeInteraction

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ceid`
            - Get or set the Contact exclusion ID for output only
          * - :py:attr:`~cid`
            - Get or set the Contact interface ID for limiting exclusions to an interface
          * - :py:attr:`~sid2`
            - Get or set the Set ID of set 2
          * - :py:attr:`~sid1`
            - Get or set the Set ID of set 1
          * - :py:attr:`~type2`
            - Get or set the ID type of set SID2:
          * - :py:attr:`~type1`
            - Get or set the ID type of set SID1:


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

    from contact_exclude_interaction import ContactExcludeInteraction

Property detail
---------------

.. py:property:: ceid
   :type: Optional[int]


   
   Get or set the Contact exclusion ID for output only
















   ..
       !! processed by numpydoc !!

.. py:property:: cid
   :type: Optional[int]


   
   Get or set the Contact interface ID for limiting exclusions to an interface
















   ..
       !! processed by numpydoc !!

.. py:property:: sid2
   :type: Optional[int]


   
   Get or set the Set ID of set 2
















   ..
       !! processed by numpydoc !!

.. py:property:: sid1
   :type: Optional[int]


   
   Get or set the Set ID of set 1
















   ..
       !! processed by numpydoc !!

.. py:property:: type2
   :type: int


   
   Get or set the ID type of set SID2:
   EQ.0:   Segment set
   EQ.1 : Shell element set
   EQ.2 : Part set
















   ..
       !! processed by numpydoc !!

.. py:property:: type1
   :type: int


   
   Get or set the ID type of set SID1:
   EQ.0:   Segment set
   EQ.1 : Shell element set
   EQ.2 : Part set
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTACT'


.. py:attribute:: subkeyword
   :value: 'EXCLUDE_INTERACTION'






