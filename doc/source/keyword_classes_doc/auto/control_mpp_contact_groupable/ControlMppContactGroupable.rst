





:class:`ControlMppContactGroupable`
===================================


.. py:class:: control_mpp_contact_groupable.ControlMppContactGroupable(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_MPP_CONTACT_GROUPABLE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlMppContactGroupable

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~grp`
            - Get or set the The sum of these available options (in any combination that makes sense):


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

    from control_mpp_contact_groupable import ControlMppContactGroupable

Property detail
---------------

.. py:property:: grp
   :type: int


   
   Get or set the The sum of these available options (in any combination that makes sense):
   1: Turn on GROUPABLE for all non-tied contacts
   2: Turn on GROUPABLE for all tied contacts
   4: Turn off GROUPABLE for all non-tied contacts
   8: Turn off GROUPABLE for all tied contacts.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'MPP_CONTACT_GROUPABLE'






