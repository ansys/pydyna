





:class:`EmControlSwitchContact`
===============================


.. py:class:: em_control_switch_contact.EmControlSwitchContact(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EM_CONTROL_SWITCH_CONTACT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EmControlSwitchContact

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~lcid`
            - Get or set the Load Curve ID.Negative values switch the contact detection off, positive values switch it back on.
          * - :py:attr:`~ncylfem`
            - Get or set the Determines the number of cycles before FEM matrix recomputation.If defined this will overwrite the previous NCYCLFEM as long as the contact detection is turned on.


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

    from em_control_switch_contact import EmControlSwitchContact

Property detail
---------------

.. py:property:: lcid
   :type: int


   
   Get or set the Load Curve ID.Negative values switch the contact detection off, positive values switch it back on.
















   ..
       !! processed by numpydoc !!

.. py:property:: ncylfem
   :type: int


   
   Get or set the Determines the number of cycles before FEM matrix recomputation.If defined this will overwrite the previous NCYCLFEM as long as the contact detection is turned on.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EM'


.. py:attribute:: subkeyword
   :value: 'CONTROL_SWITCH_CONTACT'






