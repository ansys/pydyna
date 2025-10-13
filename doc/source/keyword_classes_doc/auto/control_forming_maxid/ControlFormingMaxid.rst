





:class:`ControlFormingMaxid`
============================


.. py:class:: control_forming_maxid.ControlFormingMaxid(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_FORMING_MAXID keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlFormingMaxid

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Part ID of the sheet blank, as in *PART.
          * - :py:attr:`~maxidn`
            - Get or set the Node ID number from which adaptive node ID numbers will be created
          * - :py:attr:`~maxide`
            - Get or set the Element ID number from which adaptive element ID numbers will be created.
          * - :py:attr:`~i2dynain`
            - Get or set the Setting I2DYNAIN to 1 will cause this keyword to be output to a dynain file with the updated maximum node and element IDs. This output simplifies post-processing for multi-step processes since it ensures that element and node IDs generated in subsequent steps are larger than those in previous steps. By default, this keyword is not output to a dynain file.


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

    from control_forming_maxid import ControlFormingMaxid

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID of the sheet blank, as in *PART.
















   ..
       !! processed by numpydoc !!

.. py:property:: maxidn
   :type: Optional[int]


   
   Get or set the Node ID number from which adaptive node ID numbers will be created
















   ..
       !! processed by numpydoc !!

.. py:property:: maxide
   :type: Optional[int]


   
   Get or set the Element ID number from which adaptive element ID numbers will be created.
















   ..
       !! processed by numpydoc !!

.. py:property:: i2dynain
   :type: Optional[int]


   
   Get or set the Setting I2DYNAIN to 1 will cause this keyword to be output to a dynain file with the updated maximum node and element IDs. This output simplifies post-processing for multi-step processes since it ensures that element and node IDs generated in subsequent steps are larger than those in previous steps. By default, this keyword is not output to a dynain file.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'FORMING_MAXID'






