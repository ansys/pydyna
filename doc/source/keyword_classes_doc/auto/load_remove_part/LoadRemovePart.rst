





:class:`LoadRemovePart`
=======================


.. py:class:: load_remove_part.LoadRemovePart(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA LOAD_REMOVE_PART keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: LoadRemovePart

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Part ID for deletion.
          * - :py:attr:`~time0`
            - Get or set the Time at which stress reduction starts
          * - :py:attr:`~time1`
            - Get or set the Time at which stresses become zero and elements are deleted
          * - :py:attr:`~stgr`
            - Get or set the Construction stage at which part is removed (optional)


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

    from load_remove_part import LoadRemovePart

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID for deletion.
















   ..
       !! processed by numpydoc !!

.. py:property:: time0
   :type: Optional[float]


   
   Get or set the Time at which stress reduction starts
















   ..
       !! processed by numpydoc !!

.. py:property:: time1
   :type: Optional[float]


   
   Get or set the Time at which stresses become zero and elements are deleted
















   ..
       !! processed by numpydoc !!

.. py:property:: stgr
   :type: Optional[int]


   
   Get or set the Construction stage at which part is removed (optional)
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'LOAD'


.. py:attribute:: subkeyword
   :value: 'REMOVE_PART'






