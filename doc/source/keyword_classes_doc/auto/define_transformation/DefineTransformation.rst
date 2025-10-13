





:class:`DefineTransformation`
=============================


.. py:class:: define_transformation.DefineTransformation(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_TRANSFORMATION keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineTransformation

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~tranid`
            - Get or set the Transform ID.
          * - :py:attr:`~transforms`
            - Get the table of transforms.
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

    from define_transformation import DefineTransformation

Property detail
---------------

.. py:property:: tranid
   :type: Optional[int]


   
   Get or set the Transform ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: transforms
   :type: pandas.DataFrame


   
   Get the table of transforms.
















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
   :value: 'DEFINE'


.. py:attribute:: subkeyword
   :value: 'TRANSFORMATION'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





