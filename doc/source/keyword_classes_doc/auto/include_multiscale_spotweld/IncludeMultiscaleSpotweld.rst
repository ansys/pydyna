





:class:`IncludeMultiscaleSpotweld`
==================================


.. py:class:: include_multiscale_spotweld.IncludeMultiscaleSpotweld(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA INCLUDE_MULTISCALE_SPOTWELD keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IncludeMultiscaleSpotweld

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~type`
            - Get or set the TYPE for this multiscale spot weld. This type is used in the keyword
          * - :py:attr:`~filename`
            - Get or set the Name of file from which to read the spot weld definition


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

    from include_multiscale_spotweld import IncludeMultiscaleSpotweld

Property detail
---------------

.. py:property:: type
   :type: Optional[int]


   
   Get or set the TYPE for this multiscale spot weld. This type is used in the keyword
   *DEFINE_SPOTWELD_MULTISCALE. Any unique integer will do.
















   ..
       !! processed by numpydoc !!

.. py:property:: filename
   :type: Optional[str]


   
   Get or set the Name of file from which to read the spot weld definition
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'INCLUDE'


.. py:attribute:: subkeyword
   :value: 'MULTISCALE_SPOTWELD'






