





:class:`ParameterDuplication`
=============================


.. py:class:: parameter_duplication.ParameterDuplication(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA PARAMETER_DUPLICATION keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ParameterDuplication

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~dflag`
            - Get or set the Flag to control treatment of duplicate parameter definitions:


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

    from parameter_duplication import ParameterDuplication

Property detail
---------------

.. py:property:: dflag
   :type: int


   
   Get or set the Flag to control treatment of duplicate parameter definitions:
   EQ.1: issue a warning and ignore the new definition (default)
   EQ.2: issue a warning and accept the new definition
   EQ.3: issue an error and ignore (terminates at end of input)
   EQ.4: accept silently
   EQ.5: ignore silently
   .
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'PARAMETER'


.. py:attribute:: subkeyword
   :value: 'DUPLICATION'






