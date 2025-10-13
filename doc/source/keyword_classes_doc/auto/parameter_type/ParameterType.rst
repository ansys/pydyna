





:class:`ParameterType`
======================


.. py:class:: parameter_type.ParameterType(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA PARAMETER_TYPE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ParameterType

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~prmr`
            - Get or set the Define the nth parameter in a field of 10.
          * - :py:attr:`~val`
            - Get or set the Define the numerical value of the n parameter as either a real or integer number consistent with preceding definition for PRMRn
          * - :py:attr:`~prtyp`
            - Get or set the The TYPE references the entity allowing for improved LSPrepost/HM renumbering


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

    from parameter_type import ParameterType

Property detail
---------------

.. py:property:: prmr
   :type: Optional[str]


   
   Get or set the Define the nth parameter in a field of 10.
















   ..
       !! processed by numpydoc !!

.. py:property:: val
   :type: Optional[int]


   
   Get or set the Define the numerical value of the n parameter as either a real or integer number consistent with preceding definition for PRMRn
















   ..
       !! processed by numpydoc !!

.. py:property:: prtyp
   :type: str


   
   Get or set the The TYPE references the entity allowing for improved LSPrepost/HM renumbering
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'PARAMETER'


.. py:attribute:: subkeyword
   :value: 'TYPE'






