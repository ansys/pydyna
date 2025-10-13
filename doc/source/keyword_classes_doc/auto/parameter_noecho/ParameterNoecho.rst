





:class:`ParameterNoecho`
========================


.. py:class:: parameter_noecho.ParameterNoecho(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA PARAMETER_NOECHO keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ParameterNoecho

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~prmr1`
            - Get or set the Define the nth parameter in a field of 10, Within this field the first character must be either an 'R' for a real number or an 'I' for an integer. Lower or upper case for 'I' or 'R' is okay. Following the type designation, define the name of the parameter using up to, but not exceeding 7 characters. For example, when defining a shell thickness named, 'SHLTHK', both inputs 'RSHLTHK' or 'R  SHLTHK' can be used and placed anywhere in the field of 10. When referencing SHLTHK in the input field, place a '&' at the first column of its field followed by the name of the parameter without blanks
          * - :py:attr:`~val1`
            - Get or set the Define the numerical value of the n parameter as either a real or integer number consistent with proceeding definition for PRMRn
          * - :py:attr:`~prmr2`
            - Get or set the Define the 2nd parameter
          * - :py:attr:`~val2`
            - Get or set the Define the 2nd numerical value
          * - :py:attr:`~prmr3`
            - Get or set the Define the 3rd parameter
          * - :py:attr:`~val3`
            - Get or set the Define the 3rd numerical value
          * - :py:attr:`~prmr4`
            - Get or set the Define the 4th parameter
          * - :py:attr:`~val4`
            - Get or set the Define the 4th numerical value


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

    from parameter_noecho import ParameterNoecho

Property detail
---------------

.. py:property:: prmr1
   :type: Optional[str]


   
   Get or set the Define the nth parameter in a field of 10, Within this field the first character must be either an 'R' for a real number or an 'I' for an integer. Lower or upper case for 'I' or 'R' is okay. Following the type designation, define the name of the parameter using up to, but not exceeding 7 characters. For example, when defining a shell thickness named, 'SHLTHK', both inputs 'RSHLTHK' or 'R  SHLTHK' can be used and placed anywhere in the field of 10. When referencing SHLTHK in the input field, place a '&' at the first column of its field followed by the name of the parameter without blanks
















   ..
       !! processed by numpydoc !!

.. py:property:: val1
   :type: Optional[str]


   
   Get or set the Define the numerical value of the n parameter as either a real or integer number consistent with proceeding definition for PRMRn
















   ..
       !! processed by numpydoc !!

.. py:property:: prmr2
   :type: Optional[str]


   
   Get or set the Define the 2nd parameter
















   ..
       !! processed by numpydoc !!

.. py:property:: val2
   :type: Optional[str]


   
   Get or set the Define the 2nd numerical value
















   ..
       !! processed by numpydoc !!

.. py:property:: prmr3
   :type: Optional[str]


   
   Get or set the Define the 3rd parameter
















   ..
       !! processed by numpydoc !!

.. py:property:: val3
   :type: Optional[str]


   
   Get or set the Define the 3rd numerical value
















   ..
       !! processed by numpydoc !!

.. py:property:: prmr4
   :type: Optional[str]


   
   Get or set the Define the 4th parameter
















   ..
       !! processed by numpydoc !!

.. py:property:: val4
   :type: Optional[str]


   
   Get or set the Define the 4th numerical value
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'PARAMETER'


.. py:attribute:: subkeyword
   :value: 'NOECHO'






