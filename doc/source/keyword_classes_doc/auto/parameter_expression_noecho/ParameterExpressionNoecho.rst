





:class:`ParameterExpressionNoecho`
==================================


.. py:class:: parameter_expression_noecho.ParameterExpressionNoecho(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA PARAMETER_EXPRESSION_NOECHO keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ParameterExpressionNoecho

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~prmr`
            - Get or set the Define the nth parameter in a field of 10.  Within this field the first character must be either an R for a real number or an I for an integer.  Lower or upper case for I or R is okay.  Following the type designation, define the name of the parameter using up to, but not exceeding seven characters.  For example, when defining a shell thickness named, SHLTHK, both inputs RSHLTHK or R   SHLTHK can be used and placed anywhere in the field of 10
          * - :py:attr:`~expression`
            - Get or set the General expression which is evaluated, having the result stored in PRMRn.  The following functions are available: sin, cos, tan, csc, sec, ctn, asin, acos, atan, atan2, sinh, cosh, tanh, asinh, acosh, atanh, min, max, sqrt, mod, abs, sign, LS_INTEGER, aint, nint, anint, LS_REAL, exp, log, log10, LS_REAL, and general arithmetic expressions involving +, -, *, /, and **.  The standard rules regarding operator precedence are obeyed, and nested parentheses are allowed. The expression can reference previously defined parameters (with or without the leading &).  The expression can be continued on multiple lines simply by leaving the first 10 characters of the continuation line blank.


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

    from parameter_expression_noecho import ParameterExpressionNoecho

Property detail
---------------

.. py:property:: prmr
   :type: Optional[str]


   
   Get or set the Define the nth parameter in a field of 10.  Within this field the first character must be either an R for a real number or an I for an integer.  Lower or upper case for I or R is okay.  Following the type designation, define the name of the parameter using up to, but not exceeding seven characters.  For example, when defining a shell thickness named, SHLTHK, both inputs RSHLTHK or R   SHLTHK can be used and placed anywhere in the field of 10
















   ..
       !! processed by numpydoc !!

.. py:property:: expression
   :type: Optional[str]


   
   Get or set the General expression which is evaluated, having the result stored in PRMRn.  The following functions are available: sin, cos, tan, csc, sec, ctn, asin, acos, atan, atan2, sinh, cosh, tanh, asinh, acosh, atanh, min, max, sqrt, mod, abs, sign, LS_INTEGER, aint, nint, anint, LS_REAL, exp, log, log10, LS_REAL, and general arithmetic expressions involving +, -, *, /, and **.  The standard rules regarding operator precedence are obeyed, and nested parentheses are allowed. The expression can reference previously defined parameters (with or without the leading &).  The expression can be continued on multiple lines simply by leaving the first 10 characters of the continuation line blank.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'PARAMETER'


.. py:attribute:: subkeyword
   :value: 'EXPRESSION_NOECHO'






