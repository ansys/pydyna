





:class:`DefineFunctionTabulated`
================================


.. py:class:: define_function_tabulated.DefineFunctionTabulated(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_FUNCTION_TABULATED keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineFunctionTabulated

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~fid`
            - Get or set the Function ID.  Functions, tables (see *DEFINE_TABLE), and load curves may not share common ID's.  A unique number has to be defined.
          * - :py:attr:`~heading`
            - Get or set the An optional descriptive heading.
          * - :py:attr:`~function`
            - Get or set the Arithmetic expression involving a combination of independent variables and other functions, i.e., f(a,b,c)=a*2+b*c+sqrt(a*c) where a, b, and c are the independent variables.  The function name, f(a,b,c), must be unique since other functions can then use and reference this function.  For example, g(a,b,c,d)=f(a,b,c)**2+d.  In this example, two *DEFINE_ FUNCTION definitions are needed to define functions f and g.
          * - :py:attr:`~a1`
            - Get or set the Abscissa values.  Only pairs have to be defined, see remarks below.
          * - :py:attr:`~o1`
            - Get or set the Ordinate (function) values.  Only pairs have to be defined, see remarks below.
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

    from define_function_tabulated import DefineFunctionTabulated

Property detail
---------------

.. py:property:: fid
   :type: Optional[int]


   
   Get or set the Function ID.  Functions, tables (see *DEFINE_TABLE), and load curves may not share common ID's.  A unique number has to be defined.
















   ..
       !! processed by numpydoc !!

.. py:property:: heading
   :type: Optional[str]


   
   Get or set the An optional descriptive heading.
















   ..
       !! processed by numpydoc !!

.. py:property:: function
   :type: Optional[str]


   
   Get or set the Arithmetic expression involving a combination of independent variables and other functions, i.e., f(a,b,c)=a*2+b*c+sqrt(a*c) where a, b, and c are the independent variables.  The function name, f(a,b,c), must be unique since other functions can then use and reference this function.  For example, g(a,b,c,d)=f(a,b,c)**2+d.  In this example, two *DEFINE_ FUNCTION definitions are needed to define functions f and g.
















   ..
       !! processed by numpydoc !!

.. py:property:: a1
   :type: Optional[float]


   
   Get or set the Abscissa values.  Only pairs have to be defined, see remarks below.
















   ..
       !! processed by numpydoc !!

.. py:property:: o1
   :type: Optional[float]


   
   Get or set the Ordinate (function) values.  Only pairs have to be defined, see remarks below.
















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
   :value: 'FUNCTION_TABULATED'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





