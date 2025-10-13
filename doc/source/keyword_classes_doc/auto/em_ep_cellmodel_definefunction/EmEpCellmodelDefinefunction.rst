





:class:`EmEpCellmodelDefinefunction`
====================================


.. py:class:: em_ep_cellmodel_definefunction.EmEpCellmodelDefinefunction(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EM_EP_CELLMODEL_DEFINEFUNCTION keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EmEpCellmodelDefinefunction

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material ID defined in *MAT_.
          * - :py:attr:`~nstate`
            - Get or set the Number of state variables u1,u2,...un.The maximum value is 7.
          * - :py:attr:`~fswitch`
            - Get or set the Switch for the ODE definition (see Remark 1):
          * - :py:attr:`~dvdt`
            - Get or set the Function ID (see *DEFINE_FUNCTION) for evolution of V (function g in the equations).
          * - :py:attr:`~du1dt`
            - Get or set the Function ID (see *DEFINE_FUNCTION) for evolution of u_i (function f_i in the equations)
          * - :py:attr:`~du2dt`
            - Get or set the Function ID (see *DEFINE_FUNCTION) for evolution of u_i (function f_i in the equations).
          * - :py:attr:`~du3dt`
            - Get or set the Function ID (see *DEFINE_FUNCTION) for evolution of u_i (function f_i in the equations).
          * - :py:attr:`~du4dt`
            - Get or set the Function ID (see *DEFINE_FUNCTION) for evolution of u_i (function f_i in the equations).
          * - :py:attr:`~du5dt`
            - Get or set the Function ID (see *DEFINE_FUNCTION) for evolution of u_i (function f_i in the equations).
          * - :py:attr:`~du6dt`
            - Get or set the Function ID (see *DEFINE_FUNCTION) for evolution of u_i (function f_i in the equations).
          * - :py:attr:`~du7dt`
            - Get or set the Function ID (see *DEFINE_FUNCTION) for evolution of u_i (function f_i in the equations).
          * - :py:attr:`~v0`
            - Get or set the Define Function ID for initial values of V.
          * - :py:attr:`~u1`
            - Get or set the Define Function ID for initial values of u.
          * - :py:attr:`~u2`
            - Get or set the Define Function ID for initial values of u.
          * - :py:attr:`~u3`
            - Get or set the Define Function ID for initial values of u.
          * - :py:attr:`~u4`
            - Get or set the Define Function ID for initial values of u.
          * - :py:attr:`~u5`
            - Get or set the Define Function ID for initial values of u.
          * - :py:attr:`~u6`
            - Get or set the Define Function ID for initial values of u.
          * - :py:attr:`~u7`
            - Get or set the Define Function ID for initial values of u.


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

    from em_ep_cellmodel_definefunction import EmEpCellmodelDefinefunction

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material ID defined in *MAT_.
















   ..
       !! processed by numpydoc !!

.. py:property:: nstate
   :type: Optional[int]


   
   Get or set the Number of state variables u1,u2,...un.The maximum value is 7.
















   ..
       !! processed by numpydoc !!

.. py:property:: fswitch
   :type: int


   
   Get or set the Switch for the ODE definition (see Remark 1):
   EQ.0:   functions
   EQ.1 : derivatives
















   ..
       !! processed by numpydoc !!

.. py:property:: dvdt
   :type: Optional[int]


   
   Get or set the Function ID (see *DEFINE_FUNCTION) for evolution of V (function g in the equations).
















   ..
       !! processed by numpydoc !!

.. py:property:: du1dt
   :type: Optional[int]


   
   Get or set the Function ID (see *DEFINE_FUNCTION) for evolution of u_i (function f_i in the equations)
















   ..
       !! processed by numpydoc !!

.. py:property:: du2dt
   :type: Optional[int]


   
   Get or set the Function ID (see *DEFINE_FUNCTION) for evolution of u_i (function f_i in the equations).
















   ..
       !! processed by numpydoc !!

.. py:property:: du3dt
   :type: Optional[int]


   
   Get or set the Function ID (see *DEFINE_FUNCTION) for evolution of u_i (function f_i in the equations).
















   ..
       !! processed by numpydoc !!

.. py:property:: du4dt
   :type: Optional[int]


   
   Get or set the Function ID (see *DEFINE_FUNCTION) for evolution of u_i (function f_i in the equations).
















   ..
       !! processed by numpydoc !!

.. py:property:: du5dt
   :type: Optional[int]


   
   Get or set the Function ID (see *DEFINE_FUNCTION) for evolution of u_i (function f_i in the equations).
















   ..
       !! processed by numpydoc !!

.. py:property:: du6dt
   :type: Optional[int]


   
   Get or set the Function ID (see *DEFINE_FUNCTION) for evolution of u_i (function f_i in the equations).
















   ..
       !! processed by numpydoc !!

.. py:property:: du7dt
   :type: Optional[int]


   
   Get or set the Function ID (see *DEFINE_FUNCTION) for evolution of u_i (function f_i in the equations).
















   ..
       !! processed by numpydoc !!

.. py:property:: v0
   :type: Optional[int]


   
   Get or set the Define Function ID for initial values of V.
















   ..
       !! processed by numpydoc !!

.. py:property:: u1
   :type: Optional[int]


   
   Get or set the Define Function ID for initial values of u.
















   ..
       !! processed by numpydoc !!

.. py:property:: u2
   :type: Optional[int]


   
   Get or set the Define Function ID for initial values of u.
















   ..
       !! processed by numpydoc !!

.. py:property:: u3
   :type: Optional[int]


   
   Get or set the Define Function ID for initial values of u.
















   ..
       !! processed by numpydoc !!

.. py:property:: u4
   :type: Optional[int]


   
   Get or set the Define Function ID for initial values of u.
















   ..
       !! processed by numpydoc !!

.. py:property:: u5
   :type: Optional[int]


   
   Get or set the Define Function ID for initial values of u.
















   ..
       !! processed by numpydoc !!

.. py:property:: u6
   :type: Optional[int]


   
   Get or set the Define Function ID for initial values of u.
















   ..
       !! processed by numpydoc !!

.. py:property:: u7
   :type: Optional[int]


   
   Get or set the Define Function ID for initial values of u.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EM'


.. py:attribute:: subkeyword
   :value: 'EP_CELLMODEL_DEFINEFUNCTION'






