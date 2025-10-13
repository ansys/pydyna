





:class:`DefineSpotweldFailureResultants`
========================================


.. py:class:: define_spotweld_failure_resultants.DefineSpotweldFailureResultants(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_SPOTWELD_FAILURE_RESULTANTS keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineSpotweldFailureResultants

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the Identification number. Only one table is allowed
          * - :py:attr:`~dsn`
            - Get or set the Default value of the normal static stress resultant at failure
          * - :py:attr:`~dss`
            - Get or set the Default value of the transverse static stress resultant at failure
          * - :py:attr:`~dlcidsn`
            - Get or set the Load curve ID defining a scale factor for the normal stress resultant as a function of strain rate. This factor multiplies DSN to obtain the failure value at a given strain rate.
          * - :py:attr:`~dlcidss`
            - Get or set the Load curve ID defining a scale factor for the static shear stress resultant as a function of strain rate. This factor multiplies DSS to obtain the failure value at a given strain rate.
          * - :py:attr:`~pid_i`
            - Get or set the Part ID I.
          * - :py:attr:`~pid_j`
            - Get or set the Part ID J.
          * - :py:attr:`~snij`
            - Get or set the The normal static stress resultant at failure between parts I and J.
          * - :py:attr:`~ssij`
            - Get or set the The transverse shear static stress resultant at failure between parts I and J.
          * - :py:attr:`~lcidsnij`
            - Get or set the Load curve ID defining a scale factor for the normal stress resultant as a function of strain rate. This factor multiplies SNIJ to obtain the failure value at a given strain rate.
          * - :py:attr:`~lcidssij`
            - Get or set the Load curve ID defining a scale factor for static shear stress resultant as a function of strain rate. This factor multiplies SSIJ to obtain the failure value at a given strain rate.
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

    from define_spotweld_failure_resultants import DefineSpotweldFailureResultants

Property detail
---------------

.. py:property:: id
   :type: int


   
   Get or set the Identification number. Only one table is allowed
















   ..
       !! processed by numpydoc !!

.. py:property:: dsn
   :type: float


   
   Get or set the Default value of the normal static stress resultant at failure
















   ..
       !! processed by numpydoc !!

.. py:property:: dss
   :type: float


   
   Get or set the Default value of the transverse static stress resultant at failure
















   ..
       !! processed by numpydoc !!

.. py:property:: dlcidsn
   :type: int


   
   Get or set the Load curve ID defining a scale factor for the normal stress resultant as a function of strain rate. This factor multiplies DSN to obtain the failure value at a given strain rate.
















   ..
       !! processed by numpydoc !!

.. py:property:: dlcidss
   :type: int


   
   Get or set the Load curve ID defining a scale factor for the static shear stress resultant as a function of strain rate. This factor multiplies DSS to obtain the failure value at a given strain rate.
















   ..
       !! processed by numpydoc !!

.. py:property:: pid_i
   :type: Optional[int]


   
   Get or set the Part ID I.
















   ..
       !! processed by numpydoc !!

.. py:property:: pid_j
   :type: Optional[int]


   
   Get or set the Part ID J.
















   ..
       !! processed by numpydoc !!

.. py:property:: snij
   :type: float


   
   Get or set the The normal static stress resultant at failure between parts I and J.
















   ..
       !! processed by numpydoc !!

.. py:property:: ssij
   :type: float


   
   Get or set the The transverse shear static stress resultant at failure between parts I and J.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidsnij
   :type: int


   
   Get or set the Load curve ID defining a scale factor for the normal stress resultant as a function of strain rate. This factor multiplies SNIJ to obtain the failure value at a given strain rate.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidssij
   :type: int


   
   Get or set the Load curve ID defining a scale factor for static shear stress resultant as a function of strain rate. This factor multiplies SSIJ to obtain the failure value at a given strain rate.
















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
   :value: 'SPOTWELD_FAILURE_RESULTANTS'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





