





:class:`ControlFormingParameterRead`
====================================


.. py:class:: control_forming_parameter_read.ControlFormingParameterRead(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_FORMING_PARAMETER_READ keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlFormingParameterRead

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~filename`
            - Get or set the file name will be opened to read
          * - :py:attr:`~paraname`
            - Get or set the The name of the parameters in the parameter list
          * - :py:attr:`~imethod`
            - Get or set the which method to be used
          * - :py:attr:`~line`
            - Get or set the the line # in filename
          * - :py:attr:`~nbegpa`
            - Get or set the the value defined between nbegpa and nendpa will be read for paraname
          * - :py:attr:`~nendpa_`
            - Get or set the the value defined between nbegpa and nendpa will be read for paraname
          * - :py:attr:`~value_`
            - Get or set the the value of the defined parameter


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

    from control_forming_parameter_read import ControlFormingParameterRead

Property detail
---------------

.. py:property:: filename
   :type: Optional[str]


   
   Get or set the file name will be opened to read
















   ..
       !! processed by numpydoc !!

.. py:property:: paraname
   :type: Optional[str]


   
   Get or set the The name of the parameters in the parameter list
















   ..
       !! processed by numpydoc !!

.. py:property:: imethod
   :type: Optional[int]


   
   Get or set the which method to be used
















   ..
       !! processed by numpydoc !!

.. py:property:: line
   :type: Optional[int]


   
   Get or set the the line # in filename
















   ..
       !! processed by numpydoc !!

.. py:property:: nbegpa
   :type: Optional[int]


   
   Get or set the the value defined between nbegpa and nendpa will be read for paraname
















   ..
       !! processed by numpydoc !!

.. py:property:: nendpa_
   :type: Optional[int]


   
   Get or set the the value defined between nbegpa and nendpa will be read for paraname
















   ..
       !! processed by numpydoc !!

.. py:property:: value_
   :type: Optional[float]


   
   Get or set the the value of the defined parameter
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'FORMING_PARAMETER_READ'






