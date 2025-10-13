





:class:`ControlFormingAutopositionParameterPositive`
====================================================


.. py:class:: control_forming_autoposition_parameter_positive.ControlFormingAutopositionParameterPositive(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_FORMING_AUTOPOSITION_PARAMETER_POSITIVE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlFormingAutopositionParameterPositive

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Part ID. This part will be moved based on the following controlling parameters.
          * - :py:attr:`~cid`
            - Get or set the Coordinate ID set with *DEFINE_COORDINATE_SYSTEM or *DEFINE_COORDINATE_VECTOR. The default is the global coordinate system.
          * - :py:attr:`~dir`
            - Get or set the Direction that the part will be moved
          * - :py:attr:`~mpid`
            - Get or set the Part ID. The part (with PID) will be moved based on part defined by MPID.
          * - :py:attr:`~position`
            - Get or set the .EQ1: means that PID is above MPID       .EQ-1: means that PID is below MPID
          * - :py:attr:`~premove`
            - Get or set the PID is moved with a value of PREMOVE. If this parameter is defined, it is unnecessary to define MPID
          * - :py:attr:`~thick`
            - Get or set the Part thickness
          * - :py:attr:`~porder`
            - Get or set the The name of the parameters in the parameter list.


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

    from control_forming_autoposition_parameter_positive import ControlFormingAutopositionParameterPositive

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID. This part will be moved based on the following controlling parameters.
















   ..
       !! processed by numpydoc !!

.. py:property:: cid
   :type: Optional[int]


   
   Get or set the Coordinate ID set with *DEFINE_COORDINATE_SYSTEM or *DEFINE_COORDINATE_VECTOR. The default is the global coordinate system.
   LT.0: | CID | is vector ID giving the direction the part will be moved
















   ..
       !! processed by numpydoc !!

.. py:property:: dir
   :type: int


   
   Get or set the Direction that the part will be moved
   .EQ.1:  x direction
   .EQ.2:  y direction
   .EQ.3:  z direction
















   ..
       !! processed by numpydoc !!

.. py:property:: mpid
   :type: Optional[int]


   
   Get or set the Part ID. The part (with PID) will be moved based on part defined by MPID.
















   ..
       !! processed by numpydoc !!

.. py:property:: position
   :type: int


   
   Get or set the .EQ1: means that PID is above MPID       .EQ-1: means that PID is below MPID
















   ..
       !! processed by numpydoc !!

.. py:property:: premove
   :type: Optional[float]


   
   Get or set the PID is moved with a value of PREMOVE. If this parameter is defined, it is unnecessary to define MPID
















   ..
       !! processed by numpydoc !!

.. py:property:: thick
   :type: Optional[float]


   
   Get or set the Part thickness
















   ..
       !! processed by numpydoc !!

.. py:property:: porder
   :type: Optional[str]


   
   Get or set the The name of the parameters in the parameter list.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'FORMING_AUTOPOSITION_PARAMETER_POSITIVE'






