





:class:`ControlFormingAutopositionParameterSet`
===============================================


.. py:class:: control_forming_autoposition_parameter_set.ControlFormingAutopositionParameterSet(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_FORMING_AUTOPOSITION_PARAMETER_SET keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlFormingAutopositionParameterSet

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~psid`
            - Get or set the Part set ID. This part will be moved based on the following controlling parameters.
          * - :py:attr:`~cid`
            - Get or set the Coordinate ID (Default means global coordinate ID)
          * - :py:attr:`~dir`
            - Get or set the Direction that the part will be moved
          * - :py:attr:`~mpsid`
            - Get or set the Part set ID. The part (with PID) will be moved based on part defined by MPID.
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

    from control_forming_autoposition_parameter_set import ControlFormingAutopositionParameterSet

Property detail
---------------

.. py:property:: psid
   :type: Optional[int]


   
   Get or set the Part set ID. This part will be moved based on the following controlling parameters.
















   ..
       !! processed by numpydoc !!

.. py:property:: cid
   :type: Optional[int]


   
   Get or set the Coordinate ID (Default means global coordinate ID)
















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

.. py:property:: mpsid
   :type: Optional[int]


   
   Get or set the Part set ID. The part (with PID) will be moved based on part defined by MPID.
















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
   :value: 'FORMING_AUTOPOSITION_PARAMETER_SET'






