





:class:`ControlFormingTipping`
==============================


.. py:class:: control_forming_tipping.ControlFormingTipping(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_FORMING_TIPPING keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlFormingTipping

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the PID or part set ID that requires tipping and/or translation
          * - :py:attr:`~itype`
            - Get or set the Type of PID (see remark 1):
          * - :py:attr:`~ifstrn`
            - Get or set the Strain tensors.4
          * - :py:attr:`~ifstrs`
            - Get or set the Stress tensors.
          * - :py:attr:`~nmove`
            - Get or set the Total number of tipping and translation included under this keyword
          * - :py:attr:`~rot_tran`
            - Get or set the Transformation type.
          * - :py:attr:`~v11`
            - Get or set the Direction cosines of an axis about which tipping is performed
          * - :py:attr:`~v12`
            - Get or set the Direction cosines of an axis about which tipping is performed
          * - :py:attr:`~v13`
            - Get or set the Direction cosines of an axis about which tipping is performed
          * - :py:attr:`~x01`
            - Get or set the X coordinates of a point through which the tipping axis passes
          * - :py:attr:`~y01`
            - Get or set the Y coordinates of a point through which the tipping axis passes
          * - :py:attr:`~z01`
            - Get or set the Z coordinates of a point through which the tipping axis passes
          * - :py:attr:`~dista1`
            - Get or set the Tipping angle in degree
          * - :py:attr:`~dx`
            - Get or set the Translation distance along X-axis
          * - :py:attr:`~dy`
            - Get or set the Translation distance along Y-axis
          * - :py:attr:`~dz`
            - Get or set the Translation distance along Z-axis.


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

    from control_forming_tipping import ControlFormingTipping

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the PID or part set ID that requires tipping and/or translation
















   ..
       !! processed by numpydoc !!

.. py:property:: itype
   :type: int


   
   Get or set the Type of PID (see remark 1):
   EQ.0:  part set ID (PSID).
   EQ.1:  part ID (PID)
















   ..
       !! processed by numpydoc !!

.. py:property:: ifstrn
   :type: Optional[int]


   
   Get or set the Strain tensors.4
   EQ.1: included in tipping/translation.
















   ..
       !! processed by numpydoc !!

.. py:property:: ifstrs
   :type: Optional[int]


   
   Get or set the Stress tensors.
   EQ.1: included in tipping/translation.
















   ..
       !! processed by numpydoc !!

.. py:property:: nmove
   :type: Optional[int]


   
   Get or set the Total number of tipping and translation included under this keyword
















   ..
       !! processed by numpydoc !!

.. py:property:: rot_tran
   :type: int


   
   Get or set the Transformation type.
   EQ.1: rotation.
   EQ.2: translation.
















   ..
       !! processed by numpydoc !!

.. py:property:: v11
   :type: Optional[float]


   
   Get or set the Direction cosines of an axis about which tipping is performed
















   ..
       !! processed by numpydoc !!

.. py:property:: v12
   :type: Optional[float]


   
   Get or set the Direction cosines of an axis about which tipping is performed
















   ..
       !! processed by numpydoc !!

.. py:property:: v13
   :type: Optional[float]


   
   Get or set the Direction cosines of an axis about which tipping is performed
















   ..
       !! processed by numpydoc !!

.. py:property:: x01
   :type: Optional[float]


   
   Get or set the X coordinates of a point through which the tipping axis passes
















   ..
       !! processed by numpydoc !!

.. py:property:: y01
   :type: Optional[float]


   
   Get or set the Y coordinates of a point through which the tipping axis passes
















   ..
       !! processed by numpydoc !!

.. py:property:: z01
   :type: Optional[float]


   
   Get or set the Z coordinates of a point through which the tipping axis passes
















   ..
       !! processed by numpydoc !!

.. py:property:: dista1
   :type: Optional[float]


   
   Get or set the Tipping angle in degree
















   ..
       !! processed by numpydoc !!

.. py:property:: dx
   :type: Optional[float]


   
   Get or set the Translation distance along X-axis
















   ..
       !! processed by numpydoc !!

.. py:property:: dy
   :type: Optional[float]


   
   Get or set the Translation distance along Y-axis
















   ..
       !! processed by numpydoc !!

.. py:property:: dz
   :type: Optional[float]


   
   Get or set the Translation distance along Z-axis.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'FORMING_TIPPING'






