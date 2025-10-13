





:class:`CeseDefineNoninertial`
==============================


.. py:class:: cese_define_noninertial.CeseDefineNoninertial(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CESE_DEFINE_NONINERTIAL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: CeseDefineNoninertial

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~freq`
            - Get or set the Frequency of rotation.
          * - :py:attr:`~lcid`
            - Get or set the Load curve ID for scaling factor of FREQ.
          * - :py:attr:`~pid`
            - Get or set the Starting point ID for the reference frame (See *CESE_DEFINE_POINT).
          * - :py:attr:`~nx`
            - Get or set the Rotating axis direction.
          * - :py:attr:`~ny`
            - Get or set the Rotating axis direction.
          * - :py:attr:`~nz`
            - Get or set the Rotating axis direction.
          * - :py:attr:`~l`
            - Get or set the Length of rotating frame.
          * - :py:attr:`~r`
            - Get or set the Radius of rotating frame.
          * - :py:attr:`~relv`
            - Get or set the Velocity display mode:


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

    from cese_define_noninertial import CeseDefineNoninertial

Property detail
---------------

.. py:property:: freq
   :type: Optional[float]


   
   Get or set the Frequency of rotation.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: int


   
   Get or set the Load curve ID for scaling factor of FREQ.
















   ..
       !! processed by numpydoc !!

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Starting point ID for the reference frame (See *CESE_DEFINE_POINT).
















   ..
       !! processed by numpydoc !!

.. py:property:: nx
   :type: Optional[float]


   
   Get or set the Rotating axis direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: ny
   :type: Optional[float]


   
   Get or set the Rotating axis direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: nz
   :type: Optional[float]


   
   Get or set the Rotating axis direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: l
   :type: Optional[float]


   
   Get or set the Length of rotating frame.
















   ..
       !! processed by numpydoc !!

.. py:property:: r
   :type: Optional[float]


   
   Get or set the Radius of rotating frame.
















   ..
       !! processed by numpydoc !!

.. py:property:: relv
   :type: int


   
   Get or set the Velocity display mode:
   EQ.0: Relative velocity, only the non-rotating components of the velocity are output.
   EQ.1: Absolute velocity is output.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CESE'


.. py:attribute:: subkeyword
   :value: 'DEFINE_NONINERTIAL'






