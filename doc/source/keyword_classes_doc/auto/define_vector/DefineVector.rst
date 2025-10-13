





:class:`DefineVector`
=====================


.. py:class:: define_vector.DefineVector(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_VECTOR keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineVector

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~vid`
            - Get or set the Vector ID. A unique number has to be used.
          * - :py:attr:`~xt`
            - Get or set the x-coordinate of tail of vector.
          * - :py:attr:`~yt`
            - Get or set the y-coordinate of tail of vector.
          * - :py:attr:`~zt`
            - Get or set the z-coordinate of tail of vector.
          * - :py:attr:`~xh`
            - Get or set the x-coordinate of head of vector.
          * - :py:attr:`~yh`
            - Get or set the y-coordinate of head of vector.
          * - :py:attr:`~zh`
            - Get or set the z-coordinate of head of vector.
          * - :py:attr:`~cid`
            - Get or set the Coordinate system ID to define vector in loacal coordinate system. All coordinates, XT,YT,ZT,XH,YH, and ZH are in prespect to cid. 0 gobal
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

    from define_vector import DefineVector

Property detail
---------------

.. py:property:: vid
   :type: int


   
   Get or set the Vector ID. A unique number has to be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: xt
   :type: float


   
   Get or set the x-coordinate of tail of vector.
















   ..
       !! processed by numpydoc !!

.. py:property:: yt
   :type: float


   
   Get or set the y-coordinate of tail of vector.
















   ..
       !! processed by numpydoc !!

.. py:property:: zt
   :type: float


   
   Get or set the z-coordinate of tail of vector.
















   ..
       !! processed by numpydoc !!

.. py:property:: xh
   :type: float


   
   Get or set the x-coordinate of head of vector.
















   ..
       !! processed by numpydoc !!

.. py:property:: yh
   :type: float


   
   Get or set the y-coordinate of head of vector.
















   ..
       !! processed by numpydoc !!

.. py:property:: zh
   :type: float


   
   Get or set the z-coordinate of head of vector.
















   ..
       !! processed by numpydoc !!

.. py:property:: cid
   :type: int


   
   Get or set the Coordinate system ID to define vector in loacal coordinate system. All coordinates, XT,YT,ZT,XH,YH, and ZH are in prespect to cid. 0 gobal
















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
   :value: 'VECTOR'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





