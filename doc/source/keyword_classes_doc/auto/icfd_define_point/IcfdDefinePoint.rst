





:class:`IcfdDefinePoint`
========================


.. py:class:: icfd_define_point.IcfdDefinePoint(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ICFD_DEFINE_POINT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IcfdDefinePoint

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~poid`
            - Get or set the Point ID.
          * - :py:attr:`~x`
            - Get or set the x coordinate for the point.
          * - :py:attr:`~y`
            - Get or set the y coordinate for the point.
          * - :py:attr:`~z`
            - Get or set the z coordinate for the point.
          * - :py:attr:`~constpid`
            - Get or set the Surface Part ID to which the point is constrained. This means that if the selected surface moves, then the localization of the point will update as well.
          * - :py:attr:`~lcidx`
            - Get or set the The point can be made to translate.three load curve IDs for the three translation components.
          * - :py:attr:`~lcidy`
            - Get or set the The point can be made to translate.three load curve IDs for the three translation components.
          * - :py:attr:`~lcidz`
            - Get or set the The point can be made to translate.three load curve IDs for the three translation components.
          * - :py:attr:`~lcidw`
            - Get or set the The point can also be made to rotate. This load curve specifies the angular velocity.
          * - :py:attr:`~xt`
            - Get or set the Rotation axis tail point coordinates.
          * - :py:attr:`~yt`
            - Get or set the Rotation axis tail point coordinates.
          * - :py:attr:`~zt`
            - Get or set the Rotation axis tail point coordinates.
          * - :py:attr:`~xh`
            - Get or set the Rotation axis head point coordinates.
          * - :py:attr:`~yh`
            - Get or set the Rotation axis head point coordinates.
          * - :py:attr:`~zh`
            - Get or set the Rotation axis head point coordinates.


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

    from icfd_define_point import IcfdDefinePoint

Property detail
---------------

.. py:property:: poid
   :type: Optional[int]


   
   Get or set the Point ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: x
   :type: Optional[float]


   
   Get or set the x coordinate for the point.
















   ..
       !! processed by numpydoc !!

.. py:property:: y
   :type: Optional[float]


   
   Get or set the y coordinate for the point.
















   ..
       !! processed by numpydoc !!

.. py:property:: z
   :type: Optional[float]


   
   Get or set the z coordinate for the point.
















   ..
       !! processed by numpydoc !!

.. py:property:: constpid
   :type: Optional[int]


   
   Get or set the Surface Part ID to which the point is constrained. This means that if the selected surface moves, then the localization of the point will update as well.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidx
   :type: Optional[int]


   
   Get or set the The point can be made to translate.three load curve IDs for the three translation components.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidy
   :type: Optional[int]


   
   Get or set the The point can be made to translate.three load curve IDs for the three translation components.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidz
   :type: Optional[int]


   
   Get or set the The point can be made to translate.three load curve IDs for the three translation components.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidw
   :type: Optional[int]


   
   Get or set the The point can also be made to rotate. This load curve specifies the angular velocity.
















   ..
       !! processed by numpydoc !!

.. py:property:: xt
   :type: Optional[float]


   
   Get or set the Rotation axis tail point coordinates.
















   ..
       !! processed by numpydoc !!

.. py:property:: yt
   :type: Optional[float]


   
   Get or set the Rotation axis tail point coordinates.
















   ..
       !! processed by numpydoc !!

.. py:property:: zt
   :type: Optional[float]


   
   Get or set the Rotation axis tail point coordinates.
















   ..
       !! processed by numpydoc !!

.. py:property:: xh
   :type: Optional[float]


   
   Get or set the Rotation axis head point coordinates.
















   ..
       !! processed by numpydoc !!

.. py:property:: yh
   :type: Optional[float]


   
   Get or set the Rotation axis head point coordinates.
















   ..
       !! processed by numpydoc !!

.. py:property:: zh
   :type: Optional[float]


   
   Get or set the Rotation axis head point coordinates.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ICFD'


.. py:attribute:: subkeyword
   :value: 'DEFINE_POINT'






