





:class:`ControlFormingPreBendingLocal`
======================================


.. py:class:: control_forming_pre_bending_local.ControlFormingPreBendingLocal(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_FORMING_PRE_BENDING_LOCAL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlFormingPreBendingLocal

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pset`
            - Get or set the Part set ID to be included in the pre-bending.
          * - :py:attr:`~radius`
            - Get or set the Radius of the pre-bending.
          * - :py:attr:`~vx`
            - Get or set the x Vector components of an axis about which the flat blank will be bent.
          * - :py:attr:`~vy`
            - Get or set the y Vector components of an axis about which the flat blank will be bent.
          * - :py:attr:`~vz`
            - Get or set the z Vector components of an axis about which the flat blank will be bent.
          * - :py:attr:`~xc`
            - Get or set the X coordinates of the center of most-bent location. If undefined,center of gravity of the blank will be used as a default.
          * - :py:attr:`~yc`
            - Get or set the Y coordinates of the center of most-bent location. If undefined,center of gravity of the blank will be used as a default..
          * - :py:attr:`~zc`
            - Get or set the Z coordinates of the center of most-bent location. If undefined,center of gravity of the blank will be used as a default..
          * - :py:attr:`~cid`
            - Get or set the ID for a local coordinate system in which the blank will be bent.


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

    from control_forming_pre_bending_local import ControlFormingPreBendingLocal

Property detail
---------------

.. py:property:: pset
   :type: Optional[int]


   
   Get or set the Part set ID to be included in the pre-bending.
















   ..
       !! processed by numpydoc !!

.. py:property:: radius
   :type: Optional[float]


   
   Get or set the Radius of the pre-bending.
   GT.0.0: bending center is on the same side as the element normals
   LT.0.0: bending center is on the reverse side of the element normals.
   See figure below for more information.
















   ..
       !! processed by numpydoc !!

.. py:property:: vx
   :type: Optional[float]


   
   Get or set the x Vector components of an axis about which the flat blank will be bent.
















   ..
       !! processed by numpydoc !!

.. py:property:: vy
   :type: Optional[float]


   
   Get or set the y Vector components of an axis about which the flat blank will be bent.
















   ..
       !! processed by numpydoc !!

.. py:property:: vz
   :type: Optional[float]


   
   Get or set the z Vector components of an axis about which the flat blank will be bent.
















   ..
       !! processed by numpydoc !!

.. py:property:: xc
   :type: Optional[float]


   
   Get or set the X coordinates of the center of most-bent location. If undefined,center of gravity of the blank will be used as a default.
















   ..
       !! processed by numpydoc !!

.. py:property:: yc
   :type: Optional[float]


   
   Get or set the Y coordinates of the center of most-bent location. If undefined,center of gravity of the blank will be used as a default..
















   ..
       !! processed by numpydoc !!

.. py:property:: zc
   :type: Optional[float]


   
   Get or set the Z coordinates of the center of most-bent location. If undefined,center of gravity of the blank will be used as a default..
















   ..
       !! processed by numpydoc !!

.. py:property:: cid
   :type: Optional[int]


   
   Get or set the ID for a local coordinate system in which the blank will be bent.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'FORMING_PRE_BENDING_LOCAL'






