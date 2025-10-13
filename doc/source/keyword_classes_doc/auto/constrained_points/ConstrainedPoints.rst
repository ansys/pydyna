





:class:`ConstrainedPoints`
==========================


.. py:class:: constrained_points.ConstrainedPoints(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONSTRAINED_POINTS keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ConstrainedPoints

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~cid`
            - Get or set the Constrained points ID.
          * - :py:attr:`~eid1`
            - Get or set the First Shell element ID
          * - :py:attr:`~x1`
            - Get or set the X Coordinates of the constrained point 1
          * - :py:attr:`~y1`
            - Get or set the Y Coordinates of the constrained point 1
          * - :py:attr:`~z1`
            - Get or set the Z Coordinates of the constrained point 1
          * - :py:attr:`~eid2`
            - Get or set the Second Shell element ID
          * - :py:attr:`~x2`
            - Get or set the X Coordinates of the constrained point 2
          * - :py:attr:`~y2`
            - Get or set the Y Coordinates of the constrained point 2
          * - :py:attr:`~z2`
            - Get or set the Z Coordinates of the constrained point 2
          * - :py:attr:`~psf`
            - Get or set the Penalty scale factor (Default=1.0).
          * - :py:attr:`~faila`
            - Get or set the Axial force resultant failure value (Skip if zero.).
          * - :py:attr:`~fails`
            - Get or set the Shear force resultant failure value (Skip if zero.).
          * - :py:attr:`~failm`
            - Get or set the Moment resultant failure value (Skip if zero.).


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

    from constrained_points import ConstrainedPoints

Property detail
---------------

.. py:property:: cid
   :type: Optional[int]


   
   Get or set the Constrained points ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: eid1
   :type: Optional[int]


   
   Get or set the First Shell element ID
















   ..
       !! processed by numpydoc !!

.. py:property:: x1
   :type: float


   
   Get or set the X Coordinates of the constrained point 1
















   ..
       !! processed by numpydoc !!

.. py:property:: y1
   :type: float


   
   Get or set the Y Coordinates of the constrained point 1
















   ..
       !! processed by numpydoc !!

.. py:property:: z1
   :type: float


   
   Get or set the Z Coordinates of the constrained point 1
















   ..
       !! processed by numpydoc !!

.. py:property:: eid2
   :type: Optional[int]


   
   Get or set the Second Shell element ID
















   ..
       !! processed by numpydoc !!

.. py:property:: x2
   :type: float


   
   Get or set the X Coordinates of the constrained point 2
















   ..
       !! processed by numpydoc !!

.. py:property:: y2
   :type: float


   
   Get or set the Y Coordinates of the constrained point 2
















   ..
       !! processed by numpydoc !!

.. py:property:: z2
   :type: float


   
   Get or set the Z Coordinates of the constrained point 2
















   ..
       !! processed by numpydoc !!

.. py:property:: psf
   :type: float


   
   Get or set the Penalty scale factor (Default=1.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: faila
   :type: float


   
   Get or set the Axial force resultant failure value (Skip if zero.).
















   ..
       !! processed by numpydoc !!

.. py:property:: fails
   :type: float


   
   Get or set the Shear force resultant failure value (Skip if zero.).
















   ..
       !! processed by numpydoc !!

.. py:property:: failm
   :type: float


   
   Get or set the Moment resultant failure value (Skip if zero.).
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONSTRAINED'


.. py:attribute:: subkeyword
   :value: 'POINTS'






