





:class:`ConstrainedGlobal`
==========================


.. py:class:: constrained_global.ConstrainedGlobal(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONSTRAINED_GLOBAL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ConstrainedGlobal

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~tc`
            - Get or set the Translational constraint:
          * - :py:attr:`~rc`
            - Get or set the Rotational constraint:
          * - :py:attr:`~dir`
            - Get or set the Direction of normal
          * - :py:attr:`~x`
            - Get or set the x-offset coordinate.
          * - :py:attr:`~y`
            - Get or set the y-offset coordinate.
          * - :py:attr:`~z`
            - Get or set the z-offset coordinate.
          * - :py:attr:`~tol`
            - Get or set the User-defined tolerance in length units. If non-zero, the internal mesh-size dependent tolerance gets replaced by this value.


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

    from constrained_global import ConstrainedGlobal

Property detail
---------------

.. py:property:: tc
   :type: int


   
   Get or set the Translational constraint:
   EQ.0: no constraints added,
   EQ.1: constrained x-translation,
   EQ.2: constrained y-translation,
   EQ.3: constrained z-translation,
   EQ.4: constrained x and y translations,
   EQ.5: constrained y and z translations,
   EQ.6: constrained x and z translations,
   EQ.7: constrained x, y, and z translations.
















   ..
       !! processed by numpydoc !!

.. py:property:: rc
   :type: int


   
   Get or set the Rotational constraint:
   EQ.0: no constraints added,
   EQ.1: constrained x-rotation,
   EQ.2: constrained y-rotation,
   EQ.3: constrained z-rotation,
   EQ.4: constrained x and y rotations,
   EQ.5: constrained y and z rotations,
   EQ.6: constrained z and x rotations,
   EQ.7: constrained x, y, and z rotations.
















   ..
       !! processed by numpydoc !!

.. py:property:: dir
   :type: int


   
   Get or set the Direction of normal
   EQ.0: no constraints added,
   EQ.1: global x,
   EQ.2: global y,
   EQ.3: global z.
















   ..
       !! processed by numpydoc !!

.. py:property:: x
   :type: float


   
   Get or set the x-offset coordinate.
















   ..
       !! processed by numpydoc !!

.. py:property:: y
   :type: float


   
   Get or set the y-offset coordinate.
















   ..
       !! processed by numpydoc !!

.. py:property:: z
   :type: float


   
   Get or set the z-offset coordinate.
















   ..
       !! processed by numpydoc !!

.. py:property:: tol
   :type: float


   
   Get or set the User-defined tolerance in length units. If non-zero, the internal mesh-size dependent tolerance gets replaced by this value.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONSTRAINED'


.. py:attribute:: subkeyword
   :value: 'GLOBAL'






