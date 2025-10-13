





:class:`LsoPointSet`
====================


.. py:class:: lso_point_set.LsoPointSet(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA LSO_POINT_SET keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: LsoPointSet

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~setid`
            - Get or set the Identifier for this point set. Called by *LSO_DOMAIN.
          * - :py:attr:`~use`
            - Get or set the Points in this set are used as:
          * - :py:attr:`~x`
            - Get or set the Coordinates of the point.
          * - :py:attr:`~y`
            - Get or set the Coordinates of the point.
          * - :py:attr:`~z`
            - Get or set the Coordinates of the point.


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

    from lso_point_set import LsoPointSet

Property detail
---------------

.. py:property:: setid
   :type: Optional[int]


   
   Get or set the Identifier for this point set. Called by *LSO_DOMAIN.
















   ..
       !! processed by numpydoc !!

.. py:property:: use
   :type: int


   
   Get or set the Points in this set are used as:
   EQ.1: Fixed time history points (default)
   EQ.2: Positions of tracer particles
















   ..
       !! processed by numpydoc !!

.. py:property:: x
   :type: Optional[float]


   
   Get or set the Coordinates of the point.
















   ..
       !! processed by numpydoc !!

.. py:property:: y
   :type: Optional[float]


   
   Get or set the Coordinates of the point.
















   ..
       !! processed by numpydoc !!

.. py:property:: z
   :type: Optional[float]


   
   Get or set the Coordinates of the point.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'LSO'


.. py:attribute:: subkeyword
   :value: 'POINT_SET'






