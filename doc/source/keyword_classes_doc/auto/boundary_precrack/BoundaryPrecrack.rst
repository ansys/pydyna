





:class:`BoundaryPrecrack`
=========================


.. py:class:: boundary_precrack.BoundaryPrecrack(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BOUNDARY_PRECRACK keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BoundaryPrecrack

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Part ID where the pre-crack is located
          * - :py:attr:`~ctype`
            - Get or set the Type of pre-crack:
          * - :py:attr:`~np`
            - Get or set the Number of points defining the pre-crack
          * - :py:attr:`~x`
            - Get or set the Coordinates of the points defining the pre-crack.  It is recommended that these points be defined such that the pre-crack does not coincide with mesh lines.   A pre-crack coinciding with mesh lines will be automatically moved with sometimes unexpected results, e.g., the moved pre-crack location does not lie on part PID and the pre-crack cannot be created
          * - :py:attr:`~y`
            - Get or set the Coordinates of the points defining the pre-crack.  It is recommended that these points be defined such that the pre-crack does not coincide with mesh lines.   A pre-crack coinciding with mesh lines will be automatically moved with sometimes unexpected results, e.g., the moved pre-crack location does not lie on part PID and the pre-crack cannot be created
          * - :py:attr:`~z`
            - Get or set the Coordinates of the points defining the pre-crack.  It is recommended that these points be defined such that the pre-crack does not coincide with mesh lines.   A pre-crack coinciding with mesh lines will be automatically moved with sometimes unexpected results, e.g., the moved pre-crack location does not lie on part PID and the pre-crack cannot be created


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

    from boundary_precrack import BoundaryPrecrack

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID where the pre-crack is located
















   ..
       !! processed by numpydoc !!

.. py:property:: ctype
   :type: int


   
   Get or set the Type of pre-crack:
   EQ.1: straight line
















   ..
       !! processed by numpydoc !!

.. py:property:: np
   :type: Optional[int]


   
   Get or set the Number of points defining the pre-crack
















   ..
       !! processed by numpydoc !!

.. py:property:: x
   :type: Optional[float]


   
   Get or set the Coordinates of the points defining the pre-crack.  It is recommended that these points be defined such that the pre-crack does not coincide with mesh lines.   A pre-crack coinciding with mesh lines will be automatically moved with sometimes unexpected results, e.g., the moved pre-crack location does not lie on part PID and the pre-crack cannot be created
















   ..
       !! processed by numpydoc !!

.. py:property:: y
   :type: Optional[float]


   
   Get or set the Coordinates of the points defining the pre-crack.  It is recommended that these points be defined such that the pre-crack does not coincide with mesh lines.   A pre-crack coinciding with mesh lines will be automatically moved with sometimes unexpected results, e.g., the moved pre-crack location does not lie on part PID and the pre-crack cannot be created
















   ..
       !! processed by numpydoc !!

.. py:property:: z
   :type: Optional[float]


   
   Get or set the Coordinates of the points defining the pre-crack.  It is recommended that these points be defined such that the pre-crack does not coincide with mesh lines.   A pre-crack coinciding with mesh lines will be automatically moved with sometimes unexpected results, e.g., the moved pre-crack location does not lie on part PID and the pre-crack cannot be created
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'BOUNDARY'


.. py:attribute:: subkeyword
   :value: 'PRECRACK'






