





:class:`EmBoundary`
===================


.. py:class:: em_boundary.EmBoundary(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EM_BOUNDARY keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EmBoundary

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ssid`
            - Get or set the Segment set ID
          * - :py:attr:`~btype`
            - Get or set the EQ.9: The faces of this segment set are eliminated from the BEM calculations (used for example for the rear or side faces of a workpiece)


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

    from em_boundary import EmBoundary

Property detail
---------------

.. py:property:: ssid
   :type: Optional[int]


   
   Get or set the Segment set ID
   .
















   ..
       !! processed by numpydoc !!

.. py:property:: btype
   :type: int


   
   Get or set the EQ.9: The faces of this segment set are eliminated from the BEM calculations (used for example for the rear or side faces of a workpiece)
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EM'


.. py:attribute:: subkeyword
   :value: 'BOUNDARY'






