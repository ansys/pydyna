





:class:`DatabaseMassout`
========================


.. py:class:: database_massout.DatabaseMassout(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DATABASE_MASSOUT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DatabaseMassout

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~setid`
            - Get or set the Optional set ID.
          * - :py:attr:`~ndflg`
            - Get or set the Database extent:
          * - :py:attr:`~rbflg`
            - Get or set the Rigid body data:


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

    from database_massout import DatabaseMassout

Property detail
---------------

.. py:property:: setid
   :type: Optional[int]


   
   Get or set the Optional set ID.
   EQ.0: mass output for all nodes,
   LT.0: no output,
   GT.0: set ID identifying nodes whose mass will be output.
















   ..
       !! processed by numpydoc !!

.. py:property:: ndflg
   :type: int


   
   Get or set the Database extent:
   EQ.1: output translational mass for deformable nodes identified by      SETID (default),
   EQ.2: output translational mass and rotary inertias for the deformable  nodes identified by the SETID.
   EQ.3: output translational mass for deformable and rigid nodes identified by SETID (default),
   EQ.4: output translational mass and rotary inertias for the deformable  and rigid nodes identified by the SETID.
















   ..
       !! processed by numpydoc !!

.. py:property:: rbflg
   :type: int


   
   Get or set the Rigid body data:
   EQ.0: no output for rigid bodies,
   EQ.1: output rigid body mass and inertia.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DATABASE'


.. py:attribute:: subkeyword
   :value: 'MASSOUT'






