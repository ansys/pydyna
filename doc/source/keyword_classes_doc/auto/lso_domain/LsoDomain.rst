





:class:`LsoDomain`
==================


.. py:class:: lso_domain.LsoDomain(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA LSO_DOMAIN keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: LsoDomain

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~domain_type`
            - Get or set the The type of domain for which LSO output may be generated.Accepted entries so far are 'thist_point'
          * - :py:attr:`~solver_name`
            - Get or set the Selects the solver from which data is output on this domain.
          * - :py:attr:`~outid`
            - Get or set the LSO domain ID associated with this domain, and used by *LSO_TIME_SEQUENCE cards.
          * - :py:attr:`~refid`
            - Get or set the Support set ID. This can be a set defined by a *SET card, a *LSO_ID_SET, card, or a *LSO_POINT_SET card. Unless OVERRIDE is specified,this set must be of the same type as DOMAIN_TYPE.
          * - :py:attr:`~reduct`
            - Get or set the A function that operates on the entire domain and returns a single
          * - :py:attr:`~override`
            - Get or set the If non-zero, then REFID is interpreted as:
          * - :py:attr:`~variable_name`
            - Get or set the Either the name of a single output variable or a variable group. If no names are given, then no output occurs.


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

    from lso_domain import LsoDomain

Property detail
---------------

.. py:property:: domain_type
   :type: str


   
   Get or set the The type of domain for which LSO output may be generated.Accepted entries so far are 'thist_point'
















   ..
       !! processed by numpydoc !!

.. py:property:: solver_name
   :type: str


   
   Get or set the Selects the solver from which data is output on this domain.
   Accepted entries so far are 'em', 'cese' and 'icfd'.
















   ..
       !! processed by numpydoc !!

.. py:property:: outid
   :type: Optional[int]


   
   Get or set the LSO domain ID associated with this domain, and used by *LSO_TIME_SEQUENCE cards.
















   ..
       !! processed by numpydoc !!

.. py:property:: refid
   :type: Optional[int]


   
   Get or set the Support set ID. This can be a set defined by a *SET card, a *LSO_ID_SET, card, or a *LSO_POINT_SET card. Unless OVERRIDE is specified,this set must be of the same type as DOMAIN_TYPE.
















   ..
       !! processed by numpydoc !!

.. py:property:: reduct
   :type: Optional[int]


   
   Get or set the A function that operates on the entire domain and returns a single
   value for scalar variables, three values for vector variables, or 6
   values for symmetric tensor variables. For REDUCT=range, the
   number of returned values doubles. The following are the supported
   functions:
   EQ.BLANK: no reduction (default)
   EQ.none: Same as above
   EQ.avg: the average by component
   EQ.average: Same as above
   EQ.min: the minimum by component
   EQ.minimum: Same as above
   EQ.max: the maximum by component
   EQ.maximum: Same as above
   EQ.maximum: Same as above
   EQ.range: the minimum by component followed by the maximum by component.
















   ..
       !! processed by numpydoc !!

.. py:property:: override
   :type: int


   
   Get or set the If non-zero, then REFID is interpreted as:
   .EQ.1: a PART set for SOLVER_NAME
   EQ.2: a PART set of volume parts created with a *LSO_ID_SET card (volume parts are defined with *MESH_VOLUME cards).
   EQ.3: a PART set of surface parts created with a *LSO_ID_SET card (surface parts are defined with *MESH_SURFACE_ELEMENT cards).
   EQ.4: a set of segment sets created with a *LSO_ID_SET card.
















   ..
       !! processed by numpydoc !!

.. py:property:: variable_name
   :type: Optional[str]


   
   Get or set the Either the name of a single output variable or a variable group. If no names are given, then no output occurs.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'LSO'


.. py:attribute:: subkeyword
   :value: 'DOMAIN'






