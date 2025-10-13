





:class:`DefineSphMassflowPlane`
===============================


.. py:class:: define_sph_massflow_plane.DefineSphMassflowPlane(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_SPH_MASSFLOW_PLANE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineSphMassflowPlane

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~prtclsid`
            - Get or set the Node set ID, node ID, part set ID or part ID specifying SPH particles to be measured.  PTYPE below indicates the ID type specified by PRTCLSID
          * - :py:attr:`~surfsid`
            - Get or set the Part set ID or part ID defining the surface across which the flow rate is measured.  STYPE below indicates the ID type specified by SURFSID
          * - :py:attr:`~ptype`
            - Get or set the PRTCLSID type:
          * - :py:attr:`~stype`
            - Get or set the SURFSID type:
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

    from define_sph_massflow_plane import DefineSphMassflowPlane

Property detail
---------------

.. py:property:: prtclsid
   :type: int


   
   Get or set the Node set ID, node ID, part set ID or part ID specifying SPH particles to be measured.  PTYPE below indicates the ID type specified by PRTCLSID
















   ..
       !! processed by numpydoc !!

.. py:property:: surfsid
   :type: int


   
   Get or set the Part set ID or part ID defining the surface across which the flow rate is measured.  STYPE below indicates the ID type specified by SURFSID
















   ..
       !! processed by numpydoc !!

.. py:property:: ptype
   :type: int


   
   Get or set the PRTCLSID type:
   EQ.0:   Node set
   EQ.1 : Node
   EQ.2 : Part set
   EQ.3 : Part
















   ..
       !! processed by numpydoc !!

.. py:property:: stype
   :type: int


   
   Get or set the SURFSID type:
   EQ.0:   Part set
   EQ.1 : Part
















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
   :value: 'SPH_MASSFLOW_PLANE'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





