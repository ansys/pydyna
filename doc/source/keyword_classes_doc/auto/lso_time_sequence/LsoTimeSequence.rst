





:class:`LsoTimeSequence`
========================


.. py:class:: lso_time_sequence.LsoTimeSequence(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA LSO_TIME_SEQUENCE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: LsoTimeSequence

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~solver_name`
            - Get or set the Selects the solver from which data is output in this time sequence.
          * - :py:attr:`~dt`
            - Get or set the Time interval between outputs.
          * - :py:attr:`~lcdt`
            - Get or set the Optional load curve ID specifying the time interval between dumps.
          * - :py:attr:`~lcopt`
            - Get or set the Flag to govern behavior of plot frequency load curve.
          * - :py:attr:`~npltc`
            - Get or set the DT=ENDTIM/NPLTC overrides the DT specified in the first field.
          * - :py:attr:`~tbeg`
            - Get or set the The problem time at which to begin writing output to this time sequence.
          * - :py:attr:`~tend`
            - Get or set the The problem time at which to terminate writing output to this time sequence.
          * - :py:attr:`~domid1`
            - Get or set the Output set ID defining the domain over which variable output is
          * - :py:attr:`~domid2`
            - Get or set the Output set ID defining the domain over which variable output is
          * - :py:attr:`~domid3`
            - Get or set the Output set ID defining the domain over which variable output is
          * - :py:attr:`~domid4`
            - Get or set the Output set ID defining the domain over which variable output is
          * - :py:attr:`~domid5`
            - Get or set the Output set ID defining the domain over which variable output is
          * - :py:attr:`~domid6`
            - Get or set the Output set ID defining the domain over which variable output is
          * - :py:attr:`~domid7`
            - Get or set the Output set ID defining the domain over which variable output is
          * - :py:attr:`~domid8`
            - Get or set the Output set ID defining the domain over which variable output is
          * - :py:attr:`~global_var`
            - Get or set the The name of a global output variable computed by SOLVER_NAME.


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

    from lso_time_sequence import LsoTimeSequence

Property detail
---------------

.. py:property:: solver_name
   :type: str


   
   Get or set the Selects the solver from which data is output in this time sequence.
















   ..
       !! processed by numpydoc !!

.. py:property:: dt
   :type: float


   
   Get or set the Time interval between outputs.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcdt
   :type: int


   
   Get or set the Optional load curve ID specifying the time interval between dumps.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcopt
   :type: int


   
   Get or set the Flag to govern behavior of plot frequency load curve.
   = 1: At the time each plot is generated, the load curve value is added to the current time to determine the next plot time (this is the default behavior).
   = 2: At the time each plot is generated, the next plot time T is computed so that T = the current time plus the load curve value at the time T.
   =3: A plot is generated for each ordinate point in the load curve definition. The actual value of the load curve is ignored.
















   ..
       !! processed by numpydoc !!

.. py:property:: npltc
   :type: int


   
   Get or set the DT=ENDTIM/NPLTC overrides the DT specified in the first field.
















   ..
       !! processed by numpydoc !!

.. py:property:: tbeg
   :type: float


   
   Get or set the The problem time at which to begin writing output to this time sequence.
















   ..
       !! processed by numpydoc !!

.. py:property:: tend
   :type: float


   
   Get or set the The problem time at which to terminate writing output to this time sequence.
















   ..
       !! processed by numpydoc !!

.. py:property:: domid1
   :type: Optional[int]


   
   Get or set the Output set ID defining the domain over which variable output is
   to be performed in this time sequence. Each DOMID refers to the
   domain identified in an *LSO_DOMAIN keyword card.
















   ..
       !! processed by numpydoc !!

.. py:property:: domid2
   :type: Optional[int]


   
   Get or set the Output set ID defining the domain over which variable output is
   to be performed in this time sequence. Each DOMID refers to the
   domain identified in an *LSO_DOMAIN keyword card.
















   ..
       !! processed by numpydoc !!

.. py:property:: domid3
   :type: Optional[int]


   
   Get or set the Output set ID defining the domain over which variable output is
   to be performed in this time sequence. Each DOMID refers to the
   domain identified in an *LSO_DOMAIN keyword card.
















   ..
       !! processed by numpydoc !!

.. py:property:: domid4
   :type: Optional[int]


   
   Get or set the Output set ID defining the domain over which variable output is
   to be performed in this time sequence. Each DOMID refers to the
   domain identified in an *LSO_DOMAIN keyword card.
















   ..
       !! processed by numpydoc !!

.. py:property:: domid5
   :type: Optional[int]


   
   Get or set the Output set ID defining the domain over which variable output is
   to be performed in this time sequence. Each DOMID refers to the
   domain identified in an *LSO_DOMAIN keyword card.
















   ..
       !! processed by numpydoc !!

.. py:property:: domid6
   :type: Optional[int]


   
   Get or set the Output set ID defining the domain over which variable output is
   to be performed in this time sequence. Each DOMID refers to the
   domain identified in an *LSO_DOMAIN keyword card.
















   ..
       !! processed by numpydoc !!

.. py:property:: domid7
   :type: Optional[int]


   
   Get or set the Output set ID defining the domain over which variable output is
   to be performed in this time sequence. Each DOMID refers to the
   domain identified in an *LSO_DOMAIN keyword card.
















   ..
       !! processed by numpydoc !!

.. py:property:: domid8
   :type: Optional[int]


   
   Get or set the Output set ID defining the domain over which variable output is
   to be performed in this time sequence. Each DOMID refers to the
   domain identified in an *LSO_DOMAIN keyword card.
















   ..
       !! processed by numpydoc !!

.. py:property:: global_var
   :type: Optional[str]


   
   Get or set the The name of a global output variable computed by SOLVER_NAME.
   This variable must have a single value (scalar, vector, or
   tensor), and therefore does not depend upon any DOMID. Any
   number of such variables may be specified with a given time
   sequence. These variables are listed as having  global  domain
   for SOLVER_NAME in a separate document. This document
   (LSO_VARIABLES.TXT) is created by running the command: LSDYNA
   print_lso_doc.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'LSO'


.. py:attribute:: subkeyword
   :value: 'TIME_SEQUENCE'






