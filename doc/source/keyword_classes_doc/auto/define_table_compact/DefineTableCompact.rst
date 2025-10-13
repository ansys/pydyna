





:class:`DefineTableCompact`
===========================


.. py:class:: define_table_compact.DefineTableCompact(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_TABLE_COMPACT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineTableCompact

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~tbid`
            - Get or set the Table ID. Table ID.  Tables and load curves may not share common IDs.  LS DYNA allows load curve IDs and table IDs to be used interchangeably
          * - :py:attr:`~nvar`
            - Get or set the Number of variables (dimension of the table). Current maximum is 9.
          * - :py:attr:`~lcint`
            - Get or set the Number of discretization points.
          * - :py:attr:`~mathis`
            - Get or set the Material history flag. Option to identify the abscissa variables as a specified  history variable number(s) (see Remarks 3 and 6). Additional Card 2 (and possibly Card 2.1) is read if this option is active.
          * - :py:attr:`~inexect`
            - Get or set the Extra curve settings flag. Option to assign settings about curve discretization, extrapolation and interpolation for each abscissa variable. Additional Card 3 (and possibly Card 3.1) is read if this option is active.
          * - :py:attr:`~his1`
            - Get or set the History variable numbers which indicate that variable X is HISX. For instance, setting HIS2 = 6 indicates that variable 2 is history variable 6 for the material.  A value of 0 indicates that the abscissa variable is not a history variable.
          * - :py:attr:`~his2`
            - Get or set the History variable numbers which indicate that variable X is HISX. For instance, setting HIS2 = 6 indicates that variable 2 is history variable 6 for the material.  A value of 0 indicates that the abscissa variable is not a history variable.
          * - :py:attr:`~his3`
            - Get or set the History variable numbers which indicate that variable X is HISX. For instance, setting HIS2 = 6 indicates that variable 2 is history variable 6 for the material.  A value of 0 indicates that the abscissa variable is not a history variable.
          * - :py:attr:`~his4`
            - Get or set the History variable numbers which indicate that variable X is HISX. For instance, setting HIS2 = 6 indicates that variable 2 is history variable 6 for the material.  A value of 0 indicates that the abscissa variable is not a history variable.
          * - :py:attr:`~his5`
            - Get or set the History variable numbers which indicate that variable X is HISX. For instance, setting HIS2 = 6 indicates that variable 2 is history variable 6 for the material.  A value of 0 indicates that the abscissa variable is not a history variable.
          * - :py:attr:`~his6`
            - Get or set the History variable numbers which indicate that variable X is HISX. For instance, setting HIS2 = 6 indicates that variable 2 is history variable 6 for the material.  A value of 0 indicates that the abscissa variable is not a history variable.
          * - :py:attr:`~his7`
            - Get or set the History variable numbers which indicate that variable X is HISX. For instance, setting HIS2 = 6 indicates that variable 2 is history variable 6 for the material.  A value of 0 indicates that the abscissa variable is not a history variable.
          * - :py:attr:`~his8`
            - Get or set the History variable numbers which indicate that variable X is HISX. For instance, setting HIS2 = 6 indicates that variable 2 is history variable 6 for the material.  A value of 0 indicates that the abscissa variable is not a history variable.
          * - :py:attr:`~his9`
            - Get or set the History variable numbers which indicate that variable X is HISX. For instance, setting HIS2 = 6 indicates that variable 2 is history variable 6 for the material.  A value of 0 indicates that the abscissa variable is not a history variable.
          * - :py:attr:`~ixe1`
            - Get or set the Extra settings assigned to abscissa values.
          * - :py:attr:`~ixe2`
            - Get or set the Extra settings assigned to abscissa values.
          * - :py:attr:`~ixe3`
            - Get or set the Extra settings assigned to abscissa values.
          * - :py:attr:`~ixe4`
            - Get or set the Extra settings assigned to abscissa values.
          * - :py:attr:`~ixe5`
            - Get or set the Extra settings assigned to abscissa values.
          * - :py:attr:`~ixe6`
            - Get or set the Extra settings assigned to abscissa values.
          * - :py:attr:`~ixe7`
            - Get or set the Extra settings assigned to abscissa values.
          * - :py:attr:`~ixe8`
            - Get or set the Extra settings assigned to abscissa values.
          * - :py:attr:`~ixe9`
            - Get or set the Extra settings assigned to abscissa values.
          * - :py:attr:`~o1`
            - Get or set the Ordinate (function) values..
          * - :py:attr:`~a1_1`
            - Get or set the Abscissa values of variable X.
          * - :py:attr:`~a1_2`
            - Get or set the Abscissa values of variable X.
          * - :py:attr:`~a1_3`
            - Get or set the Abscissa values of variable X.
          * - :py:attr:`~a1_4`
            - Get or set the Abscissa values of variable X.
          * - :py:attr:`~a1_5`
            - Get or set the Abscissa values of variable X.
          * - :py:attr:`~a1_6`
            - Get or set the Abscissa values of variable X.
          * - :py:attr:`~a1_7`
            - Get or set the Abscissa values of variable X.
          * - :py:attr:`~a1_8`
            - Get or set the Abscissa values of variable X.
          * - :py:attr:`~a1_9`
            - Get or set the Abscissa values of variable X.
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

    from define_table_compact import DefineTableCompact

Property detail
---------------

.. py:property:: tbid
   :type: Optional[int]


   
   Get or set the Table ID. Table ID.  Tables and load curves may not share common IDs.  LS DYNA allows load curve IDs and table IDs to be used interchangeably
















   ..
       !! processed by numpydoc !!

.. py:property:: nvar
   :type: Optional[int]


   
   Get or set the Number of variables (dimension of the table). Current maximum is 9.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcint
   :type: int


   
   Get or set the Number of discretization points.
   EQ.0:   Value of LCINT from * CONTROL_‌SOLUTION will be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: mathis
   :type: int


   
   Get or set the Material history flag. Option to identify the abscissa variables as a specified  history variable number(s) (see Remarks 3 and 6). Additional Card 2 (and possibly Card 2.1) is read if this option is active.
   EQ.0:   Off
   EQ.1 : On.
















   ..
       !! processed by numpydoc !!

.. py:property:: inexect
   :type: int


   
   Get or set the Extra curve settings flag. Option to assign settings about curve discretization, extrapolation and interpolation for each abscissa variable. Additional Card 3 (and possibly Card 3.1) is read if this option is active.
   EQ.0:   Off
   EQ.1 : On.
















   ..
       !! processed by numpydoc !!

.. py:property:: his1
   :type: Optional[int]


   
   Get or set the History variable numbers which indicate that variable X is HISX. For instance, setting HIS2 = 6 indicates that variable 2 is history variable 6 for the material.  A value of 0 indicates that the abscissa variable is not a history variable.
















   ..
       !! processed by numpydoc !!

.. py:property:: his2
   :type: Optional[int]


   
   Get or set the History variable numbers which indicate that variable X is HISX. For instance, setting HIS2 = 6 indicates that variable 2 is history variable 6 for the material.  A value of 0 indicates that the abscissa variable is not a history variable.
















   ..
       !! processed by numpydoc !!

.. py:property:: his3
   :type: Optional[int]


   
   Get or set the History variable numbers which indicate that variable X is HISX. For instance, setting HIS2 = 6 indicates that variable 2 is history variable 6 for the material.  A value of 0 indicates that the abscissa variable is not a history variable.
















   ..
       !! processed by numpydoc !!

.. py:property:: his4
   :type: Optional[int]


   
   Get or set the History variable numbers which indicate that variable X is HISX. For instance, setting HIS2 = 6 indicates that variable 2 is history variable 6 for the material.  A value of 0 indicates that the abscissa variable is not a history variable.
















   ..
       !! processed by numpydoc !!

.. py:property:: his5
   :type: Optional[int]


   
   Get or set the History variable numbers which indicate that variable X is HISX. For instance, setting HIS2 = 6 indicates that variable 2 is history variable 6 for the material.  A value of 0 indicates that the abscissa variable is not a history variable.
















   ..
       !! processed by numpydoc !!

.. py:property:: his6
   :type: Optional[int]


   
   Get or set the History variable numbers which indicate that variable X is HISX. For instance, setting HIS2 = 6 indicates that variable 2 is history variable 6 for the material.  A value of 0 indicates that the abscissa variable is not a history variable.
















   ..
       !! processed by numpydoc !!

.. py:property:: his7
   :type: Optional[int]


   
   Get or set the History variable numbers which indicate that variable X is HISX. For instance, setting HIS2 = 6 indicates that variable 2 is history variable 6 for the material.  A value of 0 indicates that the abscissa variable is not a history variable.
















   ..
       !! processed by numpydoc !!

.. py:property:: his8
   :type: Optional[int]


   
   Get or set the History variable numbers which indicate that variable X is HISX. For instance, setting HIS2 = 6 indicates that variable 2 is history variable 6 for the material.  A value of 0 indicates that the abscissa variable is not a history variable.
















   ..
       !! processed by numpydoc !!

.. py:property:: his9
   :type: Optional[int]


   
   Get or set the History variable numbers which indicate that variable X is HISX. For instance, setting HIS2 = 6 indicates that variable 2 is history variable 6 for the material.  A value of 0 indicates that the abscissa variable is not a history variable.
















   ..
       !! processed by numpydoc !!

.. py:property:: ixe1
   :type: Optional[int]


   
   Get or set the Extra settings assigned to abscissa values.
















   ..
       !! processed by numpydoc !!

.. py:property:: ixe2
   :type: Optional[int]


   
   Get or set the Extra settings assigned to abscissa values.
















   ..
       !! processed by numpydoc !!

.. py:property:: ixe3
   :type: Optional[int]


   
   Get or set the Extra settings assigned to abscissa values.
















   ..
       !! processed by numpydoc !!

.. py:property:: ixe4
   :type: Optional[int]


   
   Get or set the Extra settings assigned to abscissa values.
















   ..
       !! processed by numpydoc !!

.. py:property:: ixe5
   :type: Optional[int]


   
   Get or set the Extra settings assigned to abscissa values.
















   ..
       !! processed by numpydoc !!

.. py:property:: ixe6
   :type: Optional[int]


   
   Get or set the Extra settings assigned to abscissa values.
















   ..
       !! processed by numpydoc !!

.. py:property:: ixe7
   :type: Optional[int]


   
   Get or set the Extra settings assigned to abscissa values.
















   ..
       !! processed by numpydoc !!

.. py:property:: ixe8
   :type: Optional[int]


   
   Get or set the Extra settings assigned to abscissa values.
















   ..
       !! processed by numpydoc !!

.. py:property:: ixe9
   :type: Optional[int]


   
   Get or set the Extra settings assigned to abscissa values.
















   ..
       !! processed by numpydoc !!

.. py:property:: o1
   :type: Optional[float]


   
   Get or set the Ordinate (function) values..
















   ..
       !! processed by numpydoc !!

.. py:property:: a1_1
   :type: Optional[float]


   
   Get or set the Abscissa values of variable X.
















   ..
       !! processed by numpydoc !!

.. py:property:: a1_2
   :type: Optional[float]


   
   Get or set the Abscissa values of variable X.
















   ..
       !! processed by numpydoc !!

.. py:property:: a1_3
   :type: Optional[float]


   
   Get or set the Abscissa values of variable X.
















   ..
       !! processed by numpydoc !!

.. py:property:: a1_4
   :type: Optional[float]


   
   Get or set the Abscissa values of variable X.
















   ..
       !! processed by numpydoc !!

.. py:property:: a1_5
   :type: Optional[float]


   
   Get or set the Abscissa values of variable X.
















   ..
       !! processed by numpydoc !!

.. py:property:: a1_6
   :type: Optional[float]


   
   Get or set the Abscissa values of variable X.
















   ..
       !! processed by numpydoc !!

.. py:property:: a1_7
   :type: Optional[float]


   
   Get or set the Abscissa values of variable X.
















   ..
       !! processed by numpydoc !!

.. py:property:: a1_8
   :type: Optional[float]


   
   Get or set the Abscissa values of variable X.
















   ..
       !! processed by numpydoc !!

.. py:property:: a1_9
   :type: Optional[float]


   
   Get or set the Abscissa values of variable X.
















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
   :value: 'TABLE_COMPACT'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





