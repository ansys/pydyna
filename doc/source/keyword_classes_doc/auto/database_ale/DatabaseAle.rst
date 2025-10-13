





:class:`DatabaseAle`
====================


.. py:class:: database_ale.DatabaseAle(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DATABASE_ALE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DatabaseAle

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~dtout`
            - Get or set the Time interval between the outputs.
          * - :py:attr:`~setid`
            - Get or set the ALE element set ID.
          * - :py:attr:`~var1`
            - Get or set the Variable rank in the following list:
          * - :py:attr:`~var2`
            - Get or set the Variable rank in the following list:
          * - :py:attr:`~var3`
            - Get or set the Variable rank in the following list:
          * - :py:attr:`~var4`
            - Get or set the Variable rank in the following list:
          * - :py:attr:`~var5`
            - Get or set the Variable rank in the following list:
          * - :py:attr:`~var6`
            - Get or set the Variable rank in the following list:
          * - :py:attr:`~var7`
            - Get or set the Variable rank in the following list:
          * - :py:attr:`~var8`
            - Get or set the Variable rank in the following list:


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

    from database_ale import DatabaseAle

Property detail
---------------

.. py:property:: dtout
   :type: Optional[float]


   
   Get or set the Time interval between the outputs.
















   ..
       !! processed by numpydoc !!

.. py:property:: setid
   :type: Optional[int]


   
   Get or set the ALE element set ID.
   If the model is 1D (*SECTION_ALE1D), the set should be *SET_BEAM
   If the model is 2D (*SECTION_ALE2D), the set should be *SET_SHELL
   If the model is 3D (*SECTION_SOLID), the set should be *SET_SOLID.
















   ..
       !! processed by numpydoc !!

.. py:property:: var1
   :type: Optional[int]


   
   Get or set the Variable rank in the following list:
   LT.0:   |VAR| : rank of the auxiliary variable to be replaced in d3plot (see remark 3)
   EQ.1: xx-stress
   EQ.2: yy-stress
   EQ.3: zz-stress
   EQ.4: xy-stress
   EQ.5: yz-stress
   EQ.6: zx-stress
   EQ.7: plastic strain
   EQ.8: internal energy
   EQ.9: bulk viscosity
   EQ.10: previous volume
   EQ.11: pressure
   EQ.12: mass
   EQ.13: volume
   EQ.14: density
   EQ.15:  kinetic energy
   EQ.16: The 6 stresses are added to the database.
   EQ.17:  Impulse (pressure integrated over time)
   If there is a blank column between 2 variable ranks, the list between these 2 ranks is selected.For example, if the card is as follows :
   1, ,6
   The 6 stresses are added to the database.
















   ..
       !! processed by numpydoc !!

.. py:property:: var2
   :type: Optional[int]


   
   Get or set the Variable rank in the following list:
   LT.0:   |VAR| : rank of the auxiliary variable to be replaced in d3plot (see remark 3)
   EQ.1: xx-stress
   EQ.2: yy-stress
   EQ.3: zz-stress
   EQ.4: xy-stress
   EQ.5: yz-stress
   EQ.6: zx-stress
   EQ.7: plastic strain
   EQ.8: internal energy
   EQ.9: bulk viscosity
   EQ.10: previous volume
   EQ.11: pressure
   EQ.12: mass
   EQ.13: volume
   EQ.14: density
   EQ.15:  kinetic energy
   EQ.16: The 6 stresses are added to the database.
   EQ.17:  Impulse (pressure integrated over time)
   If there is a blank column between 2 variable ranks, the list between these 2 ranks is selected.For example, if the card is as follows :
   1, ,6
   The 6 stresses are added to the database.
















   ..
       !! processed by numpydoc !!

.. py:property:: var3
   :type: Optional[int]


   
   Get or set the Variable rank in the following list:
   LT.0:   |VAR| : rank of the auxiliary variable to be replaced in d3plot (see remark 3)
   EQ.1: xx-stress
   EQ.2: yy-stress
   EQ.3: zz-stress
   EQ.4: xy-stress
   EQ.5: yz-stress
   EQ.6: zx-stress
   EQ.7: plastic strain
   EQ.8: internal energy
   EQ.9: bulk viscosity
   EQ.10: previous volume
   EQ.11: pressure
   EQ.12: mass
   EQ.13: volume
   EQ.14: density
   EQ.15:  kinetic energy
   EQ.16: The 6 stresses are added to the database.
   EQ.17:  Impulse (pressure integrated over time)
   If there is a blank column between 2 variable ranks, the list between these 2 ranks is selected.For example, if the card is as follows :
   1, ,6
   The 6 stresses are added to the database.
















   ..
       !! processed by numpydoc !!

.. py:property:: var4
   :type: Optional[int]


   
   Get or set the Variable rank in the following list:
   LT.0:   |VAR| : rank of the auxiliary variable to be replaced in d3plot (see remark 3)
   EQ.1: xx-stress
   EQ.2: yy-stress
   EQ.3: zz-stress
   EQ.4: xy-stress
   EQ.5: yz-stress
   EQ.6: zx-stress
   EQ.7: plastic strain
   EQ.8: internal energy
   EQ.9: bulk viscosity
   EQ.10: previous volume
   EQ.11: pressure
   EQ.12: mass
   EQ.13: volume
   EQ.14: density
   EQ.15:  kinetic energy
   EQ.16: The 6 stresses are added to the database.
   EQ.17:  Impulse (pressure integrated over time)
   If there is a blank column between 2 variable ranks, the list between these 2 ranks is selected.For example, if the card is as follows :
   1, ,6
   The 6 stresses are added to the database.
















   ..
       !! processed by numpydoc !!

.. py:property:: var5
   :type: Optional[int]


   
   Get or set the Variable rank in the following list:
   LT.0:   |VAR| : rank of the auxiliary variable to be replaced in d3plot (see remark 3)
   EQ.1: xx-stress
   EQ.2: yy-stress
   EQ.3: zz-stress
   EQ.4: xy-stress
   EQ.5: yz-stress
   EQ.6: zx-stress
   EQ.7: plastic strain
   EQ.8: internal energy
   EQ.9: bulk viscosity
   EQ.10: previous volume
   EQ.11: pressure
   EQ.12: mass
   EQ.13: volume
   EQ.14: density
   EQ.15:  kinetic energy
   EQ.16: The 6 stresses are added to the database.
   EQ.17:  Impulse (pressure integrated over time)
   If there is a blank column between 2 variable ranks, the list between these 2 ranks is selected.For example, if the card is as follows :
   1, ,6
   The 6 stresses are added to the database.
















   ..
       !! processed by numpydoc !!

.. py:property:: var6
   :type: Optional[int]


   
   Get or set the Variable rank in the following list:
   LT.0:   |VAR| : rank of the auxiliary variable to be replaced in d3plot (see remark 3)
   EQ.1: xx-stress
   EQ.2: yy-stress
   EQ.3: zz-stress
   EQ.4: xy-stress
   EQ.5: yz-stress
   EQ.6: zx-stress
   EQ.7: plastic strain
   EQ.8: internal energy
   EQ.9: bulk viscosity
   EQ.10: previous volume
   EQ.11: pressure
   EQ.12: mass
   EQ.13: volume
   EQ.14: density
   EQ.15:  kinetic energy
   EQ.16: The 6 stresses are added to the database.
   EQ.17:  Impulse (pressure integrated over time)
   If there is a blank column between 2 variable ranks, the list between these 2 ranks is selected.For example, if the card is as follows :
   1, ,6
   The 6 stresses are added to the database.
















   ..
       !! processed by numpydoc !!

.. py:property:: var7
   :type: Optional[int]


   
   Get or set the Variable rank in the following list:
   LT.0:   |VAR| : rank of the auxiliary variable to be replaced in d3plot (see remark 3)
   EQ.1: xx-stress
   EQ.2: yy-stress
   EQ.3: zz-stress
   EQ.4: xy-stress
   EQ.5: yz-stress
   EQ.6: zx-stress
   EQ.7: plastic strain
   EQ.8: internal energy
   EQ.9: bulk viscosity
   EQ.10: previous volume
   EQ.11: pressure
   EQ.12: mass
   EQ.13: volume
   EQ.14: density
   EQ.15:  kinetic energy
   EQ.16: The 6 stresses are added to the database.
   EQ.17:  Impulse (pressure integrated over time)
   If there is a blank column between 2 variable ranks, the list between these 2 ranks is selected.For example, if the card is as follows :
   1, ,6
   The 6 stresses are added to the database.
















   ..
       !! processed by numpydoc !!

.. py:property:: var8
   :type: Optional[int]


   
   Get or set the Variable rank in the following list:
   LT.0:   |VAR| : rank of the auxiliary variable to be replaced in d3plot (see remark 3)
   EQ.1: xx-stress
   EQ.2: yy-stress
   EQ.3: zz-stress
   EQ.4: xy-stress
   EQ.5: yz-stress
   EQ.6: zx-stress
   EQ.7: plastic strain
   EQ.8: internal energy
   EQ.9: bulk viscosity
   EQ.10: previous volume
   EQ.11: pressure
   EQ.12: mass
   EQ.13: volume
   EQ.14: density
   EQ.15:  kinetic energy
   EQ.16: The 6 stresses are added to the database.
   EQ.17:  Impulse (pressure integrated over time)
   If there is a blank column between 2 variable ranks, the list between these 2 ranks is selected.For example, if the card is as follows :
   1, ,6
   The 6 stresses are added to the database.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DATABASE'


.. py:attribute:: subkeyword
   :value: 'ALE'






