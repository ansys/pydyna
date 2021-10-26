"""Module level description.

Module level summary line should be a single line.  Extended summary
can span multiple lines and include examples.

Examples
--------
>>> from ansys.product import library
>>> library.add(1, 2)
3

"""


def add(arg1, arg2):
    """Summary line <should be one one line>.

    Extended description of function.  Can span multiple lines and
    provides a general overview of the function.

    .. warning::
       Use the ``.. warning::`` directive within the doc-string for
       any warnings you would like to explicitly state.  For example,
       if this method will be deprecated in the next release.

    Parameters
    ----------
    arg1 : int
        Description of arg1.
    arg2 : str
        Description of arg2.  Note how there is no space between the
        previous parameter definition.  Also note that lines are being
        wrapped at 72 characters.

    Returns
    -------
    int
        Description of return value.  This description ends with a
        sentence, and we also don't name the return value.

    Examples
    --------
    >>> from ansys.product.library import add
    >>> add(1, 2)
    3

    See Also
    --------
    :class:`ExampleClass <ansys.product.library.ExampleClass>`

    """
    return arg1 + arg2
