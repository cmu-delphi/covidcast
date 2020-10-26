"""Custom warnings and exceptions for covidcast functions."""


class NoDataWarning(Warning):
    """Warning raised when no data is returned on a given day by covidcast.signal()."""
