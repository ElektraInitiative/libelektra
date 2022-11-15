defmodule NifUtil do
  @doc"""
  Convert `term` to an atom if it is `nil`.
  """
  @spec nullify(term()) :: :null | term()
  def nullify(term)
  def nullify(nil) do
    :null
  end
  def nullify(term) do
    term
  end
end
