defmodule NifUtil do
  @doc """
  Prepare `term` to be passed to a NIF.
  """
  @spec unwrap(term()) :: :null | term()
  def unwrap(term)

  def unwrap(nil) do
    :null
  end

  def unwrap(term) do
    NifResource.nif_resource(term)
  end
end
