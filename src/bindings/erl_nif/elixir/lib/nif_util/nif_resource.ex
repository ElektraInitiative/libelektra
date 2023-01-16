defprotocol NifResource do
  @doc """
  Expose the NIF resource of `term`.
  """
  def nif_resource(term)
end

defimpl NifResource, for: PID do
  def nif_resource(pid) do
    GenServer.call(pid, :nif_resource)
  end
end
