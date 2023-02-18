defmodule UpsellOpportunity do
  @enforce_keys [:person_id, :vehicle_id]
  defstruct [:person_id, :vehicle_id]
end
