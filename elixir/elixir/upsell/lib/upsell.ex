defmodule Upsell do
  def get_owned_vehicles(person_ids) do
    if Enum.count(person_ids) >= 100 do
      raise ArgumentError, "too much"
    end

    [
      %OwnedVehicle{person_id: "P1", vehicle_id: "V8"},
      %OwnedVehicle{person_id: "P1", vehicle_id: "V3"},
      %OwnedVehicle{person_id: "P2", vehicle_id: "V6"}
    ]
  end

  def find_potential_upsells(policies) do
    person_ids =
      policies
      |> Enum.map(fn policy -> Map.get(policy, :person_id) end)

    vehicle_ids = MapSet.new(policies, fn p -> p.vehicle_id end)

    person_ids
    |> Enum.chunk_every(99)
    |> Enum.flat_map(fn chunk ->
      chunk
      |> get_owned_vehicles()
      |> Enum.filter(fn item ->
        not MapSet.member?(vehicle_ids, item.vehicle_id)
      end)
      |> Enum.map(fn v -> %UpsellOpportunity{person_id: v.person_id, vehicle_id: v.vehicle_id} end)
    end)
  end
end
