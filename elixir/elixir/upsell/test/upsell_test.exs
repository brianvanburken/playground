defmodule UpsellTest do
  use ExUnit.Case
  doctest Upsell

  test "should return V3 for P1" do
    policies = [
      %Policy{person_id: "P1", vehicle_id: "V8"},
      %Policy{person_id: "P2", vehicle_id: "V6"}
    ]

    assert Upsell.find_potential_upsells(policies) == [
             %UpsellOpportunity{person_id: "P1", vehicle_id: "V3"}
           ]
  end

  test "should fetch in batches" do
    policies =
      [
        %Policy{person_id: "P1", vehicle_id: "V8"},
        %Policy{person_id: "P2", vehicle_id: "V6"}
      ] ++
        for n <- 3..103, do: %Policy{person_id: "P#{n}", vehicle_id: "V#{n}"}

    assert Upsell.find_potential_upsells(policies) == [
             %UpsellOpportunity{person_id: "P1", vehicle_id: "V3"}
           ]
  end
end
