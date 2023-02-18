# Find potential up-sells

As an insurance company, we try to sell as many insurance policies as possible.

A way to sell more policies is "upselling" our current customers: offering them additional products that we know they may be interested in, based on the data we have at our disposal.

Imagine we recently obtained access to an external datasource that we can use to query all the vehicles owned by a particular person. This allows us to find which of our current customers have additional vehicle(s) that are not insured with us. These are great upsell opportunities!

Your objective is, given a list of some of our current policies (and therefore current customers), to make use of this external API in the most efficient way possible in order to find such upselling opportunities

There is a service available that takes a list of person ids, and returns a list of all the vehicles owned by anyone in that list. You can supply up to 100 person ids per call
Keep in mind that this function wraps an external API, so calls to it are costly and relatively slow

## Pseudocode

```elixir
# For the sake of the exercise, the types have the same shape.
# (Feel free to take advantage of that).
# If you want, you could also just use a tuple of (person id, vehicle id)
# In reality, these types would have many more fields and differ from each other.

record OwnedVehicle
  person id: String
  vehicle id: String

record Policy
  person id: String
  vehicle id: String

record UpsellOpportunity
  person id: String
  vehicle id: String

# This is the function vou can call to get the vehicles owned by someone
# It supports batching, and keep in mind it wraps an external API, so it
# is costly and slow..

function get_owned_vehicles(person_ids: List<String>): List<OwnedVehicle>
# "Real" third party source behind this function

# This is the function you have to implement,
# It should return a list of up-selling targets, where each target is
# another vehicle owned by a customer.
function find_potential_upsells(policies: List<Policy>): List<UpsellOpportunity>
# Your implementation goes here
```

## Example

Assume you are given as input the following policies:

```json
[
  {"person id": "P1", "vehicle id": "V8"},
  {"person id": "P2", "vehicle id": "V6"}
]
```

And assume that P1, in addition to the vehicle V8, also owns a vehicle V3; while P2 only owns the vehicle V6.
(You can obtain this info by calling `get_owned_ vehicles`)

In this case, since the only upsell opportunity is to offer a policy to our customer P1 for their other vehicle V3, this is what your function should return:

```elixir
> find potential upsells(policies)
[{"person id": "P4", "vehicle id": "V3"}]
```
