use std::collections::HashSet;

#[derive(Debug)]
struct OwnedVehicle {
    person_id: String,
    vehicle_id: String,
}

struct Policy {
    person_id: String,
    vehicle_id: String,
}

#[derive(PartialEq, Debug)]
struct UpsellOpportunity {
    person_id: String,
    vehicle_id: String,
}

fn get_owned_vehicles(_person_ids: Vec<String>) -> Vec<OwnedVehicle> {
    vec![
        OwnedVehicle {
            person_id: "P1".to_string(),
            vehicle_id: "V8".to_string(),
        },
        OwnedVehicle {
            person_id: "P1".to_string(),
            vehicle_id: "V3".to_string(),
        },
        OwnedVehicle {
            person_id: "P2".to_string(),
            vehicle_id: "V6".to_string(),
        },
    ]
}

fn find_potential_upsells(policies: Vec<Policy>) -> Vec<UpsellOpportunity> {
    let mut person_ids = Vec::new();
    let mut vehicle_ids = HashSet::new();

    for policy in policies.iter() {
        person_ids.push(policy.person_id.clone());
        vehicle_ids.insert(policy.vehicle_id.clone());
    }

    return get_owned_vehicles(person_ids)
        .iter()
        .filter(|v| !vehicle_ids.contains(&v.vehicle_id))
        .map(|v| UpsellOpportunity {
            person_id: v.person_id.clone(),
            vehicle_id: v.vehicle_id.clone(),
        })
        .collect();
}

mod test {
    use super::*;

    #[test]
    fn test_return_upsell() {
        let policies = vec![
            Policy {
                person_id: "P1".to_string(),
                vehicle_id: "V8".to_string(),
            },
            Policy {
                person_id: "P2".to_string(),
                vehicle_id: "V6".to_string(),
            },
        ];

        let upsells = find_potential_upsells(policies);
        assert!(upsells.len() > 0);
        let actual = upsells.iter().next().unwrap();

        let expected = UpsellOpportunity {
            person_id: "P1".to_string(),
            vehicle_id: "V3".to_string(),
        };

        assert!(*actual == expected);
    }
}
