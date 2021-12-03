import { NatuurlijkPersoonModel } from "./NatuurlijkPersoonModel";

interface NatuurlijkPersoonListItemProps {
  persoon: NatuurlijkPersoonModel;
  deletePersoon: (_: NatuurlijkPersoonModel) => void;
  editPersoon: (_: NatuurlijkPersoonModel) => void;
}

export default function NatuurlijkPersoonListItem({
  persoon,
  deletePersoon,
  editPersoon,
}: NatuurlijkPersoonListItemProps) {
  return (
    <div>
      <div>{getPersoonnaam(persoon)}</div>
      <div>{persoon.woonplaats}</div>
      <button onClick={() => editPersoon(persoon)}>Wijzigen</button>
      <button onClick={() => deletePersoon(persoon)}>Verwijderen</button>
    </div>
  );
}

function getPersoonnaam(persoon: NatuurlijkPersoonModel): string {
  return `${persoon.voornamen}${
    persoon.voorvoegselGeslachtsnaam
      ? " " + persoon.voorvoegselGeslachtsnaam
      : ""
  } ${persoon.geslachtsnaam}`;
}
